const std = @import("std");
const BitSet = std.bit_set.IntegerBitSet(256);
const Allocator = std.mem.Allocator;

const Regex = union(enum) {
    const Self = @This();
    const RE = *const @This();

    _literal: []const u8,

    _repetition: struct {
        re: RE,
        min: usize,
        max: ?usize = null,
        greedy: bool,
    },

    _alt: []const RE,

    _concat: []const RE,

    _capture: RE,

    _backref: usize,

    start,
    end,
    boundary,

    any,
    word,
    digit,
    alphabet,
    alphanum,
    whitespace,

    @"!boundary",
    @"!word",
    @"!digit",
    @"!alphabet",
    @"!alphanum",
    @"!whitespace",

    _charset: BitSet,

    pub const Match = struct {
        // position within the source string
        pos: usize,
        len: usize,

        pub fn string(self: @This(), source: []const u8) []const u8 {
            const i = self.pos;
            return source[i .. i + self.len];
        }
    };

    const SearchState = struct {
        capture: ?*const Capture,
        input: *const Input,

        fn getCapture(self: @This(), index: usize) ?*const Capture {
            var current = self.capture;
            while (true) {
                if (current) |cap| {
                    if (cap.index == index) return cap;
                    current = cap.parent;
                } else return null;
            }
        }

        fn withCapture(self: @This(), cap: *const Capture) SearchState {
            return .{
                .capture = cap,
                .input = self.input,
            };
        }

        fn withInput(self: @This(), input: *const Input) SearchState {
            return .{
                .capture = self.capture,
                .input = input,
            };
        }

        fn numCaptures(self: @This()) usize {
            if (self.capture) |c| return c.index + 1;
            return 0;
        }
    };

    const Capture = struct {
        value: []const u8,
        index: usize,
        parent: ?*const Capture,
    };

    const Input = struct {
        value: []const u8,
        pos: usize,

        inline fn size(self: @This()) usize {
            return self.value.len - self.pos;
        }

        inline fn string(self: @This()) []const u8 {
            return self.value[self.pos..];
        }

        fn prev(self: @This()) ?u8 {
            if (self.pos > 0 and self.value.len > 0)
                return self.value[self.pos - 1];
            return null;
        }
        fn current(self: @This()) ?u8 {
            const len = self.value.len;
            if (self.pos >= 0 and self.pos < len and len > 0)
                return self.value[self.pos];
            return null;
        }

        fn slice(self: @This(), offset: usize) Input {
            return .{
                .value = self.value,
                .pos = self.pos + offset,
            };
        }
    };

    const CharClass = struct {
        const any = BitSet.initFull();
        const digit = fromString("0123456789");
        const alphabet = fromString("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
        const underscore = fromString("_");
        const whitespace = fromString(" \t\n\r\x0C\x0B\x85");
        const alphanum = digit.unionWith(alphabet);
        const word = alphanum.unionWith(underscore);

        fn fromString(str: []const u8) BitSet {
            var set: BitSet = .initEmpty();
            for (str) |c| set.set(c);
            return set;
        }
    };

    fn search(self: @This(), allocator: Allocator, input: []const u8) Allocator.Error!?Match {
        var state: SearchState = .{
            .captures = .{},
        };
        return self.match(allocator, state);
    }

    // TODO:
    // const match_iter = re.searchAll();
    // defer match_iter.deinit();

    fn handleConcat(
        allocator: Allocator,
        state_arg: SearchState,
        args: []const RE,
    ) !?Match {
        if (args.len == 0) return .{ .pos = state_arg.input.pos, .len = 0 };

        var i: usize = 0;
        var state = state_arg;
        var result: Match = .{ .pos = state.input.pos, .len = 0 };

        loop: while (i < args.len) : (i += 1) {
            const re = args[i];
            switch (re.*) {
                ._repetition, ._capture => break :loop,
                else => {
                    const m = try re.match(allocator, state) orelse return null;
                    result.len += m.len;
                    state = state.withInput(&state.input.slice(m.len));
                },
            }
        }

        if (i >= args.len) {
            return result;
        }

        const rest_args = args[i + 1 ..];
        switch (args[i].*) {
            ._repetition => {
                var iterator: Iterator = .init(args[i], state);
                defer iterator.deinit(allocator);

                while (try iterator.next(allocator)) |m| {
                    if (rest_args.len == 0) return .{
                        .pos = result.pos,
                        .len = result.len + m.len,
                    };

                    const sub_state = state.withInput(&state.input.slice(m.len));
                    if (try handleConcat(allocator, sub_state, rest_args)) |m2| {
                        return .{
                            .pos = result.pos,
                            .len = result.len + m.len + m2.len,
                        };
                    }
                }
            },
            ._capture => |re| {
                const m = try re.match(allocator, state) orelse return null;
                const sub_state: SearchState = .{
                    .capture = &.{
                        .value = m.string(state.input.value),
                        .index = if (state.capture) |c| c.index + 1 else 0,
                        .parent = state.capture,
                    },
                    .input = &state.input.slice(m.len),
                };

                if (try handleConcat(allocator, sub_state, rest_args)) |m2| {
                    return .{
                        .pos = result.pos,
                        .len = result.len + m.len + m2.len,
                    };
                }
            },
            else => unreachable,
        }

        return null;
    }

    // TODO: remove
    fn handleConcatIterative(
        allocator: Allocator,
        input: []const u8,
        state: *SearchState,
        args: []const RE,
    ) !?Match {
        const size = state.capture.items.len;
        defer state.capture.shrinkRetainingCapacity(size);

        if (args.len == 0) return null;

        var i: usize = 0;
        var step_back = false;
        var result: Match = .{ .pos = input.pos, .len = 0 };
        var snapshot: std.ArrayListUnmanaged(struct {
            match: Match,
            iterator: Iterator,
            arg_index: usize,
            capture_size: usize,
        }) = .{};

        defer {
            for (snapshot.items) |*elem| {
                elem.iterator.deinit(allocator);
            }
            snapshot.deinit(allocator);
        }

        while (true) {
            const re, const capture_result = switch (args[i].*) {
                ._capture => |val| .{ val, true },
                else => .{ args[i], false },
            };

            var iterator, const str = blk: switch (step_back) {
                true => {
                    const s = snapshot.pop() orelse break;
                    const capture_size = s.capture_size;
                    state.capture.shrinkRetainingCapacity(capture_size);

                    i = s.arg_index;
                    result = s.match;
                    step_back = false;

                    const str = input[result.pos + result.len ..];
                    break :blk .{ s.iterator, str };
                },
                false => {
                    const str = input[result.pos + result.len ..];
                    const iterator: Iterator = .init(re, str, state);
                    break :blk .{ iterator, str };
                },
            };
            errdefer iterator.deinit(allocator);

            const m = try iterator.next(allocator) orelse {
                iterator.deinit(allocator);
                step_back = true;
                continue;
            };

            switch (iterator) {
                .single => {},
                else => try snapshot.append(allocator, .{
                    .match = result,
                    .iterator = iterator,
                    .arg_index = i,
                    .capture_size = state.capture.items.len,
                }),
            }

            result.len += m.len;
            if (i == 0) result.pos = m.pos;
            if (i >= args.len - 1) return result;
            if (capture_result) try state.capture.append(allocator, m.value(str));
            i += 1;
        }

        return null;
    }

    fn match(self: Self, allocator: Allocator, state: SearchState) Allocator.Error!?Match {
        const input_arg = state.input;
        const match_one: Match = .{ .pos = input_arg.pos, .len = 1 };
        const match_zero: Match = .{ .pos = input_arg.pos, .len = 0 };

        return switch (self) {
            .boundary => {
                const prev = input_arg.prev() orelse return match_zero;
                const next = input_arg.current() orelse return match_zero;

                const is_prev_word = CharClass.word.isSet(prev);
                const is_next_word = CharClass.word.isSet(next);
                const matched =
                    is_prev_word and !is_next_word or
                    !is_prev_word and is_next_word;

                return if (matched) match_zero else null;
            },

            .start => return if (input_arg.pos == 0) match_zero else null,
            .end => {
                const len = input_arg.size();
                return if (len == 0 or input_arg.pos == len - 1) match_zero else null;
            },

            ._literal => |val| {
                const input = input_arg.string();
                if (std.mem.eql(u8, val, input[0..@min(val.len, input.len)]))
                    return .{ .pos = input_arg.pos, .len = val.len }
                else
                    return null;
            },

            // TODO: needs backtracking too, add iterator
            ._alt => |args| {
                for (args) |re| {
                    if (try re.match(allocator, state)) |m| {
                        return m;
                    }
                }
                return null;
            },

            ._backref => |index| {
                if (index >= 0 and index < state.numCaptures()) {
                    const input = input_arg.string();
                    const item = (state.getCapture(index) orelse return null).value;
                    if (item.len > 0 and std.mem.eql(u8, item, input[0..@min(item.len, input.len)])) {
                        return .{ .pos = input_arg.pos, .len = item.len };
                    }
                }
                return null;
            },

            ._capture => |re| {
                // Captures are only done under concats.
                // If it got here it means it's outside of concat,
                // so just match normally without capturing.
                return try re.match(allocator, state);
            },

            ._repetition => handleConcat(allocator, state, &.{&self}),

            ._concat => |args| handleConcat(allocator, state, args),

            inline .any,
            .word,
            .digit,
            .alphabet,
            .alphanum,
            .whitespace,
            .@"!word",
            .@"!digit",
            .@"!alphabet",
            .@"!alphanum",
            .@"!whitespace",
            ._charset,
            => |val, tag| {
                if (input_arg.size() == 0) return null;

                const input = input_arg.string();
                const ch = input[0];
                return switch (tag) {
                    .any => match_one,

                    .word => if (CharClass.word.isSet(ch)) match_one else null,
                    .digit => if (CharClass.digit.isSet(ch)) match_one else null,
                    .alphabet => if (CharClass.alphabet.isSet(ch)) match_one else null,
                    .alphanum => if (CharClass.alphanum.isSet(ch)) match_one else null,
                    .whitespace => if (CharClass.whitespace.isSet(ch)) match_one else null,

                    .@"!word" => if (!CharClass.word.isSet(ch)) match_one else null,
                    .@"!digit" => if (!CharClass.digit.isSet(ch)) match_one else null,
                    .@"!alphabet" => if (!CharClass.alphabet.isSet(ch)) match_one else null,
                    .@"!alphanum" => if (!CharClass.alphanum.isSet(ch)) match_one else null,
                    .@"!whitespace" => if (!CharClass.whitespace.isSet(ch)) match_one else null,
                    ._charset => if (val.isSet(ch)) match_one else null,

                    else => comptime unreachable,
                };
            },

            else => {
                std.debug.print("TODO: {s}\n", .{@tagName(self)});
                unreachable;
            },
        };
    }

    fn replaceAll(
        self: @This(),
        input: std.io.AnyWriter,
        output: std.io.AnyWriter,
    ) void {
        _ = self;
        _ = input;
        _ = output;
    }

    // Note: constructor functions like zero_or_more
    // takes a pointer and return a value to avoid
    // heap allocations.
    // Ideally they could take a value parameter,
    // but Regex is self-referential, so they need
    // to be pointers.

    fn literal(value: []const u8) Self {
        return .{ ._literal = value };
    }

    fn anyChar(value: []const u8) Self {
        var set: BitSet = .initEmpty();
        for (value) |c| set.set(c);
        return .{ ._charset = set };
    }
    fn @"!anyChar"(value: []const u8) Self {
        var set: BitSet = .initEmpty();
        for (value) |c| set.set(c);
        return .{ ._charset = set.complement() };
    }

    fn range(start: u8, end: u8) Self {
        var set: BitSet = .initEmpty();
        for (start..end) |c| set.set(c);
        return .{ ._charset = set };
    }
    fn @"!range"(start: u8, end: u8) Self {
        var set: BitSet = .initEmpty();
        for (start..end) |c| set.set(c);
        return .{ ._charset = set.complement() };
    }

    fn either(args: []const RE) Self {
        return .{ ._alt = args };
    }

    fn optional(re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = false,
            },
        };
    }

    fn @"zeroOrMore?"(re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = false,
                .min = 0,
            },
        };
    }

    fn zeroOrMore(re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = true,
                .min = 0,
            },
        };
    }

    fn oneOrMore(re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = true,
                .min = 1,
            },
        };
    }

    fn @"oneOrMore?"(re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = false,
                .min = 1,
            },
        };
    }

    fn atLeast(count: usize, re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = true,
                .min = count,
            },
        };
    }

    fn @"atLeast?"(count: usize, re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = false,
                .min = count,
            },
        };
    }

    fn atMost(count: usize, re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = true,
                .min = 0,
                .max = count,
            },
        };
    }

    fn @"atMost?"(count: usize, re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = false,
                .min = 0,
                .max = count,
            },
        };
    }

    fn around(min: usize, max: usize, re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = true,
                .min = min,
                .max = max,
            },
        };
    }

    fn @"around?"(min: usize, max: usize, re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = false,
                .min = min,
                .max = max,
            },
        };
    }

    fn times(count: usize, re: RE) Self {
        return .{
            ._repetition = .{
                .re = re,
                .greedy = true,
                .min = count,
                .max = count,
            },
        };
    }

    inline fn @"*"(re: RE) Self {
        return zeroOrMore(re);
    }
    inline fn @"+"(re: RE) Self {
        return @"oneOrMore?"(re);
    }
    inline fn @"*?"(re: RE) Self {
        return @"zeroOrMore?"(re);
    }
    inline fn @"+?"(re: RE) Self {
        return oneOrMore(re);
    }

    fn backref(index: usize) Self {
        return .{ ._backref = index };
    }

    fn concat(contents: []const RE) Self {
        return .{ ._concat = contents };
    }

    fn capture(re: RE) Self {
        return .{ ._capture = re };
    }
};

const LazyIterator = struct {
    re: Regex.RE,
    state: Regex.SearchState,
    pos: usize = 0,
    len: usize = 0,
    iterations: usize = 0,
    min: usize,
    max: ?usize,
    first: bool = true,

    const Self = @This();

    fn deinit(_: *Self, _: Allocator) void {}

    fn next(self: *Self, allocator: Allocator) !?Regex.Match {
        const state = self.state;
        var input = state.input;

        if (self.first) {
            self.first = false;

            // skip first few matches until min
            while (self.iterations < self.min) {
                if (try self.re.match(allocator, state)) |m| {
                    self.pos = m.pos;
                    self.len = m.len;
                    self.iterations += 1;
                } else return null;
            }

            return .{ .pos = self.pos, .len = self.len };
        }

        const max = self.max orelse std.math.maxInt(usize);

        if (self.iterations <= max) {
            const sub_state = state.withInput(&input.slice(self.len));
            if (try self.re.match(allocator, sub_state)) |m| {
                self.len += m.len;
                self.iterations += 1;
                return .{ .pos = self.pos, .len = self.len };
            }
        }

        // match zero string
        if (self.iterations <= max + 1) {
            self.iterations += 1;
            self.pos = input.pos;
            self.len = 0;
            return .{ .pos = self.pos, .len = self.len };
        }

        return null;
    }
};

const GreedyIterator = struct {
    re: Regex.RE,
    state: Regex.SearchState,
    pos: usize = 0,
    len: usize = 0,
    min: usize,
    max: ?usize = null,
    first: bool = true,
    lengths: std.ArrayListUnmanaged(usize) = .{},

    const Self = @This();

    fn deinit(self: *Self, allocator: Allocator) void {
        self.lengths.deinit(allocator);
    }

    fn next(self: *Self, allocator: Allocator) !?Regex.Match {
        const state = self.state;

        if (self.first) {
            self.first = false;

            var n: usize = 0;
            var input = self.state.input;
            var lengths = &self.lengths;

            // skip first few matches until min
            while (n < self.min) {
                if (try self.re.match(allocator, state)) |m| {
                    self.pos = m.pos;
                    n += 1;
                } else return null;
            }

            if (n == 0) try lengths.append(allocator, 0);

            while (true) {
                if (self.max) |max| {
                    if (lengths.items.len >= max) break;
                }
                if (try self.re.match(allocator, state.withInput(&input.slice(self.len)))) |m| {
                    self.len += m.len;
                    if (lengths.items.len == 0) self.pos = m.pos;
                    try lengths.append(allocator, self.len);
                } else break;
            }
        }

        if (self.lengths.pop()) |len| {
            self.len = len;
            return .{ .pos = self.pos, .len = self.len };
        }

        return null;
    }
};

const SingleIterator = struct {
    re: Regex.RE,
    state: Regex.SearchState,
    pos: usize = 0,
    len: usize = 0,
    first: bool = true,

    const Self = @This();

    fn deinit(_: *Self, _: Allocator) void {}

    fn next(self: *Self, allocator: Allocator) !?Regex.Match {
        if (self.first) {
            self.first = false;
            const result = try self.re.match(allocator, self.state);
            if (result) |m| {
                self.pos = m.pos;
                self.len = m.len;
            }
            return result;
        }
        return null;
    }
};

const Iterator = union(enum) {
    greedy: GreedyIterator,
    lazy: LazyIterator,
    single: SingleIterator,

    const Self = @This();

    fn init(
        re: Regex.RE,
        state: Regex.SearchState,
    ) Self {
        return switch (re.*) {
            ._repetition => |val| if (val.greedy) .{ .greedy = GreedyIterator{
                .re = val.re,
                .state = state,
                .min = val.min,
                .max = val.max,
            } } else .{ .lazy = LazyIterator{
                .re = val.re,
                .state = state,
                .min = val.min,
                .max = val.max,
            } },
            else => .{ .single = .{
                .re = re,
                .state = state,
            } },
        };
    }

    fn deinit(self: *Iterator, allocator: Allocator) void {
        return switch (self.*) {
            inline else => |*iter| iter.deinit(allocator),
        };
    }

    fn next(self: *Iterator, allocator: Allocator) !?Regex.Match {
        return switch (self.*) {
            inline else => |*iter| try iter.next(allocator),
        };
    }
};

test {
    const TestItem = struct {
        re: Regex.RE,
        input: []const u8,
        expected: ?[]const u8,
    };

    // TODO: once I have more tests running,
    // I could try refactoring it using a one big nested switch-loop.
    // Or not, this is just a toy regex implementation anyway.
    // I have other things to do...
    // I think the kleen star shit and backtracking is the most
    // complicated part of the regex engine, so at least
    // I've scratched my itch already.
    const tests: []const TestItem = &.{
        .{ .re = &.literal("a"), .input = "b", .expected = null },
        .{ .re = &.literal("b"), .input = "a", .expected = null },
        .{
            .re = &.literal("a"),
            .input = "ab",
            .expected = "a",
        },
        .{
            .re = &.concat(&.{
                &.literal("a"),
                &.literal("a"),
                &.literal("a"),
            }),
            .input = "aaa",
            .expected = "aaa",
        },
        .{
            .re = &.concat(&.{
                &.literal("a"),
                &.literal("a"),
                &.literal("a"),
            }),
            .input = "aab",
            .expected = null,
        },
        .{
            .re = &.concat(&.{
                &.literal("a1"),
                &.literal("a2"),
                &.literal("a34"),
            }),
            .input = "a1a2a345678",
            .expected = "a1a2a34",
        },
        .{
            .re = &.concat(&.{
                &.literal("a"),
                &.literal("a"),
            }),
            .input = "aab",
            .expected = "aa",
        },
        .{
            .re = &.zeroOrMore(&.literal("a")),
            .input = "qwersdfz",
            .expected = "",
        },
        .{
            .re = &.zeroOrMore(&.literal("a")),
            .input = "aaaqwersdfz",
            .expected = "aaa",
        },
        .{
            .re = &.concat(&.{
                &.zeroOrMore(&.literal("a")),
                &.literal("a"),
            }),
            .input = "aaabbbcca",
            .expected = "aaa",
        },
        .{
            .re = &.concat(&.{
                &.zeroOrMore(&.literal("a")),
                &.zeroOrMore(&.literal("a")),
                &.literal("aa"),
                &.literal("b"),
                &.zeroOrMore(&.literal("c")),
            }),
            .input = "aaaaab",
            .expected = "aaaaab",
        },
        .{
            .re = &.concat(&.{
                &.literal("a"),
                &.zeroOrMore(&.any),
                &.literal("x"),
            }),
            .input = "abcdxefghxij",
            .expected = "abcdxefghx",
        },

        .{
            .re = &.concat(&.{
                &.literal("a"),
                &.@"zeroOrMore?"(&.any),
                &.literal("x"),
            }),
            .input = "abcdxefghxij",
            .expected = "abcdx",
        },
        .{
            .re = &.concat(&.{
                &.literal("a"),
                &.@"zeroOrMore?"(&.any),
                &.literal("x"),
            }),
            .input = "axefghxij",
            .expected = "ax",
        },
        .{
            .re = &.concat(&.{
                &.literal("a"),
                &.zeroOrMore(&.any),
                &.literal("x"),
            }),
            .input = "axefghij",
            .expected = "ax",
        },

        .{
            .re = &.concat(&.{
                &.zeroOrMore(&.literal("a")),
                &.literal("bc"),
            }),
            .input = "aabbaabaabcca",
            .expected = null,
        },
        .{
            .re = &.concat(&.{
                &.capture(&.zeroOrMore(&.literal("a"))),
                &.capture(&.literal("bb")),
                &.capture(&.zeroOrMore(&.literal("x"))),
                &.backref(0),
                &.backref(2),
                &.backref(1),
            }),
            .input = "aabbxxxxaaxxxxbbbcca",
            .expected = "aabbxxxxaaxxxxbb",
        },
        .{
            .re = &.oneOrMore(&.word),
            .input = "abcd1234",
            .expected = "abcd1234",
        },
        .{
            .re = &.zeroOrMore(&.word),
            .input = "abcd1234",
            .expected = "abcd1234",
        },

        .{
            .re = &.concat(&.{
                &.times(4, &.word),
                &.@"!word",
                &.@"oneOrMore?"(&.digit),
            }),
            .input = "abcd.123",
            .expected = "abcd.1",
        },

        .{
            .re = &.concat(&.{
                &.times(4, &.word),
                &.@"!word",
                &.@"oneOrMore?"(&.digit),
                &.@"!digit",
            }),
            .input = "abcd.1234x-xyz",
            .expected = "abcd.1234x",
        },
        .{
            .re = &.concat(&.{
                &.boundary,
                &.literal("ab"),
                &.boundary,
                &.literal("-"),
                &.boundary,
                &.literal("12"),
                &.boundary,
            }),
            .input = "ab-12",
            .expected = "ab-12",
        },
        .{
            .re = &.concat(&.{
                &.literal("ab"),
                &.boundary,
                &.literal("cd"),
            }),
            .input = "abcd",
            .expected = null,
        },
        .{
            .re = &.concat(&.{
                &.start,
                &.literal("abcd"),
                &.end,
            }),
            .input = "abcd",
            .expected = "abcd",
        },
        .{
            .re = &.concat(&.{
                &.start,
                &.literal("abcd"),
                &.end,
            }),
            .input = "abcdefg",
            .expected = null,
        },
        .{
            .re = &.either(&.{
                &.literal("abcd"),
                &.literal("xyz"),
            }),
            .input = "abcdefg",
            .expected = "abcd",
        },
        .{
            .re = &.either(&.{
                &.literal("abcd"),
                &.literal("898989"),
                &.zeroOrMore(&.alphabet),
            }),
            .input = "xyz1abcdefg",
            .expected = "xyz",
        },
        .{
            .re = &.concat(&.{
                &.anyChar("abcx"),
                &.anyChar("xyzx"),
                &.anyChar("zooz"),
                &.@"!anyChar"("abcdef"),
            }),
            .input = "xyz1abcdefg",
            .expected = "xyz1",
        },
        .{
            .re = &.concat(&.{
                &.@"!anyChar"("a"),
                &.@"!anyChar"("a"),
                &.@"!anyChar"("a"),
                &.@"!anyChar"("a"),
                &.anyChar("a"),
            }),
            .input = "bcdea",
            .expected = "bcdea",
        },
        .{
            .re = &.concat(&.{
                &.range('x', 'z'),
                &.@"!range"('x', 'z'),
            }),
            .input = "xw",
            .expected = "xw",
        },
        .{
            .re = &.concat(&.{
                &.literal("a"),
                &.literal("b"),
                &.zeroOrMore(&.alphabet),
                &.zeroOrMore(&.digit),
                &.literal("a"),
            }),
            .input = "abcd1234abcd",
            .expected = "abcd1234a",
        },
        .{
            .re = &.concat(&.{
                &.oneOrMore(&.literal("abc")),
            }),
            .input = "aaaaabcefg",
            .expected = "abc",
        },
    };

    for (tests) |item| {
        const result = try item.re.search(std.testing.allocator, item.input);
        std.debug.print("input: {s}, expected: {s}, got: ", .{ item.input, item.expected orelse "<null>" });
        if (result) |m| {
            std.debug.print("{s}\n", .{m.string(item.input)});
        } else {
            std.debug.print("<null>\n", .{});
        }

        if (item.expected) |expected| {
            try std.testing.expect(result != null);
            try std.testing.expectEqualSlices(u8, expected, result.?.string(item.input));
        } else {
            try std.testing.expect(result == null);
        }
    }
}

// TODO: re.normalize(allocator) for optimization
// - combine adjacent literals into a single literal
//   - if all concat args is all literal, convert concat to single literal
// - flatten nested concats
// - expand char ranges into literal
// - replace backrefs of captured literals with literals
//   - not very useful since captures are done on character sets
// - expand min repetition (if min is not too large)
//   - atLeast(2, "foo") -> concat("foofoo", zeroOrMore("foo"))
//   - atLeast(2, any) -> concat(any, any, zeroOrMore(any))
// - extract common either cases
//   - either("xyz", "xyyy", "xxx") -> concat("x", either("yz", "yyy", "xx"))
// - combine either charsets
// - if either contains only literals, use a hashmap
// - use a non-allocating iterator for literal repetitions

// .atLeastOne(.either("abc", "y"))
