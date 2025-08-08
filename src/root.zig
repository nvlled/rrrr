const std = @import("std");
const Allocator = std.mem.Allocator;

const U8Enum = enum(u8) {

    // TODO: better way of doing this?
    // zig fmt: off
    _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18,
    _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33, _34, _35,
    _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, _51, _52,
    _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63, _64, _65, _66, _67, _68, _69,
    _70, _71, _72, _73, _74, _75, _76, _77, _78, _79, _80, _81, _82, _83, _84, _85, _86,
    _87, _88, _89, _90, _91, _92, _93, _94, _95, _96, _97, _98, _99, _100, _101, _102,
    _103, _104, _105, _106, _107, _108, _109, _110, _111, _112, _113, _114, _115, _116,
    _117, _118, _119, _120, _121, _122, _123, _124, _125, _126, _127, _128, _129, _130,
    _131, _132, _133, _134, _135, _136, _137, _138, _139, _140, _141, _142, _143, _144,
    _145, _146, _147, _148, _149, _150, _151, _152, _153, _154, _155, _156, _157, _158,
    _159, _160, _161, _162, _163, _164, _165, _166, _167, _168, _169, _170, _171, _172,
    _173, _174, _175, _176, _177, _178, _179, _180, _181, _182, _183, _184, _185, _186,
    _187, _188, _189, _190, _191, _192, _193, _194, _195, _196, _197, _198, _199, _200,
    _201, _202, _203, _204, _205, _206, _207, _208, _209, _210, _211, _212, _213, _214,
    _215, _216, _217, _218, _219, _220, _221, _222, _223, _224, _225, _226, _227, _228,
    _229, _230, _231, _232, _233, _234, _235, _236, _237, _238, _239, _240, _241, _242,
    _243, _244, _245, _246, _247, _248, _249, _250, _251, _252, _253, _254,
    // zig fmt: on

    fn enumSet(str: []const u8) std.EnumSet(@This()) {
        var set: std.EnumSet(U8Enum) = .initEmpty();
        for (str) |c| set.insert(@enumFromInt(c));
        return set;
    }
};

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

    _charset: std.EnumSet(U8Enum),

    pub const Match = struct {
        pos: usize,
        len: usize,

        pub fn value(self: @This(), source: []const u8) []const u8 {
            const i = self.pos;
            return source[i .. i + self.len];
        }
    };

    const SearchState = struct {
        captures: std.ArrayListUnmanaged([]const u8),
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
        fn next(self: @This()) ?u8 {
            const len = self.value.len;
            if (self.pos < len - 1 and self.value.len > 0)
                return self.value[self.pos + 1];
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
        const digit = U8Enum.enumSet("0123456789");
        const alphabet = U8Enum.enumSet("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
        const underscore = U8Enum.enumSet("_");
        const whitespace = U8Enum.enumSet(" \t\n\r\x0C\x0B\x85");
        const any = U8Enum.enumSet("").complement();
        const alphanum = digit.unionWith(alphabet);
        const word = alphanum.unionWith(underscore);

        const not_word = word.complement();
        const not_digit = digit.complement();
        const not_alphabet = alphabet.complement();
        const not_alphanum = alphanum.complement();
        const not_whitespace = whitespace.complement();

        inline fn isWord(ch: u8) bool {
            return word.contains(@enumFromInt(ch));
        }

        fn get(re: Regex) ?std.EnumSet(U8Enum) {
            return switch (re) {
                .word => word,
                .digit => digit,
                .alphabet => alphabet,
                .alphanum => alphanum,
                .whitespace => whitespace,
                .@"!word" => not_word,
                .@"!digit" => not_digit,
                .@"!alphabet" => not_alphabet,
                .@"!alphanum" => not_alphanum,
                .@"!whitespace" => not_whitespace,
                else => null,
            };
        }
    };

    fn search(self: @This(), allocator: Allocator, input: []const u8) Allocator.Error!?Match {
        var state: SearchState = .{
            .captures = .{},
        };
        defer state.captures.deinit(allocator);
        return self.match(allocator, .{ .value = input, .pos = 0 }, &state);
    }

    fn handleConcat(
        allocator: Allocator,
        input_arg: Input,
        state: *SearchState,
        args: []const RE,
    ) !?Match {
        if (args.len == 0) return .{ .pos = 0, .len = 0 };

        const size = state.captures.items.len;
        defer state.captures.shrinkRetainingCapacity(size);

        var i: usize = 0;
        var input = input_arg;
        var result: Match = .{ .pos = 0, .len = 0 };

        // The following loop is an optimization to reduce function calls
        // by recursing only on .repetition.
        // The loop can be removed without affecting the result.
        loop: while (true) : (i += 1) {
            var re = args[i];
            switch (re.*) {
                ._repetition => break :loop,
                else => {},
            }
            const m = try re.match(allocator, input, state) orelse return null;
            if (i == 0) result.pos = m.pos;
            result.len += m.len;
            input = input.slice(m.len);

            if (i >= args.len - 1) {
                return result;
            }
        }

        const re = args[i];
        const rest_args = args[i + 1 ..];

        var iterator: Iterator = .init(re, input, state);
        defer iterator.deinit(allocator);

        while (try iterator.next(allocator)) |m| {
            const rest_input = input.slice(m.pos + m.len);
            if (rest_args.len == 0) return .{
                .pos = result.pos,
                .len = result.len + m.len,
            };

            if (try handleConcat(allocator, rest_input, state, rest_args)) |m2| {
                return .{
                    .pos = result.pos,
                    .len = result.len + m.len + m2.len,
                };
            }
        }

        return null;
    }

    fn handleConcatIterative(
        allocator: Allocator,
        input: []const u8,
        state: *SearchState,
        args: []const RE,
    ) !?Match {
        const size = state.captures.items.len;
        defer state.captures.shrinkRetainingCapacity(size);

        if (args.len == 0) return null;

        var i: usize = 0;
        var step_back = false;
        var result: Match = .{ .pos = 0, .len = 0 };
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
                    state.captures.shrinkRetainingCapacity(capture_size);

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
                    .capture_size = state.captures.items.len,
                }),
            }

            result.len += m.len;
            if (i == 0) result.pos = m.pos;
            if (i >= args.len - 1) return result;
            if (capture_result) try state.captures.append(allocator, m.value(str));
            i += 1;
        }

        return null;
    }

    fn match(self: @This(), allocator: Allocator, input_arg: Input, state: *SearchState) Allocator.Error!?Match {
        const match_one: Match = .{ .pos = 0, .len = 1 };
        const match_zero: Match = .{ .pos = 0, .len = 0 };

        switch (self) {
            .boundary, .start, .end => {},
            else => {
                const input = input_arg.string();
                if (input.len == 0) return null;
                if (CharClass.get(self)) |set| {
                    const elem: U8Enum = @enumFromInt(input[0]);
                    return if (set.contains(elem)) match_one else null;
                }
            },
        }

        return switch (self) {
            .any => match_one,

            ._charset => |set| {
                const input = input_arg.string();
                const elem: U8Enum = @enumFromInt(input[0]);
                return if (set.contains(elem)) match_one else null;
            },

            .start => if (input_arg.pos == 0) match_zero else null,
            .end => {
                const len = input_arg.size();
                return if (len == 0 or input_arg.pos == len - 1) match_zero else null;
            },

            .boundary => {
                const prev = input_arg.prev() orelse return match_zero;
                const next = input_arg.current() orelse return match_zero;

                const is_prev_word = CharClass.isWord(prev);
                const is_next_word = CharClass.isWord(next);
                const matched =
                    is_prev_word and !is_next_word or
                    !is_prev_word and is_next_word;

                return if (matched) match_zero else null;
            },

            ._literal => |val| {
                const input = input_arg.string();
                if (std.mem.eql(u8, val, input[0..@min(val.len, input.len)]))
                    return .{ .pos = 0, .len = val.len }
                else
                    return null;
            },

            ._alt => |args| {
                for (args) |re| {
                    if (try re.match(allocator, input_arg, state)) |m| {
                        return m;
                    }
                }
                return null;
            },

            ._backref => |index| {
                const size = state.captures.items.len;
                defer state.captures.shrinkRetainingCapacity(size);

                if (index >= 0 and index < state.captures.items.len) {
                    const input = input_arg.string();
                    const val = state.captures.items[index];
                    if (val.len > 0 and std.mem.eql(u8, val, input[0..@min(val.len, input.len)]))
                        return .{ .pos = 0, .len = val.len };
                }
                return null;
            },

            ._capture => |re| {
                const result = try re.match(allocator, input_arg, state);
                if (result) |m| {
                    const input = input_arg.string();
                    try state.captures.append(allocator, m.value(input));
                }
                return result;
            },

            ._repetition => handleConcat(allocator, input_arg, state, &.{&self}),

            ._concat => |args| handleConcat(allocator, input_arg, state, args),

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
        var set: std.EnumSet(U8Enum) = .initEmpty();
        for (value) |c| set.insert(@enumFromInt(c));
        return .{ ._charset = set };
    }
    fn @"!anyChar"(value: []const u8) Self {
        var set: std.EnumSet(U8Enum) = .initEmpty();
        for (value) |c| set.insert(@enumFromInt(c));
        return .{ ._charset = set.complement() };
    }

    fn range(start: u8, end: u8) Self {
        var set: std.EnumSet(U8Enum) = .initEmpty();
        for (start..end) |c| set.insert(@enumFromInt(c));
        return .{ ._charset = set };
    }
    fn @"!range"(start: u8, end: u8) Self {
        var set: std.EnumSet(U8Enum) = .initEmpty();
        for (start..end) |c| set.insert(@enumFromInt(c));
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
    state: *Regex.SearchState,
    input: Regex.Input,
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
        var input = self.input;

        if (self.first) {
            self.first = false;

            // skip first few matches until min
            while (self.iterations < self.min) {
                if (try self.re.match(allocator, input.slice(self.pos), state)) |m| {
                    self.pos = m.pos;
                    self.len = m.len;
                    self.iterations += 1;
                } else return null;
            }

            return .{ .pos = self.pos, .len = self.len };
        }

        const max = self.max orelse std.math.maxInt(usize);

        if (self.iterations <= max) {
            if (try self.re.match(allocator, input.slice(self.pos + self.len), state)) |m| {
                self.len += m.len;
                self.iterations += 1;
                return .{ .pos = self.pos, .len = self.len };
            }
        }

        // match zero string
        if (self.iterations <= max + 1) {
            self.iterations += 1;
            self.pos = 0;
            self.len = 0;
            return .{ .pos = self.pos, .len = self.len };
        }

        return null;
    }
};

const GreedyIterator = struct {
    re: Regex.RE,
    state: *Regex.SearchState,
    input: Regex.Input,
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
            var input = self.input;
            var lengths = &self.lengths;

            // skip first few matches until min
            while (n < self.min) {
                if (try self.re.match(allocator, input.slice(self.pos), state)) |m| {
                    self.pos = m.pos;
                    n += 1;
                } else return null;
            }

            if (n == 0) try lengths.append(allocator, 0);

            while (true) {
                if (self.max) |max| {
                    if (lengths.items.len >= max) break;
                }

                if (try self.re.match(allocator, input.slice(self.pos + self.len), state)) |m| {
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
    state: *Regex.SearchState,
    input: Regex.Input,
    pos: usize = 0,
    len: usize = 0,
    first: bool = true,

    const Self = @This();

    fn deinit(_: *Self, _: Allocator) void {}

    fn next(self: *Self, allocator: Allocator) !?Regex.Match {
        if (self.first) {
            self.first = false;
            const result = try self.re.match(allocator, self.input, self.state);
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
        input: Regex.Input,
        state: *Regex.SearchState,
    ) Self {
        return switch (re.*) {
            ._repetition => |val| if (val.greedy) .{ .greedy = GreedyIterator{
                .re = val.re,
                .state = state,
                .input = input,
                .min = val.min,
                .max = val.max,
            } } else .{ .lazy = LazyIterator{
                .re = val.re,
                .state = state,
                .input = input,
                .min = val.min,
                .max = val.max,
            } },
            else => .{ .single = .{
                .re = re,
                .state = state,
                .input = input,
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
                &.backref(0),
                &.backref(1),
            }),
            .input = "aabbaabbaabcca",
            .expected = "aabbaabb",
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
    };

    for (tests) |item| {
        const result = try item.re.search(std.testing.allocator, item.input);
        std.debug.print("input: {s}, expected: {s}, got: ", .{ item.input, item.expected orelse "<null>" });
        if (result) |m| {
            std.debug.print("{s}\n", .{m.value(item.input)});
        } else {
            std.debug.print("<null>\n", .{});
        }

        if (item.expected) |expected| {
            try std.testing.expect(result != null);
            try std.testing.expectEqualSlices(u8, expected, result.?.value(item.input));
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
//

// .atLeastOne(.either("abc", "y"))
