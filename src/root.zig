const std = @import("std");
const Allocator = std.mem.Allocator;

// seems doable and simple
// regex but without using dumb cryptic string patterns
// doing it like this isn't that too dissimilar from lpeg
// zig doesn't have regex anyway yet in the stdlib
// .
const Regex = union(enum) {
    const Self = @This();
    const RE = *const @This();

    _literal: []const u8,
    _zero_or_one: RE,

    _zero_or_more: struct {
        re: RE,
        greedy: bool,
    },
    _one_or_more: struct {
        re: RE,
        greedy: bool,
    },

    _times: struct {
        re: RE,
        count: usize,
        greedy: bool,
    },
    _at_least: struct {
        re: RE,
        count: usize,
        greedy: bool,
    },
    _around: struct {
        re: RE,
        min: usize,
        max: usize,
        greedy: bool,
    },

    _capture: RE,

    _backref: usize,

    word_boundary: void,
    digits: void,
    alphabet: void,
    alphanumeric: void,
    any: void,

    _alt: []const RE,

    _concat: []const RE,

    _char_range: struct {
        start: u8,
        end: u8,
    },

    _negate: RE, // only applicable to character ranges?

    input_start: void,
    input_end: void,

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

    fn search(self: @This(), allocator: Allocator, input: []const u8) Allocator.Error!?Match {
        var state: SearchState = .{
            .captures = .{},
        };
        defer state.captures.deinit(allocator);
        return self.match(allocator, input, &state);
    }

    fn match(self: @This(), allocator: Allocator, input: []const u8, state: *SearchState) Allocator.Error!?Match {
        return switch (self) {
            ._literal => |val| {
                if (val.len == 0 or input.len == 0)
                    return null;
                if (std.mem.eql(u8, val, input[0..@min(val.len, input.len)]))
                    return .{ .pos = 0, .len = val.len };
                return null;
            },
            ._alt => |args| {
                for (args) |re| {
                    if (try re.match(allocator, input, state)) |m| {
                        return m;
                    }
                }
                return null;
            },
            ._backref => |index| {
                const size = state.captures.items.len;
                defer state.captures.shrinkRetainingCapacity(size);

                if (index >= 0 and index < state.captures.items.len) {
                    const val = state.captures.items[index];
                    if (val.len > 0 and std.mem.eql(u8, val, input[0..@min(val.len, input.len)]))
                        return .{ .pos = 0, .len = val.len };
                }
                return null;
            },
            ._capture => |re| {
                const result = try re.match(allocator, input, state);
                if (result) |m| {
                    try state.captures.append(allocator, m.value(input));
                }
                return result;
            },

            // alternatively, wrap the concat on the constructor functions
            ._zero_or_more,
            ._one_or_more,
            ._zero_or_one,
            ._times,
            ._at_least,
            ._around,
            => return Regex.concat(&.{&self}).match(allocator, input, state),

            ._concat => |args| {
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
            },

            // TODO:
            else => return null,
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

    fn zero_or_more(re: RE) Self {
        return .{
            ._zero_or_more = .{
                .re = re,
                .greedy = true,
            },
        };
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

    fn captureAll(contents: []const RE) Self {
        return .capture(.concat(contents));
    }
};

// TODO: test
const LazyIterator = struct {
    re: Regex.RE,
    state: *Regex.SearchState,
    input: []const u8,
    pos: usize = 0,
    len: usize = 0,
    iterations: usize = 0,
    min: usize,
    max: ?usize,
    first: bool = true,

    const Self = @This();

    fn deinit(_: *Self, _: Allocator) void {}

    fn value(self: Self) []const u8 {
        const i = self.pos;
        return self.input[i .. i + self.len];
    }

    fn current(self: Self) Regex.Match {
        return .{
            .pos = self.pos,
            .len = self.len,
        };
    }

    fn next(self: *Self, allocator: Allocator) !?Regex.Match {
        const state = self.state;
        var input = self.input;

        if (self.first) {
            self.first = false;

            while (self.iterations < self.min) {
                if (try self.re.match(allocator, input[self.pos..], state)) |m| {
                    self.pos = m.pos;
                    self.len = m.len;
                    self.iterations += 1;
                } else return null;
            }

            return self.current();
        }

        const max = self.max orelse std.math.maxInt(usize);

        if (self.iterations <= max) {
            if (try self.re.match(allocator, input[self.pos..], state)) |m| {
                self.pos = m.pos;
                self.len = m.len;
                self.iterations += 1;
                return self.current();
            }
        }

        return null;
    }
};

const GreedyIterator = struct {
    re: Regex.RE,
    state: *Regex.SearchState,
    input: []const u8,
    pos: usize = 0,
    len: usize = 0,
    min: usize,
    max: ?usize = null,
    first: bool = true,

    // TODO: store only the len since the substrings
    // are sequential and no gaps in between
    lengths: std.ArrayListUnmanaged(Regex.Match) = .{},

    const Self = @This();

    fn deinit(self: *Self, allocator: Allocator) void {
        self.lengths.deinit(allocator);
    }

    fn value(self: Self) []const u8 {
        const i = self.pos;
        return self.input[i .. i + self.len];
    }

    fn current(self: Self) Regex.Match {
        return .{ .pos = self.pos, .len = self.len };
    }

    fn next(self: *Self, allocator: Allocator) !?Regex.Match {
        const state = self.state;

        if (self.first) {
            self.first = false;

            var n: usize = 0;
            var input = self.input;

            while (n < self.min) {
                if (try self.re.match(allocator, input[self.pos..], state)) |m| {
                    self.pos = m.pos;
                    self.len = m.len;
                    n += 1;
                } else return null;
            }

            var matched = false;
            var i: usize = 0;

            if (n == 0) {
                try self.lengths.append(allocator, .{
                    .pos = 0,
                    .len = 0,
                });
                matched = true;
            }

            while (true) {
                if (self.max) |max| {
                    if (self.lengths.items.len >= max) break;
                }

                if (try self.re.match(allocator, input[i..], state)) |m| {
                    if (!matched) {
                        self.pos = m.pos;
                    }
                    matched = true;
                    self.len += m.len;
                    i += m.pos + m.len;

                    try self.lengths.append(allocator, .{
                        .pos = self.pos,
                        .len = self.len,
                    });
                } else break;
            }

            return if (matched) self.current() else null;
        }

        if (self.lengths.pop()) |m| {
            self.pos = m.pos;
            self.len = m.len;
            return self.current();
        }

        return null;
    }
};

const SingleIterator = struct {
    re: Regex.RE,
    state: *Regex.SearchState,
    input: []const u8,
    pos: usize = 0,
    len: usize = 0,
    first: bool = true,

    const Self = @This();

    fn deinit(_: *Self, _: Allocator) void {}

    fn value(self: Self) []const u8 {
        const i = self.pos;
        return self.input[i .. i + self.len];
    }

    fn current(self: Self) Regex.Match {
        return .{ .pos = self.pos, .len = self.len };
    }

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
        input: []const u8,
        state: *Regex.SearchState,
    ) Self {
        return switch (re.*) {
            else => .{ .single = .{
                .re = re,
                .state = state,
                .input = input,
            } },
            ._zero_or_more => |val| createRepetition(.{
                .greedy = val.greedy,
                .re = val.re,
                .state = state,
                .input = input,
                .min = 0,
            }),
            ._one_or_more => |val| createRepetition(.{
                .greedy = val.greedy,
                .re = val.re,
                .state = state,
                .input = input,
                .min = 1,
            }),
            ._zero_or_one => |val| createRepetition(.{
                .greedy = false,
                .re = val,
                .state = state,
                .input = input,
                .min = 0,
                .max = 1,
            }),
            ._times => |val| createRepetition(.{
                .greedy = val.greedy,
                .re = val.re,
                .state = state,
                .input = input,
                .min = val.count,
                .max = val.count,
            }),
            ._at_least => |val| createRepetition(.{
                .greedy = val.greedy,
                .re = val.re,
                .state = state,
                .input = input,
                .min = val.count,
            }),
            ._around => |val| createRepetition(.{
                .greedy = val.greedy,
                .re = val.re,
                .state = state,
                .input = input,
                .min = val.min,
                .max = val.max,
            }),
        };
    }

    fn createRepetition(args: struct {
        greedy: bool,
        re: Regex.RE,
        state: *Regex.SearchState,
        input: []const u8,
        min: usize,
        max: ?usize = null,
    }) Self {
        return if (args.greedy) .{ .greedy = GreedyIterator{
            .re = args.re,
            .state = args.state,
            .input = args.input,
            .min = args.min,
            .max = args.max,
        } } else .{ .lazy = LazyIterator{
            .re = args.re,
            .state = args.state,
            .input = args.input,
            .min = args.min,
            .max = args.max,
        } };
    }

    fn deinit(self: *Iterator, allocator: Allocator) void {
        return switch (self.*) {
            .greedy => |*iter| iter.deinit(allocator),
            .lazy => |*iter| iter.deinit(allocator),
            .single => |*iter| iter.deinit(allocator),
        };
    }

    fn value(self: Iterator) ?[]const u8 {
        return switch (self) {
            .greedy => |*iter| iter.value(),
            .lazy => |*iter| iter.value(),
            .single => |*iter| iter.value(),
        };
    }

    fn next(self: *Iterator, allocator: Allocator) !?Regex.Match {
        return switch (self.*) {
            .greedy => |*iter| try iter.next(allocator),
            .lazy => |*iter| try iter.next(allocator),
            .single => |*iter| try iter.next(allocator),
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
            .re = &.zero_or_more(&.literal("a")),
            .input = "qwersdfz",
            .expected = "",
        },
        .{
            .re = &.zero_or_more(&.literal("a")),
            .input = "aaaqwersdfz",
            .expected = "aaa",
        },
        .{
            .re = &.concat(&.{
                &.zero_or_more(&.literal("a")),
                &.literal("a"),
            }),
            .input = "aaabbbcca",
            .expected = "aaa",
        },
        .{
            .re = &.concat(&.{
                &.zero_or_more(&.literal("a")),
                &.zero_or_more(&.literal("a")),
                &.literal("aa"),
                &.literal("b"),
                &.zero_or_more(&.literal("c")),
            }),
            .input = "aaaaab",
            .expected = "aaaaab",
        },
        .{
            .re = &.concat(&.{
                &.zero_or_more(&.literal("a")),
                &.literal("bc"),
            }),
            .input = "aabbaabaabcca",
            .expected = null,
        },
        .{
            .re = &.concat(&.{
                &.capture(&.zero_or_more(&.literal("a"))),
                &.capture(&.literal("bb")),
                &.backref(0),
                &.backref(1),
            }),
            .input = "aabbaabbaabcca",
            .expected = "aabbaabb",
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
