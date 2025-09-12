const std = @import("std");
const BitSet = std.bit_set.IntegerBitSet(256);
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const tryAlloc = @import("./alloc.zig").@"try";
var stderr = std.fs.File.stderr().writer(&.{});

pub const MatchResult = struct {
    // position within the source string
    pos: usize,
    len: usize,
    capture: ?*const Capture = null,

    // Resulting captures that are intended to be allocated on the heap memory.
    //
    // (heap)
    // capture
    //   .next --|                           (stack frame)
    //           |
    // ----------+----------------------------------------
    //           |   (heap)
    //           |-> capture
    //                 .next --|             (stack frame)
    //                         |
    // ------------------------+--------------------------
    //                         |   (heap)
    //                         |-> capture
    //                               .next   (stack frame)
    const Capture = struct {
        len: usize,
        pos: usize,
        next: ?*const Capture,

        fn string(self: *const Capture, source: []const u8) []const u8 {
            const i = self.pos;
            return source[i .. i + self.len];
        }

        // Returns the number of captures in the list.
        fn count(self: *const Capture) usize {
            var n: usize = 0;
            var current: ?*const Capture = self;

            while (current) |capture| {
                n += 1;
                const next = capture.next;
                current = next;
            }

            return n;
        }

        // Copies all captures in the linked-list.
        // If the allocator is not an ArenaAllocator,
        // caller must free returned all captures, either manually
        // with `Capture.freeAll()`.
        fn dupeAll(self: *const Capture, allocator: Allocator) *Capture {
            const result = tryAlloc(allocator.create(MatchResult.Capture));
            result.* = self.*;

            var cur_clone: *Capture = result;
            var cur_original: *const Capture = self;
            while (cur_original.next) |next| {
                const clone = tryAlloc(allocator.create(MatchResult.Capture));
                clone.* = next.*;
                cur_clone.next = clone;
                cur_clone = clone;
                cur_original = next;
            }

            return result;
        }

        // Frees all captures in the linked-list.
        fn freeAll(self: *const Capture, allocator: Allocator) void {
            var current: ?*const Capture = self;
            while (current) |capture| {
                const next = capture.next;
                allocator.destroy(capture);
                current = next;
            }
        }

        // Creates a slice of captures from the linked-list.
        // Caller must free the slice if allocator is not an ArenaAllocator.
        fn toSlice(self: *const Capture, allocator: Allocator) []*const Capture {
            const n: usize = self.count();
            const slice = tryAlloc(allocator.alloc(*const Capture, n));

            var i: usize = 0;
            var current: ?*const Capture = self;
            while (current) |capture| {
                slice[i] = capture;
                const next = capture.next;
                current = next;
                i += 1;
            }

            return slice;
        }

        // Returns a new (copy) of the list
        // with the two args appended together.
        // Caller must free the returned capture list.
        fn connect(
            arena: Allocator,
            left_arg: ?*const Capture,
            right_arg: ?*const Capture,
        ) ?*const Capture {
            if (left_arg == null) {
                return right_arg;
            }
            const left = left_arg.?.dupeAll(arena);

            var tail: *Capture = left;
            while (tail.next) |c| {
                tail = @constCast(c);
            }

            tail.next = right_arg;
            return left;
        }
    };

    pub fn freeCaptures(self: @This(), allocator: Allocator) void {
        if (self.capture) |capture| {
            capture.freeAll(allocator);
        }
    }

    pub fn string(self: @This(), source: []const u8) []const u8 {
        const i = self.pos;
        return source[i .. i + self.len];
    }
};

const MatchState = struct {
    capture: ?*const Capture,
    input: MatchInput,
    running: *std.atomic.Value(bool),
    thread_pool: *std.Thread.Pool,

    // Currently only used for intermediate captures,
    // particularly, for the `dupeAll()` and `connect()` methods
    // of `MatchResult.Capture`.
    // Main allocator will be used for returned captures
    arena: *std.heap.ArenaAllocator,

    // Temporary captures while the search is still ongoing,
    // intended to be allocated on the stack memory.
    //
    // (stack)
    // capture <-|
    //   .prev   |                          (stack frame)
    //           |
    // ----------+---------------------------------------
    //           |  (stack)
    //           |  capture <-|
    //           ---- .prev   |             (stack frame)
    //                        |
    // -----------------------+--------------------------
    //                        |  (stack)
    //                        |  capture
    //                        ---- .prev    (stack frame)
    const Capture = struct {
        pos: usize,
        len: usize,
        prev: ?*const Capture,

        fn string(self: *const Capture, source: []const u8) []const u8 {
            const i = self.pos;
            return source[i .. i + self.len];
        }

        fn toResult(
            self: *const Capture,
            allocator: Allocator,
        ) *MatchResult.Capture {
            const result = tryAlloc(allocator.create(MatchResult.Capture));
            result.* = .{
                .pos = self.pos,
                .len = self.len,
                .next = null,
            };
            return result;
        }
    };

    fn getCapture(self: @This(), index: usize) ?*const Capture {
        var current: *const Capture = self.capture orelse return null;

        var array: [100]*const Capture = undefined;
        array[0] = current;

        var len: usize = 1;
        while (current.prev) |prev| {
            if (len >= array.len) @panic("too many captures, limit is 100");
            current = prev;
            array[len] = current;
            len += 1;
        }

        return array[len - 1 - index];
    }

    fn sliceInput(self: @This(), offset: usize) MatchState {
        return .{
            .capture = self.capture,
            .input = self.input.slice(offset),
            .arena = self.arena,
            .running = self.running,
            .thread_pool = self.thread_pool,
        };
    }
};

const MatchInput = struct {
    value: []const u8,
    pos: usize = 0,

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

    fn slice(self: @This(), offset: usize) MatchInput {
        return .{
            .value = self.value,
            .pos = self.pos + offset,
        };
    }

    fn hasMore(self: @This()) bool {
        return self.pos < self.value.len;
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

pub const RE = *const Regex;

pub const Regex = union(enum) {
    const Self = @This();

    literal_string: []const u8,

    repetition: struct {
        re: RE,
        min: usize,
        max: ?usize = null,
        greedy: bool,
    },

    alternation: []const RE,

    concatenation: []const RE,

    captured: RE,

    back_reference: usize,

    start,
    end,
    boundary,

    any,
    none,
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

    char_class_set: BitSet,

    // Note: constructor functions like zeroOrMore
    // takes a pointer and return a value so
    // it could be used without heap allocating.
    // Ideally they could take a value parameter,
    // but Regex is self-referential, so they need
    // to be pointers.

    pub fn literal(value: []const u8) Self {
        return .{ .literal_string = value };
    }

    pub fn eitherChars(value: []const u8) Self {
        var set: BitSet = .initEmpty();
        for (value) |c| set.set(c);
        return .{ .char_class_set = set };
    }

    pub fn exceptChar(value: []const u8) Self {
        var set: BitSet = .initEmpty();
        for (value) |c| set.set(c);
        return .{ .char_class_set = set.complement() };
    }

    pub fn range(start: u8, end: u8) Self {
        var set: BitSet = .initEmpty();
        for (start..end + 1) |c| set.set(c);
        return .{ .char_class_set = set };
    }

    pub fn @"!range"(start: u8, end: u8) Self {
        var set: BitSet = .initEmpty();
        for (start..end + 1) |c| set.set(c);
        return .{ .char_class_set = set.complement() };
    }

    pub fn charset(set: BitSet) Self {
        return .{ .char_class_set = set };
    }

    pub fn either(args: []const RE) Self {
        return .{ .alternation = args };
    }

    pub fn optional(re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = false,
            },
        };
    }

    pub fn @"zeroOrMore?"(re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = false,
                .min = 0,
            },
        };
    }

    pub fn zeroOrMore(re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = true,
                .min = 0,
            },
        };
    }

    pub fn oneOrMore(re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = true,
                .min = 1,
            },
        };
    }

    pub fn @"oneOrMore?"(re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = false,
                .min = 1,
            },
        };
    }

    pub fn atLeast(count: usize, re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = true,
                .min = count,
            },
        };
    }

    pub fn @"atLeast?"(count: usize, re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = false,
                .min = count,
            },
        };
    }

    pub fn atMost(count: usize, re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = true,
                .min = 0,
                .max = count,
            },
        };
    }

    pub fn @"atMost?"(count: usize, re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = false,
                .min = 0,
                .max = count,
            },
        };
    }

    pub fn around(min: usize, max: usize, re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = true,
                .min = min,
                .max = max,
            },
        };
    }

    pub fn @"around?"(min: usize, max: usize, re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = false,
                .min = min,
                .max = max,
            },
        };
    }

    pub fn times(count: usize, re: RE) Self {
        return .{
            .repetition = .{
                .re = re,
                .greedy = true,
                .min = count,
                .max = count,
            },
        };
    }

    pub inline fn @"*"(re: RE) Self {
        return zeroOrMore(re);
    }

    pub inline fn @"+"(re: RE) Self {
        return @"oneOrMore?"(re);
    }

    pub inline fn @"*?"(re: RE) Self {
        return @"zeroOrMore?"(re);
    }

    pub inline fn @"+?"(re: RE) Self {
        return oneOrMore(re);
    }

    pub fn backref(index: usize) Self {
        return .{ .back_reference = index };
    }

    pub fn concat(contents: []const RE) Self {
        return .{ .concatenation = contents };
    }

    pub fn capture(re: RE) Self {
        return .{ .captured = re };
    }

    pub fn equals(re1: RE, re2: RE) bool {
        if (std.meta.activeTag(re1.*) != std.meta.activeTag(re2.*)) {
            return false;
        }

        return switch (re1.*) {
            .literal_string => std.mem.eql(u8, re1.literal_string, re2.literal_string),
            .captured => return equals(re1.captured, re2.captured),
            .back_reference => re1.back_reference == re2.back_reference,

            .concatenation => {
                const a = re1.concatenation;
                const b = re2.concatenation;
                if (a.len != b.len) return false;

                for (0..a.len) |i| {
                    if (!equals(
                        a[i],
                        b[i],
                    )) return false;
                }
                return true;
            },
            .alternation => {
                const a = re1.alternation;
                const b = re2.alternation;
                if (a.len != b.len) return false;

                for (0..a.len) |i| {
                    if (!equals(
                        a[i],
                        b[i],
                    )) return false;
                }
                return true;
            },
            .repetition => {
                const a = re1.repetition;
                const b = re2.repetition;
                if (!equals(a.re, b.re)) return false;
                return a.min == b.min and
                    a.max == b.max and
                    a.greedy == b.greedy;
            },

            else => true,
        };
    }

    // non-recursive, shallow copy
    fn dupe(self: RE, allocator: Allocator) RE {
        const copy = tryAlloc(allocator.create(Self));
        copy.* = self.*;
        return copy;
    }

    pub fn recursiveCopy(self: RE, allocator: Allocator) RE {
        const result: Self = blk: switch (self.*) {
            .literal_string => |val| {
                const str = tryAlloc(allocator.dupe(u8, val));
                break :blk .literal(str);
            },

            inline .concatenation, .alternation => |args, t| {
                var args_buf = tryAlloc(ArrayList(RE).initCapacity(allocator, args.len));

                for (args) |arg| {
                    const arg_copy = arg.recursiveCopy(allocator);
                    tryAlloc(args_buf.append(allocator, arg_copy));
                }

                const result = tryAlloc(args_buf.toOwnedSlice(allocator));
                break :blk switch (t) {
                    .concatenation => .concat(result),
                    .alternation => .either(result),
                    else => comptime unreachable,
                };
            },

            .repetition => |val| {
                const inner_re = val.re.recursiveCopy(allocator);

                return (Self{
                    .repetition = .{
                        .re = inner_re,
                        .min = val.min,
                        .max = val.max,
                        .greedy = val.greedy,
                    },
                }).dupe(allocator);
            },
            .captured => |re| {
                const copy = re.recursiveCopy(allocator);
                return Self.capture(copy).dupe(allocator);
            },

            else => break :blk self.*,
        };

        return result.dupe(allocator);
    }

    pub fn recursiveFree(self: RE, allocator: Allocator) void {
        switch (self.*) {
            .literal_string => |val| allocator.free(val),
            .repetition => |val| val.re.recursiveFree(allocator),
            .captured => |re| re.recursiveFree(allocator),
            .concatenation, .alternation => |args| {
                for (args) |sub_re| sub_re.recursiveFree(allocator);
                allocator.free(args);
            },
            else => {},
        }
        allocator.destroy(self);
    }

    fn match(self: Self, allocator: Allocator, state: MatchState) ?MatchResult {
        if (!state.running.load(.unordered)) return null;

        const input_arg = state.input;
        const match_zero: MatchResult = .{ .pos = input_arg.pos, .len = 0 };

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
            .@"!boundary" => {
                const re: RE = &.boundary;
                return if (re.match(allocator, state)) |_| null else match_zero;
            },

            .none => return null,

            .start => return if (input_arg.pos == 0) match_zero else null,
            .end => {
                const len = input_arg.size();
                return if (len == 0 or input_arg.pos == len - 1) match_zero else null;
            },

            .literal_string => |val| {
                const input = input_arg.string();
                if (std.mem.eql(u8, val, input[0..@min(val.len, input.len)]))
                    return .{ .pos = input_arg.pos, .len = val.len }
                else
                    return null;
            },

            .back_reference => |index| {
                const input = input_arg.string();
                const item = (state.getCapture(index) orelse return null);
                const str = item.string(input_arg.value);
                if (item.len > 0 and std.mem.eql(u8, str, input[0..@min(str.len, input.len)])) {
                    return .{ .pos = input_arg.pos, .len = item.len };
                }
                return null;
            },

            .repetition => |val| switch (val.re.*) {
                .captured => @panic(
                    \\capture must not be done directly inside of repetition,
                    \\move it outside instead: capture(repeat(...))
                    \\or normalize it do it automatically
                ),
                else => matchConcat(allocator, state, &.{&self}),
            },

            .captured => return matchConcat(allocator, state, &.{&self}),

            .alternation => |args| {
                for (args) |re| {
                    if (re.* == .captured)
                        @panic(
                            \\capture must not be done directly inside of alternation,
                            \\move it outside instead: capture(either(...))
                        );
                }
                return matchConcat(allocator, state, &.{&self});
            },

            .concatenation => |args| matchConcat(allocator, state, args),

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
            .char_class_set,
            => |val, tag| {
                if (input_arg.size() == 0) return null;

                const input = input_arg.string();
                const ch = input[0];
                const match_one: MatchResult = .{ .pos = input_arg.pos, .len = 1 };
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
                    .char_class_set => if (val.isSet(ch)) match_one else null,

                    else => comptime unreachable,
                };
            },
        };
    }

    fn matchConcat(
        allocator: Allocator,
        state_arg: MatchState,
        args: []const RE,
    ) ?MatchResult {
        if (!state_arg.running.load(.unordered)) return null;
        if (args.len == 0) return .{ .pos = state_arg.input.pos, .len = 0 };

        var i: usize = 0;
        var state = state_arg;
        var result: MatchResult = .{ .pos = state.input.pos, .len = 0 };

        loop: while (i < args.len) : (i += 1) {
            const re = args[i];
            var current_cap: ?*MatchResult.Capture = null;
            switch (re.*) {
                .repetition, .alternation, .captured => break :loop,
                else => {
                    const m = re.match(allocator, state) orelse return null;

                    result.len += m.len;

                    const arena = state.arena.allocator();
                    if (current_cap) |cap| {
                        const c = cap.dupeAll(arena);
                        cap.next = c;
                    } else if (m.capture) |cap| {
                        current_cap = cap.dupeAll(arena);
                        result.capture = current_cap;
                    }

                    state = state.sliceInput(m.len);
                },
            }
        }

        if (i >= args.len) {
            return result;
        }

        const rest_args = args[i + 1 ..];
        switch (args[i].*) {
            .repetition => {
                var iterator: MatchIterator = .init(args[i], state);
                defer iterator.deinit(allocator);

                // Spawn a thread for each repetition match.
                // Since repetitions must match the shortest
                // or longest possible match, the checking
                // of matches must be done in order:

                var thread_pool = state_arg.thread_pool;
                var batch: [4]struct {
                    wg: std.Thread.WaitGroup,
                    result: ?MatchResult,
                } = .{
                    .{ .wg = .{}, .result = null },
                    .{ .wg = .{}, .result = null },
                    .{ .wg = .{}, .result = null },
                    .{ .wg = .{}, .result = null },
                };

                var hasMore = true;
                var running: std.atomic.Value(bool) = .init(true);
                while (hasMore) {
                    var processed: usize = 0;
                    for (0..batch.len) |j| {
                        if (iterator.next(allocator)) |m| {
                            processed += 1;
                            var sub_state = state.sliceInput(m.len);
                            sub_state.running = &running;

                            thread_pool.spawnWg(
                                &batch[j].wg,
                                matchConcatWrapped,
                                .{
                                    allocator,
                                    m,
                                    sub_state,
                                    rest_args,
                                    &batch[j].result,
                                },
                            );
                        } else hasMore = false;
                    }

                    for (0..processed) |j| {
                        batch[j].wg.wait();

                        if (batch[j].result) |m| {
                            running.store(false, .unordered);
                            return .{
                                .pos = result.pos,
                                .len = result.len + m.len,
                                .capture = m.capture,
                            };
                        }

                        batch[j].wg.reset();
                    }
                }

                return null;
            },

            .alternation => {
                var iterator: MatchIterator = .init(args[i], state);
                defer iterator.deinit(allocator);

                var thread_pool = state_arg.thread_pool;
                var wg: std.Thread.WaitGroup = .{};
                var sub_result: ?MatchResult = null;

                while (iterator.next(allocator)) |m| {
                    if (!state_arg.running.load(.unordered)) return null;
                    if (sub_result != null) break;

                    const sub_state = state.sliceInput(m.len);
                    thread_pool.spawnWg(
                        &wg,
                        matchConcatWrapped,
                        .{
                            allocator,
                            m,
                            sub_state,
                            rest_args,
                            &sub_result,
                        },
                    );
                }

                // Wait for all sub-matches to finish.
                // Ideally this should just wait for just one
                // one match to occur, but that would fail
                // if regex isn't normalized and there's
                // a common prefix between the choices.
                wg.wait();

                return if (sub_result) |m| .{
                    .pos = result.pos,
                    .len = result.len + m.len,
                    .capture = m.capture,
                } else null;
            },

            .captured => |re| {
                const m = re.match(allocator, state) orelse return null;

                const sub_state: MatchState = .{
                    .capture = &.{
                        .pos = m.pos,
                        .len = m.len,
                        .prev = if (state.capture) |c| c else null,
                    },
                    .input = state.input.slice(m.len),
                    .arena = state.arena,
                    .running = state.running,
                    .thread_pool = state.thread_pool,
                };

                if (matchConcat(allocator, sub_state, rest_args)) |m2| {
                    const arena = state.arena.allocator();
                    return .{
                        .pos = result.pos,
                        .len = result.len + m.len + m2.len,
                        .capture = blk: {
                            const c = sub_state.capture.?;
                            const head = c.toResult(arena);
                            head.next = MatchResult.Capture.connect(
                                arena,
                                m.capture,
                                m2.capture,
                            );
                            break :blk head;
                        },
                    };
                }
            },
            else => unreachable,
        }

        return null;
    }

    fn matchConcatWrapped(
        alloc: Allocator,
        m: MatchResult,
        state: MatchState,
        args: []const RE,
        dest_result: *?MatchResult,
    ) void {
        if (matchConcat(alloc, state, args)) |m2| {
            dest_result.* = .{
                .pos = m.pos,
                .len = m.len + m2.len,
                .capture = m2.capture,
            };
        }
    }

    pub fn search(self: Self, allocator: Allocator, input: []const u8) std.Thread.SpawnError!?MatchResult {
        var pos: usize = 0;
        if (getLiteralPrefix(&self)) |prefix| {
            if (std.mem.indexOf(u8, input, prefix)) |i| pos = i;
        }

        var thread_pool: std.Thread.Pool = undefined;
        try thread_pool.init(.{
            .allocator = allocator,
        });
        defer thread_pool.deinit();

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        var running: std.atomic.Value(bool) = .init(true);
        const state: MatchState = .{
            .running = &running,
            .input = .{ .value = input, .pos = pos },
            .capture = null,
            .arena = &arena,
            .thread_pool = &thread_pool,
        };

        var result = self.match(allocator, state);
        running.store(true, .unordered);

        if (result) |*m| if (m.capture) |c| {
            m.capture = c.dupeAll(allocator);
        };

        return result;
    }

    const SearchIterator = struct {
        re: RE,
        prefix: ?[]const u8,
        state: MatchState,

        _last_pos: usize = 0,
        _last_len: usize = 0,

        fn init(
            re: RE,
            allocator: Allocator,
            source: []const u8,
        ) !SearchIterator {
            const running = tryAlloc(allocator.create(std.atomic.Value(bool)));

            const thread_pool = tryAlloc(allocator.create(std.Thread.Pool));
            try thread_pool.init(.{
                .allocator = allocator,
            });

            const arena = tryAlloc(allocator.create(std.heap.ArenaAllocator));
            arena.* = .init(allocator);

            return .{
                .re = re,
                .prefix = getLiteralPrefix(re),
                .state = .{
                    .input = .{
                        .value = source,
                        .pos = 0,
                    },
                    .capture = null,
                    .arena = arena,
                    .running = running,
                    .thread_pool = thread_pool,
                },
            };
        }

        fn deinit(self: *@This(), allocator: Allocator) void {
            allocator.destroy(self.state.running);

            self.state.arena.deinit();
            allocator.destroy(self.state.arena);

            self.state.thread_pool.deinit();
            allocator.destroy(self.state.thread_pool);
        }

        // Caller should result.freeCaptures() if any capture was done
        fn next(self: *@This(), allocator: Allocator) ?MatchResult {
            _ = self.state.arena.reset(.retain_capacity);

            const re = self.re;

            var input = &self.state.input;
            while (input.hasMore()) {
                if (self.prefix) |prefix| {
                    const pos = std.mem.indexOfPos(u8, input.value, input.pos, prefix) orelse {
                        input.pos = input.value.len; // set to end
                        return null;
                    };
                    input.pos = pos;
                }

                self.state.running.store(true, .unordered);
                defer self.state.running.store(false, .unordered);

                if (re.match(allocator, self.state)) |m| {
                    if (m.pos == self._last_pos and
                        m.len == self._last_pos)
                    {
                        // match is the same as before, so it's not moving anymore
                        // this could happen if regex is an empty string,
                        // so just break and skip to the end
                        // to avoid infinite loop
                        input.pos = input.value.len;
                        m.freeCaptures(allocator);
                        return null;
                    }

                    self._last_pos = m.pos;
                    self._last_len = m.len;
                    self.state.input = self.state.input.slice(m.len);
                    return m;
                }

                input.pos += 1;
            }

            return null;
        }
    };

    pub fn searchAll(self: RE, allocator: Allocator, source: []const u8) !SearchIterator {
        return SearchIterator.init(self, allocator, source);
    }

    fn getLiteralPrefix(self: RE) ?[]const u8 {
        return switch (self.*) {
            .literal_string => |val| val,
            .concatenation => |args| {
                // check only first arg
                if (args.len > 0) return getLiteralPrefix(args[0]);
                return null;
            },
            .captured => |re| getLiteralPrefix(re),
            .repetition => |val| {
                if (val.min == 0) return null;
                if (getLiteralPrefix(val.re)) |prefix| return prefix;
                return null;
            },
            .alternation => null,
            else => null,
        };
    }

    fn replaceAll(
        self: Self,
        input: std.io.AnyWriter,
        output: std.io.AnyWriter,
    ) void {
        _ = self;
        _ = input;
        _ = output;
    }

    inline fn normalize(re: RE, allocator: Allocator) RE {
        return Simplifier.normalize(re, allocator);
    }

    pub fn format(self: RE, w: *std.io.Writer) std.io.Writer.Error!void {
        return Formatter.write(self, w, .{ .level = 0 });
    }

    fn formatln(re: RE, w: *std.Io.Writer) !void {
        try Formatter.write(re, w, .{ .level = 0 });
        try w.writeByte('\n');
    }
};

const Simplifier = struct {
    // Simplification rules:
    //
    //   note:
    //     * for brevity, "foo" is same a literal("foo")
    //     * unquoted vars a, b, c, x, y, z etc. are any regex expressions
    //     * -> means "simplifies to"
    //
    // - concat() -> ""
    // - concat(x) -> x
    // - concat(x, none, y) -> none
    // - concat("a", "b", "c") -> "abc"
    // - concat("a", "b", x, "c", "d") -> concat("ab", x, "cd")
    // - concat(concat(a,b,c), concat(x,y,z)) -> concat(a, b, c, x, y, z)
    //
    // - either() -> none
    // - either(none, x, y, z) -> either(x, y, z)
    // - either(x) -> x
    // - either(either(a,b), either(x), y) -> either(a, b, x, y)
    // - either("fooA", "fooB", "fooC", "foo") -> concat("foo", either("A", "B", "C", ""))
    // - either(a, a, b, b, b, c) -> either(a, b, c)
    //
    // - concat(either("12", "34"), either("qq", "ww"))
    //      -> either("12qq", "12ww", "34qq", "34ww")
    //
    // - charset('x') -> "x"
    // - charset() -> none
    // - either(charset('a', 'b'), charset('c', 'd'))
    //      -> charset('a', 'b', 'c', 'd')
    //      -> either('a', 'b', 'c', 'd') // applies only for small charsets
    //
    //   note:
    //     - zeroOrMore(x) == repeat(0, null, x)
    //     - atEleast(n, x) == repeat(n, null, x)
    //     - times(n, x) == repeat(n, n, x)
    //
    // - repeat(m, n, none) -> none
    // - times(2, "a") -> "aa"
    // - atLeast(2, "foo") -> concat("foofoo", zeroOrMore("foo"))
    // - atLeast(3, x) -> concat(x, x, x, zeroOrMore(x))

    // `normalize` simplifies `re` such that (1) it removes
    // unneeded nodes (2) combines literals if possible and
    // (3) makes it easier to get a literal prefix.
    // This step is optional. `re` and `normalize(re)` are
    // (or at least should be) equivalent
    // when it comes to matching the same input strings.
    //
    // note: caller must recursively free the returned pointer afterwards,
    // either manually or use re.recursiveFree(allocator)
    fn normalize(re: RE, allocator: Allocator) RE {
        switch (re.*) {
            .alternation => |args| {
                var buf: ArrayList(RE) = tryAlloc(
                    ArrayList(RE).initCapacity(allocator, args.len),
                );
                defer buf.deinit(allocator);

                for (args) |arg| {
                    var item = normalize(arg, allocator);
                    switch (item.*) {
                        .concatenation => {
                            if (distributeConcatOverAlt(item, allocator)) |alt| {
                                item.recursiveFree(allocator);
                                item = alt;
                            }
                        },
                        else => {},
                    }

                    switch (item.*) {
                        .none => {
                            // skip none
                            item.recursiveFree(allocator);
                        },
                        .alternation => |choices| {
                            // flatten nested either:
                            // either(either(a,b), either(c,d)) -> either(a,b,c,d)
                            for (choices) |choice|
                                tryAlloc(buf.append(allocator, choice));
                            allocator.free(choices);
                            allocator.destroy(item);
                        },
                        else => tryAlloc(buf.append(allocator, item)),
                    }
                }

                if (buf.items.len == 0) {
                    const result: RE = &.none;
                    return result.dupe(allocator);
                }

                if (buf.items.len == 1) {
                    return buf.items[0];
                }

                const items = tryAlloc(buf.toOwnedSlice(allocator));
                defer {
                    for (items) |item| {
                        item.recursiveFree(allocator);
                    }
                    allocator.free(items);
                }

                const common_prefix = getCommonPrefix(allocator, items) orelse {
                    return Regex.either(items).recursiveCopy(allocator);
                };

                var added: std.StringHashMapUnmanaged(void) = .empty;
                defer added.deinit(allocator);

                var either_args: ArrayList(RE) = tryAlloc(
                    ArrayList(RE).initCapacity(allocator, items.len),
                );
                defer either_args.deinit(allocator);

                for (items) |item| {
                    const i = common_prefix.len;
                    switch (item.*) {
                        .literal_string => |s| {
                            const str = s[i..];
                            if (added.contains(str)) continue;

                            tryAlloc(added.put(allocator, str, {}));

                            const str_copy = tryAlloc(allocator.dupe(u8, str));
                            tryAlloc(
                                either_args.append(allocator, Regex.literal(str_copy).dupe(allocator)),
                            );
                        },
                        .concatenation => |cargs| {
                            // if getCommonPrefix() above does not return null,
                            // then a concat is guaranteed to have a first literal arg here
                            const str = cargs[0].literal_string[i..];
                            if (added.contains(str)) continue;

                            tryAlloc(added.put(allocator, str, {}));

                            const new_cargs = tryAlloc(allocator.dupe(RE, cargs));
                            new_cargs[0] = &Regex.literal(str);
                            defer allocator.free(new_cargs);

                            tryAlloc(
                                either_args.append(allocator, Regex.concat(new_cargs).recursiveCopy(allocator)),
                            );
                        },
                        else => unreachable,
                    }
                }

                if (either_args.items.len == 0) {
                    return Regex.literal(common_prefix).dupe(allocator);
                }
                if (either_args.items.len == 1) {
                    switch (either_args.items[0].*) {
                        .literal_string => |s| {
                            if (s.len == 0) {
                                allocator.free(s);
                                allocator.destroy(either_args.items[0]);
                                return Regex.literal(common_prefix).dupe(allocator);
                            }
                        },
                        else => {},
                    }
                }

                const concat_args = &.{
                    Regex.literal(common_prefix).dupe(allocator),
                    Regex.either(
                        tryAlloc(either_args.toOwnedSlice(allocator)),
                    ).dupe(allocator),
                };

                return Regex.concat(
                    tryAlloc(allocator.dupe(RE, concat_args)),
                ).dupe(allocator);
            },

            .repetition => |val| {
                var copy = val;
                copy.re = normalize(val.re, allocator);

                switch (copy.re.*) {
                    // repeat(none) => none
                    .none => return copy.re,

                    .captured => |re2| {
                        defer allocator.destroy(copy.re);
                        return Regex.capture(
                            Regex.dupe(&.{ .repetition = .{
                                .re = re2,
                                .min = val.min,
                                .max = val.max,
                                .greedy = val.greedy,
                            } }, allocator),
                        ).dupe(allocator);
                    },

                    .repetition => |val2| {
                        if (val.greedy != val2.greedy) {
                            const result: RE = &.{ .repetition = copy };
                            return result.dupe(allocator);
                        }

                        const min = val.min * val2.min;
                        const max = switch (val.max != null and val2.max != null) {
                            true => val.max.? * val2.max.?,
                            else => null,
                        };

                        defer copy.re.recursiveFree(allocator);

                        return normalize(
                            &Regex{ .repetition = .{
                                .re = val2.re,
                                .min = min,
                                .max = max,
                                .greedy = val2.greedy,
                            } },
                            allocator,
                        );
                    },

                    .literal_string => {
                        // repetition(literal("")) == literal("")
                        const is_zero = if (val.max) |max| val.min == max else false;
                        if (copy.re.literal_string.len == 0 or is_zero) {
                            copy.re.recursiveFree(allocator);
                            return Regex.literal("").recursiveCopy(allocator);
                        }

                        if (val.max) |max| if (val.min == max and max == 1)
                            return copy.re;

                        if (val.min == 0) {
                            // can't create a prefix if min is zero so return as it is
                            const result: RE = &.{ .repetition = copy };
                            return result.dupe(allocator);
                        }

                        // TODO: check first if the string to be created is too large
                        const str = repeatString(allocator, val.min, copy.re.literal_string);

                        return Regex.concat(tryAlloc(allocator.dupe(RE, &.{
                            Regex.literal(str).dupe(allocator),
                            Regex.zeroOrMore(copy.re).dupe(allocator),
                        }))).dupe(allocator);
                    },
                    else => {
                        if (val.max) |max| if (val.min == max and max == 1)
                            return copy.re;

                        if (val.min == 0) {
                            // can't create a prefix if min is zero so return as it is
                            const result: RE = &.{ .repetition = copy };
                            return result.dupe(allocator);
                        }

                        var args: ArrayList(RE) = tryAlloc(
                            ArrayList(RE).initCapacity(allocator, val.min + 1),
                        );
                        defer args.deinit(allocator);

                        for (0..val.min) |_| tryAlloc(
                            args.append(allocator, copy.re.recursiveCopy(allocator)),
                        );

                        if (val.max == null or val.max.? != val.min) {
                            const rep: RE = &.{
                                .repetition = .{
                                    .re = copy.re,
                                    .min = 0,
                                    .max = val.max,
                                    .greedy = val.greedy,
                                },
                            };
                            tryAlloc(
                                args.append(allocator, rep.dupe(allocator)),
                            );
                        } else copy.re.recursiveFree(allocator);

                        return Regex.concat(
                            tryAlloc(args.toOwnedSlice(allocator)),
                        ).dupe(allocator);
                    },
                }
            },

            .concatenation => |args| {
                if (args.len == 0) return Regex.literal("").dupe(allocator);

                const flattened = blk: {
                    var result: ArrayList(RE) = tryAlloc(
                        ArrayList(RE).initCapacity(allocator, args.len),
                    );
                    defer {
                        for (result.items) |item| {
                            item.recursiveFree(allocator);
                        }
                        result.deinit(allocator);
                    }

                    for (args) |arg| {
                        const item = normalize(arg, allocator);
                        switch (item.*) {
                            .none => return item,
                            .concatenation => |list| {
                                // flatten nested concats
                                for (list) |x| tryAlloc(result.append(allocator, x));
                                allocator.destroy(item);
                                allocator.free(list);
                            },
                            else => tryAlloc(result.append(allocator, item)),
                        }
                    }
                    break :blk tryAlloc(result.toOwnedSlice(allocator));
                };
                defer allocator.free(flattened);

                var i: usize = 0;
                var combined = tryAlloc(
                    ArrayList(RE).initCapacity(allocator, flattened.len),
                );
                defer combined.deinit(allocator);

                while (i < flattened.len) {
                    const current = flattened[i];
                    switch (current.*) {
                        .literal_string => {},
                        .none => {
                            // concat contains none, so it can't
                            // match anything now, so simplify to none
                            for (flattened) |item| item.recursiveFree(allocator);
                            const result: RE = &.none;
                            return result.dupe(allocator);
                        },
                        else => {
                            tryAlloc(combined.append(allocator, current));
                            i += 1;
                            continue;
                        },
                    }

                    var j: usize = i + 1;
                    while (j < flattened.len and flattened[j].* == .literal_string) {
                        j += 1;
                    }

                    const lit = concatLiterals(allocator, flattened[i..j]);

                    tryAlloc(combined.append(allocator, lit));

                    for (i..j) |index| {
                        const re2 = flattened[index];
                        allocator.free(re2.literal_string);
                        allocator.destroy(re2);
                    }

                    i = j;
                }

                if (combined.items.len == 1) {
                    const first = combined.items[0];
                    return first;
                }

                const new_args = tryAlloc(combined.toOwnedSlice(allocator));
                return Regex.concat(new_args).dupe(allocator);
            },

            .literal_string => |val| {
                const str = tryAlloc(allocator.dupe(u8, val));
                return Regex.literal(str).dupe(allocator);
            },

            .char_class_set => |set| {
                return switch (set.count()) {
                    0 => Regex.dupe(&.none, allocator),

                    // contains only one character, convert to literal
                    1 => {
                        const ch: u8 = @intCast(set.findFirstSet() orelse unreachable);
                        return Regex.literal(&.{ch}).recursiveCopy(allocator);
                    },

                    // set is small enough, convert to either()
                    // it would be a bit bigger, but also easier to
                    // combine with other regexes for simplification
                    2...16 => |len| {
                        var buf: ArrayList(RE) = tryAlloc(
                            ArrayList(RE).initCapacity(allocator, len),
                        );
                        defer buf.deinit(allocator);

                        var iter = set.iterator(.{});
                        while (iter.next()) |n| {
                            const ch: u8 = @intCast(n);
                            const lit = Regex.literal(&.{ch}).recursiveCopy(allocator);
                            tryAlloc(buf.append(allocator, lit));
                        }

                        const args = tryAlloc(buf.toOwnedSlice(allocator));
                        return Regex.either(args).dupe(allocator);
                    },
                    else => re.recursiveCopy(allocator),
                };
            },

            .captured => |val| {
                const arg = val.normalize(allocator);
                return Regex.capture(arg).dupe(allocator);
            },

            else => {
                // Since the caller is meant to free whatever is returned from normalize()
                // it makes sense to always copy and allocate, regardless of
                // whether re is changed or not.
                // Unless I could wrap the result to add a flag whether it should
                // be freed? That sounds more complicated though.
                return re.recursiveCopy(allocator);
            },
        }
    }

    fn getCommonPrefix(allocator: Allocator, args: []RE) ?[]const u8 {
        if (args.len == 0) return null;

        var buf: ArrayList([]const u8) = tryAlloc(
            ArrayList([]const u8).initCapacity(allocator, args.len),
        );
        defer buf.deinit(allocator);

        for (args) |arg| {
            switch (arg.*) {
                .literal_string => |s| tryAlloc(buf.append(allocator, s)),
                .concatenation => |concat_args| {
                    if (concat_args.len == 0)
                        tryAlloc(buf.append(allocator, ""))
                    else if (concat_args[0].* == .literal_string)
                        tryAlloc(buf.append(allocator, concat_args[0].literal_string))
                    else
                        return null;
                },
                else => return null,
            }
        }

        const first = buf.items[0];

        var i: usize = 0;
        if (first.len > 0) {
            loop: while (i < first.len) : (i += 1) {
                for (buf.items[1..]) |arg| {
                    if (i >= arg.len or first[i] != arg[i])
                        break :loop;
                }
            }
        }

        return switch (i > 0 or first.len == 0) {
            true => tryAlloc(allocator.dupe(u8, first[0..i])),
            false => null,
        };
    }

    // Transforms an expression like concat(either(a, b), either(c,d))
    // to either(concat(a,c), concat(b,d)). If a,b,c,d are all literals,
    // then the result is a flat either consisting only of literals.
    fn distributeConcatOverAlt(re: RE, allocator: Allocator) ?RE {
        if (re.* != .concatenation) return re.recursiveCopy(allocator);
        if (re.concatenation.len == 0) return re.recursiveCopy(allocator);

        var i: usize = 0;
        var accumulator: RE = re.concatenation[0].recursiveCopy(allocator);
        for (re.concatenation[1..]) |current| {
            const prev = accumulator;
            defer prev.recursiveFree(allocator);
            defer {
                i += 1;
            }

            switch (accumulator.*) {
                .literal_string => |prefix| {
                    switch (current.*) {
                        .literal_string => |suffix| {
                            const str = tryAlloc(
                                std.mem.concat(allocator, u8, &.{ prefix, suffix }),
                            );
                            accumulator = Regex.literal(str).dupe(allocator);
                        },
                        .alternation => |choices| {
                            var args: ArrayList(RE) = tryAlloc(
                                ArrayList(RE).initCapacity(allocator, choices.len),
                            );
                            defer args.deinit(allocator);

                            for (choices) |choice| {
                                const arg = distributeConcatOverAlt(&.concat(&.{
                                    &.literal(prefix),
                                    choice,
                                }), allocator) orelse {
                                    for (args.items) |arg| arg.recursiveFree(allocator);

                                    return null;
                                };
                                tryAlloc(args.append(allocator, arg));
                            }

                            accumulator = Regex.either(
                                tryAlloc(args.toOwnedSlice(allocator)),
                            ).dupe(allocator);
                        },
                        else => return null,
                    }
                },
                .alternation => |acc_choices| {
                    switch (current.*) {
                        .literal_string => |suffix| {
                            var args: ArrayList(RE) = tryAlloc(
                                ArrayList(RE).initCapacity(allocator, acc_choices.len),
                            );
                            defer args.deinit(allocator);

                            for (acc_choices) |choice| {
                                const arg = distributeConcatOverAlt(&.concat(&.{
                                    choice,
                                    &.literal(suffix),
                                }), allocator) orelse {
                                    for (args.items) |arg| arg.recursiveFree(allocator);
                                    return null;
                                };
                                tryAlloc(args.append(allocator, arg));
                            }

                            accumulator = Regex.either(
                                tryAlloc(args.toOwnedSlice(allocator)),
                            ).dupe(allocator);
                        },
                        .alternation => |choices| {
                            const len = acc_choices.len + choices.len;
                            var args: ArrayList(RE) = tryAlloc(
                                ArrayList(RE).initCapacity(allocator, len),
                            );
                            defer args.deinit(allocator);

                            for (acc_choices) |c1| {
                                for (choices) |c2| {
                                    const temp = distributeConcatOverAlt(&.concat(&.{
                                        c1,
                                        c2,
                                    }), allocator) orelse {
                                        for (args.items) |arg| arg.recursiveFree(allocator);
                                        return null;
                                    };
                                    tryAlloc(args.append(allocator, temp));
                                }
                            }

                            accumulator = Regex.either(
                                tryAlloc(args.toOwnedSlice(allocator)),
                            ).dupe(allocator);
                        },
                        else => return null,
                    }
                },
                else => return null,
            }
        }

        return accumulator;
    }

    fn concatLiterals(allocator: Allocator, args: []const RE) RE {
        var size: usize = 0;
        for (args) |arg| {
            switch (arg.*) {
                .literal_string => |val| size += val.len,
                else => unreachable,
            }
        }
        var str = tryAlloc(
            ArrayList(u8).initCapacity(allocator, size),
        );

        for (args) |arg| {
            tryAlloc(str.appendSlice(allocator, arg.literal_string));
        }

        const result = tryAlloc(str.toOwnedSlice(allocator));
        return Regex.literal(result).dupe(allocator);
    }

    // Caller should free the returned string
    fn repeatString(allocator: Allocator, count: usize, str: []const u8) []const u8 {
        const len = count * str.len;
        var buf = tryAlloc(ArrayList(u8).initCapacity(allocator, len));

        var i: usize = 0;
        while (i < len) : (i += str.len) {
            tryAlloc(buf.appendSlice(allocator, str));
        }
        return tryAlloc(buf.toOwnedSlice(allocator));
    }

    fn repeat(self: RE, allocator: Allocator, count: usize) []const RE {
        var buf = tryAlloc(ArrayList(RE).initCapacity(allocator, count));
        for (0..count) |_| {
            tryAlloc(buf.append(allocator, self.dupe(allocator)));
        }
        return tryAlloc(buf.toOwnedSlice(allocator));
    }
};

const Formatter = struct {
    inline fn writeIndent(w: *std.Io.Writer, level: usize) !void {
        for (0..level) |_| try w.writeAll("  ");
    }

    inline fn writeRangeChar(w: *std.Io.Writer, ch: u8) !void {
        switch (ch) {
            '0'...'9', 'A'...'Z', 'a'...'z' => try w.print("{c}", .{ch}),
            else => try w.print("[{x:02}]", .{ch}),
        }
    }

    fn write(re: RE, w: *std.Io.Writer, state: struct {
        level: usize,
        indent: bool = true,
    }) !void {
        if (state.indent) try writeIndent(w, state.level);

        switch (re.*) {
            .literal_string => |s| try w.print("\"{s}\"", .{s}),
            .repetition => |rep| {
                try w.print("rep[{d}..{c}]{s}(", .{
                    rep.min,
                    if (rep.max) |max| std.fmt.digitToChar(@intCast(max), .lower) else ' ',
                    if (rep.greedy) "" else "?",
                });
                try write(rep.re, w, .{ .level = state.level, .indent = false });
                try w.writeAll(")");
            },

            .captured => |val| {
                try w.writeAll("cap(");
                try write(val, w, .{ .level = state.level, .indent = false });
                try w.writeAll(")");
            },

            .back_reference => |val| try w.print("ref({d})", .{val}),

            inline .start,
            .end,
            .boundary,
            .any,
            .none,
            .word,
            .digit,
            .alphabet,
            .alphanum,
            .whitespace,
            .@"!boundary",
            .@"!word",
            .@"!digit",
            .@"!alphabet",
            .@"!alphanum",
            .@"!whitespace",
            => |_, t| {
                try w.writeAll(@tagName(t));
            },

            .char_class_set => |val| {
                try w.writeAll("set(");

                var i: usize = 0;
                while (i < 256) : (i += 1) {
                    if (!val.isSet(i)) {
                        continue;
                    }

                    try writeRangeChar(w, @intCast(i));

                    var j: usize = i + 1;
                    while (j < 256 and val.isSet(j)) j += 1;

                    if (j > i + 1) {
                        try w.writeByte('-');
                        try writeRangeChar(w, @intCast(j - 1));
                        i = j;
                    }
                }

                try w.writeAll(")");
            },

            inline .concatenation, .alternation => |args, t| {
                const tag = switch (t) {
                    .concatenation => "cat",
                    .alternation => "alt",
                    else => unreachable,
                };

                const all_literals = blk: {
                    for (args) |arg| {
                        switch (arg.*) {
                            .literal_string => {},
                            else => break :blk false,
                        }
                    }
                    break :blk true;
                };

                if (all_literals) {
                    try w.print("{s}(", .{tag});
                    for (args, 0..) |arg, i| {
                        try w.print("\"{s}\"", .{arg.literal_string});
                        if (i < args.len - 1)
                            try w.writeAll(", ");
                    }
                    try w.writeAll(")");
                } else {
                    try w.print("{s}(\n", .{tag});
                    for (args) |arg| {
                        try write(arg, w, .{ .level = state.level + 1 });
                        try w.writeAll(",\n");
                    }
                    if (state.indent) try writeIndent(w, state.level);
                    try w.writeAll(")");
                }
            },
        }
        try w.flush();
    }
};

const MatchIterator = union(enum) {
    greedy: GreedyIterator,
    lazy: LazyIterator,
    alternation: AlternationIterator,
    single: SingleIterator,

    const Self = @This();

    fn init(
        re: RE,
        state: MatchState,
    ) Self {
        return switch (re.*) {
            .repetition => |val| {
                return if (val.greedy) .{
                    .greedy = GreedyIterator{
                        .re = val.re,
                        .state = state,
                        .min = val.min,
                        .max = val.max,
                    },
                } else .{
                    .lazy = LazyIterator{
                        .re = val.re,
                        .state = state,
                        .min = val.min,
                        .max = val.max,
                    },
                };
            },
            .alternation => |args| .{
                .alternation = .{
                    .choices = args,
                    .state = state,
                },
            },
            else => .{
                .single = .{
                    .re = re,
                    .state = state,
                },
            },
        };
    }

    fn deinit(self: *MatchIterator, allocator: Allocator) void {
        return switch (self.*) {
            inline else => |*iter| iter.deinit(allocator),
        };
    }

    fn next(self: *MatchIterator, allocator: Allocator) ?MatchResult {
        return switch (self.*) {
            inline else => |*iter| iter.next(allocator),
        };
    }

    const LazyIterator = struct {
        re: RE,
        state: MatchState,
        pos: usize = 0,
        len: usize = 0,
        iterations: usize = 0,
        min: usize,
        max: ?usize,
        first: bool = true,

        fn deinit(_: *@This(), _: Allocator) void {}

        fn next(self: *@This(), allocator: Allocator) ?MatchResult {
            const state = self.state;

            if (self.first) {
                self.first = false;

                // skip first few matches until min
                while (self.iterations < self.min) {
                    if (self.re.match(allocator, state)) |m| {
                        self.pos = m.pos;
                        self.len = m.len;
                        self.iterations += 1;
                    } else return null;
                }

                return .{ .pos = self.pos, .len = self.len };
            }

            const max = self.max orelse std.math.maxInt(usize) - 1;

            if (self.iterations <= max) {
                const sub_state = state.sliceInput(self.len);
                if (self.re.match(allocator, sub_state)) |m| {
                    self.len += m.len;
                    self.iterations += 1;
                    return .{ .pos = self.pos, .len = self.len };
                }
            }

            // match zero string
            if (self.iterations <= max + 1) {
                self.iterations += 1;
                self.pos = state.input.pos;
                self.len = 0;
                return .{ .pos = self.pos, .len = self.len };
            }

            return null;
        }
    };

    const GreedyIterator = struct {
        re: RE,
        state: MatchState,
        pos: usize = 0,
        len: usize = 0,
        min: usize,
        max: ?usize = null,
        first: bool = true,
        lengths: std.ArrayListUnmanaged(usize) = .{},

        fn deinit(self: *@This(), allocator: Allocator) void {
            self.lengths.deinit(allocator);
        }

        fn next(self: *@This(), allocator: Allocator) ?MatchResult {
            const state = self.state;

            if (self.first) {
                self.first = false;

                var n: usize = 0;
                var lengths = &self.lengths;

                // skip first few matches until min
                while (n < self.min) {
                    if (self.re.match(allocator, state)) |m| {
                        self.pos = m.pos;
                        n += 1;
                    } else return null;
                }

                if (n == 0) tryAlloc(lengths.append(allocator, 0));

                while (true) {
                    if (self.max) |max| {
                        if (lengths.items.len >= max) break;
                    }
                    if (self.re.match(allocator, state.sliceInput(self.len))) |m| {
                        self.len += m.len;
                        if (lengths.items.len == 0) self.pos = m.pos;
                        tryAlloc(lengths.append(allocator, self.len));
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

    const AlternationIterator = struct {
        choices: []const RE,
        state: MatchState,
        index: usize = 0,

        fn deinit(_: *@This(), _: Allocator) void {}

        fn next(self: *@This(), allocator: Allocator) ?MatchResult {
            while (self.index < self.choices.len) {
                defer self.index += 1;
                const re = self.choices[self.index];

                if (re.match(allocator, self.state)) |m| {
                    return m;
                }
            }

            return null;
        }
    };

    const SingleIterator = struct {
        re: RE,
        state: MatchState,
        first: bool = true,

        fn deinit(_: *@This(), _: Allocator) void {}

        fn next(self: *@This(), allocator: Allocator) ?MatchResult {
            if (self.first) {
                self.first = false;
                return self.re.match(allocator, self.state);
            }
            return null;
        }
    };
};

const testing = std.testing;

test {
    const TestItem = struct {
        re: RE,
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
                &.eitherChars("abcx"),
                &.eitherChars("xyzx"),
                &.eitherChars("zooz"),
                &.exceptChar("abcdef"),
            }),
            .input = "xyz1abcdefg",
            .expected = "xyz1",
        },
        .{
            .re = &.concat(&.{
                &.exceptChar("a"),
                &.exceptChar("a"),
                &.exceptChar("a"),
                &.exceptChar("a"),
                &.eitherChars("a"),
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
        .{
            .re = &.concat(&.{
                &.oneOrMore(&.literal("abc")),
                &.either(&.{
                    &.literal("xyz"),
                    &.literal("qw"),
                    &.literal("ert"),
                }),
                &.either(&.{
                    &.literal("g"),
                    &.literal("q"),
                }),
            }),
            .input = "81293abcabcxyzqq",
            .expected = "abcabcxyzq",
        },
        .{
            .re = &.concat(&.{
                &.oneOrMore(&.literal("abc")),
                &.either(&.{
                    &.literal("xyz"),
                    &.literal("qw"),
                    &.literal("ert"),
                }),
                &.either(&.{
                    &.literal("aa"),
                    &.literal("bb"),
                    &.literal("cc"),
                }),
            }),
            .input = "898493abcabcertaa1298312",
            .expected = "abcabcertaa",
        },
        .{
            .re = &.concat(&.{
                &.either(&.{
                    &.literal("aa"),
                    &.literal("aaa"),
                    &.literal("a"),
                }),
                &.literal("aab"),
            }),
            .input = "aaab",
            .expected = "aaab",
        },
    };

    for (tests) |item| {
        const allocator = testing.allocator;
        const re = item.re.normalize(allocator);
        defer re.recursiveFree(allocator);

        const result = try re.search(allocator, item.input);
        defer if (result) |m| m.freeCaptures(allocator);

        errdefer {
            std.debug.print("input: {s}, expected: {s}, got: ", .{ item.input, item.expected orelse "<null>" });
            if (result) |m| {
                std.debug.print("{s}\n", .{m.string(item.input)});
            } else {
                std.debug.print("<null>\n", .{});
            }
            std.debug.print("re: {f}\nnormalized: {f}\n", .{ item.re, re });
        }

        if (item.expected) |expected| {
            try testing.expect(result != null);
            try testing.expectEqualSlices(u8, expected, result.?.string(item.input));
        } else {
            try testing.expect(result == null);
        }
    }
}

test "equality" {
    const allocator = testing.allocator;

    const re0: RE = &.literal("abc");
    try testing.expect(re0.equals(re0));
    try testing.expect(re0.equals(&.literal("abc")));
    try testing.expect(!re0.equals(&.literal("xyz")));

    const re1: RE = &.concat(&.{
        &.literal("aaaa"),
        &.either(&.{
            &.literal("bbbb"),
            &.capture(&.zeroOrMore(&.any)),
        }),
        &.any,
    });
    const re2 = re1.recursiveCopy(testing.allocator);
    defer re2.recursiveFree(allocator);

    try testing.expect(re1.equals(re1));
    try testing.expect(re1.equals(re2));
    try testing.expect(re2.equals(re2));
    try testing.expectEqualDeep(re1, re1);
    try testing.expectEqualDeep(re1, re2);
    try testing.expectEqualDeep(re2, re2);
    try testing.expect(!re0.equals(re1));
}

test "normalize concat 1" {
    const re: RE = &.concat(&.{
        &.literal("abc"),
        &.literal("def"),
        &.concat(&.{
            &.literal("ghi"),
        }),
        &.concat(&.{
            &.concat(&.{
                &.literal("jkl"),
            }),
        }),
    });

    const allocator = testing.allocator;
    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.literal("abcdefghijkl");
    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "normalize concat 2" {
    const re: RE = &.concat(&.{
        &.literal("abc"),
        &.literal("def"),
        &.any,
        &.literal("ghi"),
        &.literal("jkl"),
    });

    const allocator = testing.allocator;
    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.concat(&.{
        &.literal("abcdef"),
        &.any,
        &.literal("ghijkl"),
    });
    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "normalize concat 3" {
    const re: RE = &.concat(&.{
        &.literal("abc"),
        &.literal("def"),
        &.none,
    });

    const allocator = testing.allocator;
    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.none;
    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "repetition 1" {
    const allocator = testing.allocator;
    const re: RE = &.atLeast(2, &.literal("abc"));

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.concat(&.{
        &.literal("abcabc"),
        &.zeroOrMore(&.literal("abc")),
    });
    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "repetition 2" {
    const allocator = testing.allocator;
    const re: RE = &.zeroOrMore(&.literal("abc"));

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    try testing.expectEqualDeep(re, actual);
    try testing.expect(re.equals(actual));
}

test "repetition 3" {
    const allocator = testing.allocator;
    const re: RE = &.around(0, 3, &.around(0, 7, &.any));

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.around(0, 3 * 7, &.any);

    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "repetition of empty literal" {
    const allocator = testing.allocator;
    const re: RE = &.zeroOrMore(&.oneOrMore(&.literal("")));

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.literal("");
    try testing.expectEqualDeep(expected, actual);
}

test "alternation 1" {
    const allocator = testing.allocator;
    const re: RE = &.either(&.{
        &.literal("aabcdef"),
        &.literal("aaaaa"),
        &.literal("aaaxyz"),
    });
    const expected: RE = &.concat(&.{
        &.literal("aa"),
        &.either(&.{
            &.literal("bcdef"),
            &.literal("aaa"),
            &.literal("axyz"),
        }),
    });

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}
test "alternation 2" {
    const allocator = testing.allocator;
    const re: RE = &.either(&.{
        &.literal("aabcdef"),
        &.literal("aaaaa"),
        &.literal("aa"),
    });
    const expected: RE = &.concat(&.{
        &.literal("aa"),
        &.either(&.{
            &.literal("bcdef"),
            &.literal("aaa"),
            &.literal(""),
        }),
    });

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "alternation 3" {
    const allocator = testing.allocator;
    // x|(y|z)|w
    // (aabcdex|aaaaa)|aa
    // aa+(bcdex|aaa)

    // dd+aa+(bb|cc)+(ee|ff)
    // dd+(aabb|aacc)+(ee|ff)
    // dd+([aabb|aacc]+ee|[aabb|aacc]+ff)
    // dd+([aabbee|aaccee]|[aabbff|aaccff])
    // dd+(aabbee|aaccee|abbff|aaccff) // by associativy, brackets can go poof
    // ddaabbee|ddaaccee|ddabbff|ddaaccff
    // what the hell, this is neat
    // I could repeated apply distributivity
    // then later apply normalization to get a possible prefix
    // easier said than done though
    // I could simplify the expression by glancing
    // but how do I express that into a dumb algorithm
    // at least without resorting to brute-force tree search
    // maybe I could apply the simplification as a post-step
    // after normalization, I think the result in the end is the same

    // foo+[a-c]

    // this is a theoretical case though
    // in practice there would be a mix of literals and character sets
    // well, characters sets are just alternations with one-length strings
    // so it still applies
    // what about repetition though, how would that fit into distribution
    // I could always replace max len of repetition
    // with the length of the string
    // yeah that would work, sounds terrible but might actually work
    // or I could break regex compatibility or remove things
    // like back-references
    // so I could achieve mathemtical nirvana

    // (aa+(bcdex|aaa))|aa
    // (aabcdex|aaaaa)|aa
    // aha, what this means is that I could avoid
    // doing flattening before normalization
    // by applying the distribution above
    // pre-flattening wouldn't work anyway
    // if I already have the above as the argument

    // (aa+(bcdex|aaa))|aa
    // aa+bcdex|aaa|""

    // in other words, concat distributes over alternation
    // formally speaking, what kind of algebra is regex?
    // alternation is also associative and commutative
    // concatenation is associative only
    // if regex is a proper algebra, I could look up
    // a more general simplification process
    // actually, I should just read that sipser book
    // or not, forget looking things up,
    // too much math gobbledygook
    // not really as fun as figuring things out on my own

    // concat(either("aa", "bb"), either("cc", "dd"))
    // (aa|bb)+(cc|dd)
    // ([aa+(cc|dd)]|[bb+(cc|dd)])
    // (aacc|aadd|bbcc|bbdd)

    // either(either("aabcdex", "aaaaa"), "aa")
    // either(concat("aa", either("bcdex", "aaa")), "aa")

    // either(either("aabcdex", "aaaaa"), "aa")
    // either("aabcdex", "aaaaa", "aa")
    // either("aabcdex", "aaaaa", "aa")
    // concat("aa", either("bcdex", "aaa", ""))
    const re: RE = &.either(&.{
        &.either(&.{
            &.literal("aabcdex"),
            &.literal("aaaaa"),
        }),
        &.literal("aa"),
    });

    const expected: RE = &.concat(&.{
        &.literal("aa"),
        &.either(&.{
            &.literal("bcdex"),
            &.literal("aaa"),
            &.literal(""),
        }),
    });

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "alternation 4" {
    const allocator = testing.allocator;

    const re: RE = &.either(&.{
        &.either(&.{
            &.literal("abc"),
            &.concat(&.{
                &.literal("abd"),
                &.any,
            }),
            &.literal("abb"),
        }),
    });

    const expected: RE = &.concat(&.{
        &.literal("ab"),
        &.either(&.{
            &.literal("c"),
            &.concat(&.{
                &.literal("d"),
                &.any,
            }),
            &.literal("b"),
        }),
    });

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "alternation empty" {
    const allocator = testing.allocator;
    const re: RE = &.either(&.{
        &.either(&.{}),
    });

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.none;
    try testing.expectEqualDeep(expected, actual);
}

test "alternation one literal" {
    const allocator = testing.allocator;
    const re: RE = &.either(&.{&.literal("x")});

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.literal("x");
    try testing.expectEqualDeep(expected, actual);
}

test "alternation similar" {
    const allocator = testing.allocator;
    const re: RE = &.either(&.{
        &.literal("x"),
        &.literal("x"),
        &.literal("x"),
    });

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.literal("x");
    try testing.expectEqualDeep(expected, actual);
}

test "alternation no simplify" {
    const allocator = testing.allocator;
    const re: RE = &.either(&.{
        &.literal("x"),
        &.any,
    });

    const actual = re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: RE = &.either(&.{
        &.literal("x"),
        &.any,
    });
    try testing.expectEqualDeep(expected, actual);
}

test "distribution" {
    const allocator = testing.allocator;
    const re: RE = &.concat(&.{
        &.either(&.{ &.literal("a"), &.literal("b") }),
        &.literal("x"),
    });

    const actual_opt = Simplifier.distributeConcatOverAlt(re, allocator);
    if (actual_opt) |actual| {
        defer actual.recursiveFree(allocator);

        const expected: RE = &.either(&.{
            &.literal("ax"),
            &.literal("bx"),
        });
        try testing.expectEqualDeep(expected, actual);
    }
}

test "distribution 2" {
    const allocator = testing.allocator;
    const re: RE = &.concat(&.{
        &.either(&.{ &.literal("a"), &.literal("b") }),
        &.literal("x"),
        &.either(&.{ &.literal("c"), &.literal("d") }),
    });

    const actual_opt = Simplifier.distributeConcatOverAlt(re, allocator);
    if (actual_opt) |actual| {
        defer actual.recursiveFree(allocator);

        const expected: RE = &.either(&.{
            &.literal("axc"),
            &.literal("axd"),
            &.literal("bxc"),
            &.literal("bxd"),
        });
        try testing.expectEqualDeep(expected, actual);
    }
}

test "format" {
    const t = std.testing;
    const gpa = std.testing.allocator;
    var dest: std.io.Writer.Allocating = .init(gpa);
    defer dest.deinit();

    var w = &dest.writer;

    try Regex.literal("aaa").formatln(w);
    try t.expectEqualStrings(
        \\"aaa"
        \\
    ,
        w.buffered(),
    );
    dest.clearRetainingCapacity();

    try Regex.concat(&.{
        &.literal("aaa"),
        &.literal("bbb"),
        &.literal("ccc"),
    }).formatln(w);
    try t.expectEqualStrings(
        \\cat("aaa", "bbb", "ccc")
        \\
    ,
        w.buffered(),
    );
    dest.clearRetainingCapacity();

    try Regex.concat(&.{
        &.literal("aaa"),
        &.literal("bbb"),
        &.either(&.{
            &.literal("ccc"),
            &.literal("eee"),
        }),
    }).formatln(w);
    try t.expectEqualStrings(
        \\cat(
        \\  "aaa",
        \\  "bbb",
        \\  alt("ccc", "eee"),
        \\)
        \\
    ,
        w.buffered(),
    );
    dest.clearRetainingCapacity();

    try Regex.zeroOrMore(
        &.literal("aaa"),
    ).formatln(w);
    try t.expectEqualStrings(
        \\rep[0.. ]("aaa")
        \\
    ,
        w.buffered(),
    );
    dest.clearRetainingCapacity();

    try Regex.@"around?"(
        1,
        5,
        &.concat(&.{
            &.capture(&.literal("aaa")),
            &.backref(1),
            &.literal("bbb"),
            &.either(&.{
                &.literal("ccc"),
                &.literal("eee"),
            }),
        }),
    ).formatln(w);
    try t.expectEqualStrings(
        \\rep[1..5]?(cat(
        \\  cap("aaa"),
        \\  ref(1),
        \\  "bbb",
        \\  alt("ccc", "eee"),
        \\))
        \\
    ,
        w.buffered(),
    );
    dest.clearRetainingCapacity();

    try Regex.concat(&.{
        &.any,
        &.start,
        &.end,
        &.boundary,
        &.@"!word",
    }).formatln(w);
    try t.expectEqualStrings(
        \\cat(
        \\  any,
        \\  start,
        \\  end,
        \\  boundary,
        \\  !word,
        \\)
        \\
    ,
        w.buffered(),
    );
    dest.clearRetainingCapacity();

    try Regex.range('a', 'z').formatln(w);
    try t.expectEqualStrings(
        \\set(a-z)
        \\
    ,
        w.buffered(),
    );
    dest.clearRetainingCapacity();
    //set(0–9)

    const cc = CharClass;
    var set = cc.alphabet.unionWith(cc.digit);
    set.set(0);
    set.set(1);
    set.set(2);
    set.set(' ');
    set.set('.');
    set.set(',');
    try Regex.charset(set).formatln(w);
    try t.expectEqualStrings(
        \\set([00]-[02][20][2c][2e]0-9A-Za-z)
        \\
    ,
        w.buffered(),
    );
    dest.clearRetainingCapacity();
}

test "nested captures" {
    const allocator = testing.allocator;
    const re: RE = &.capture(&.concat(&.{
        &.capture(&.literal("a")),
        &.capture(&.literal("a")),
        &.concat(&.{
            &.capture(&.literal("bc")),
        }),
    }));

    const source = "aabcabc   aaabcasdjfabciasofaaaabca";

    const expected_match = "aabc";
    const expected_captures: []const []const u8 = &.{
        "aabc",
        "a",
        "a",
        "bc",
    };

    const result = try re.search(allocator, source);
    try testing.expect(result != null);
    const m = result orelse unreachable;
    defer m.freeCaptures(allocator);

    try testing.expectEqualStrings(expected_match, m.string(source));
    try testing.expect(m.capture != null);

    var i: usize = 0;
    var iter = m.capture;
    while (iter) |c| {
        try testing.expectEqualStrings(expected_captures[i], c.string(source));
        iter = c.next;
        i += 1;
    }
}

test "match captured iterator" {
    const allocator = testing.allocator;
    const Expected = struct {
        match: []const u8,
        captures: []const []const u8,
    };
    const expected: []const Expected = &.{
        .{
            .match = "aabc",
            .captures = &.{ "aa", "bc" },
        },
        .{
            .match = "abc",
            .captures = &.{ "a", "bc" },
        },
        .{
            .match = "aaabc",
            .captures = &.{ "aaa", "bc" },
        },
        .{
            .match = "abc",
            .captures = &.{ "a", "bc" },
        },
        .{
            .match = "aaaabc",
            .captures = &.{ "aaaa", "bc" },
        },
    };
    const re: RE = &.concat(&.{
        &.capture(
            &.oneOrMore(&.literal("a")),
        ),
        &.capture(&.literal("bc")),
    });
    const source = "aabcabc   aaabcasdjfabciasofaaaabca";
    var iterator = try re.searchAll(allocator, source);
    defer iterator.deinit(allocator);

    var i: usize = 0;
    while (iterator.next(testing.allocator)) |m| {
        defer i += 1;
        try testing.expect(i < expected.len);
        try testing.expectEqualStrings(expected[i].match, m.string(source));
        try testing.expect(m.capture != null);

        const captures = m.capture orelse unreachable;
        const slice = captures.toSlice(allocator);
        defer allocator.free(slice);

        try testing.expectEqual(expected[i].captures.len, slice.len);

        var j: usize = 0;
        for (slice) |c| {
            try testing.expectEqualStrings(expected[i].captures[j], c.string(source));
            j += 1;
        }
    }
}
