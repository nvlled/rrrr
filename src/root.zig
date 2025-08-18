const std = @import("std");
const BitSet = std.bit_set.IntegerBitSet(256);
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;

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
        capture: ?Capture,
        input: Input,

        fn getCapture(self: @This(), index: usize) ?*const Capture {
            var current: ?*const Capture = &(self.capture orelse return null);
            while (true) {
                if (current) |cap| {
                    if (cap.index == index) return cap;
                    current = cap.parent;
                } else return null;
            }
        }

        fn addCapture(self: @This(), value: []const u8) SearchState {
            return .{
                .input = self.input,
                .capture = .{
                    .value = value,
                    .index = if (self.capture) |c| c.index + 1,
                    .parent = self.capture,
                },
            };
        }

        fn sliceInput(self: @This(), offset: usize) SearchState {
            return .{
                .capture = self.capture,
                .input = self.input.slice(offset),
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

        fn slice(self: @This(), offset: usize) Input {
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

    const MatchIterator = struct {
        re: RE,
        prefix: ?[]const u8,
        state: SearchState,

        fn init(re: Regex.RE, source: []const u8) MatchIterator {
            return .{
                .re = re,
                .prefix = getLiteralPrefix(re),
                .state = .{
                    .input = .{
                        .value = source,
                        .pos = 0,
                    },
                    .capture = null,
                },
            };
        }

        fn next(self: *@This(), allocator: Allocator) Allocator.Error!?Match {
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

                if (try re.match(allocator, self.state)) |m| {
                    self.state.input = self.state.input.slice(m.len);
                    return m;
                }
                input.pos += 1;
            }

            return null;
        }
    };

    fn getLiteralPrefix(self: RE) ?[]const u8 {
        // TODO: an empty string is returned to stop searching
        // since returning null would proceed to search the
        // rest of the re tree. There's probably a better way to do that.
        return switch (self.*) {
            ._literal => |val| val,
            ._concat => |args| {
                for (args) |re| {
                    if (getLiteralPrefix(re)) |prefix| return prefix;
                }
                return null;
            },
            ._capture => |re| getLiteralPrefix(re),
            ._repetition => |val| {
                if (val.min == 0) return "";
                if (getLiteralPrefix(val.re)) |prefix| return prefix;
                return null;
            },
            ._alt => "",
            else => null,
        };
    }

    fn search(self: Self, allocator: Allocator, input: []const u8) Allocator.Error!?Match {
        var pos: usize = 0;
        if (getLiteralPrefix(&self)) |prefix| {
            if (std.mem.indexOf(u8, input, prefix)) |i| pos = i;
        }

        const state: SearchState = .{
            .input = .{ .value = input, .pos = pos },
            .capture = null,
        };
        return self.match(allocator, state);
    }

    fn searchAll(self: RE, source: []const u8) MatchIterator {
        return MatchIterator.init(self, source);
    }

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
                ._repetition, ._alt, ._capture => break :loop,
                else => {
                    const m = try re.match(allocator, state) orelse return null;
                    result.len += m.len;
                    state = state.sliceInput(m.len);
                },
            }
        }

        if (i >= args.len) {
            return result;
        }

        const rest_args = args[i + 1 ..];
        switch (args[i].*) {
            ._repetition, ._alt => {
                var iterator: Iterator = .init(args[i], state);
                defer iterator.deinit(allocator);

                while (try iterator.next(allocator)) |m| {
                    if (rest_args.len == 0) return .{
                        .pos = result.pos,
                        .len = result.len + m.len,
                    };

                    const sub_state = state.sliceInput(m.len);
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
                    .capture = .{
                        .value = m.string(state.input.value),
                        .index = if (state.capture) |c| c.index + 1 else 0,
                        .parent = if (state.capture) |*c| c else null,
                    },
                    .input = state.input.slice(m.len),
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
                return re.match(allocator, state);
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
        self: Self,
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

    // Caller should free the returned string
    fn repeatString(allocator: Allocator, count: usize, str: []const u8) ![]const u8 {
        const len = count * str.len;
        var buf: ArrayList(u8) = try .initCapacity(allocator, len);
        errdefer buf.deinit(allocator);

        var i: usize = 0;
        while (i < len) : (i += str.len) {
            try buf.appendSlice(allocator, str);
        }
        return buf.toOwnedSlice(allocator);
    }

    fn repeat(self: RE, allocator: Allocator, count: usize) ![]const RE {
        var buf: ArrayList(RE) = try .initCapacity(allocator, count);
        for (0..count) |_| {
            try buf.append(allocator, try self.dupe(allocator));
        }
        return buf.toOwnedSlice(allocator);
    }

    // TODO: re.normalize(allocator) for optimization
    // / combine adjacent literals into a single literal
    //   - if all concat args is all literal, convert concat to single literal
    // / flatten nested concats
    // / expand min repetition (if min is not too large)
    //   / atLeast(2, "foo") -> concat("foofoo", zeroOrMore("foo"))
    //   / atLeast(2, any) -> concat(any, any, zeroOrMore(any))
    // - extract common either cases
    //   - either() -> never
    //     - concat(never) -> never
    //     This doesn't sound quite right
    //     What's the zero value for or-operators?
    //     Is it zero or one (true or false)?
    //     1 or 1 = 1
    //     1 or 0 = 1
    //     0 or 1 = 1
    //     0 or 0 = 0
    //     This does look like an addition, so the zero value should be 0?
    //
    //   - either(either(a,b), either(c,d)) ==? either(a,b,c,d)
    //   - either(a,a,a,a,b) == either(a,b)
    //   - either("xyz", "xyyy", "xxx") -> concat("x", either("yz", "yyy", "xx"))
    //   - either("xyz", "xyyy", "") -> literal("")
    //     this makes sense? even the regex tester confirms this
    // - either with one elem -> remove either
    // - combine either charsets
    // - if either contains only literals, use a hashmap
    //   - this will need to store the size of each key though
    //     hash.contains(input[0..size]) for each key size
    //   - maybe not, hashmap is not easy to copy around
    // - .any != literal("") ?
    //   this isn't quite right, any consumes one character,
    //   whereas literal("") doesn't
    //   - concat("", "x", "y") ==? "xy"
    // - .concat() == ""
    //   - or more generally, re(never, ...) -> never
    // - use a non-allocating iterator for literal repetitions
    //
    // regex is surprinsingly algebraic, or mathy
    // whatever that's called

    // Caller must recursively free the returned pointer afterwards,
    // either manually or use re.recursiveFree(allocator)
    fn normalize(re: RE, allocator: Allocator) Allocator.Error!RE {
        // TODO: maybe call this function simplify instead?

        switch (re.*) {
            ._alt => |args| {
                if (args.len == 0) return re.recursiveCopy(allocator);

                const nargs = try normalizeAll(allocator, args);
                errdefer allocator.free(nargs);

                const flattened = try flattenEither(nargs, allocator);
                allocator.free(nargs);
                defer {
                    for (flattened) |item| {
                        item.recursiveFree(allocator);
                    }
                    allocator.free(flattened);
                }

                for (flattened) |arg| {
                    // make sure all arg is a literal
                    if (arg.* != ._literal) return re.recursiveCopy(allocator);

                    if (arg._literal.len == 0) {
                        // an empty string matches anything
                        // so simplify either("", ...) to a literal("")
                        // TODO: actually, remove "" since TRUE or X == X
                        return Regex.literal("").dupe(allocator);
                    }
                }

                std.debug.assert(flattened.len > 0);
                // at this point, every item is guaranteed to be non-empty literals

                const first = flattened[0]._literal;

                var i: usize = 0;
                loop: while (i < flattened.len) : (i += 1) {
                    for (flattened[1..]) |arg| {
                        if (first[i] != arg._literal[i]) break :loop;
                    }
                }

                if (i == 0) {
                    // no common prefix found
                    return re.recursiveCopy(allocator);
                }

                const common_prefix = try allocator.dupe(u8, first[0..i]);
                errdefer allocator.free(common_prefix);

                var new_args: ArrayList(RE) = try .initCapacity(allocator, flattened.len);
                errdefer {
                    for (new_args.items) |item| item.recursiveFree(allocator);
                    new_args.deinit(allocator);
                }

                for (flattened) |item| {
                    const str = try allocator.dupe(u8, item._literal[i..]);
                    errdefer allocator.free(str);
                    try new_args.append(allocator, try Regex.literal(str).dupe(allocator));
                }

                return try Regex.concat(try allocator.dupe(RE, &.{
                    try Regex.literal(common_prefix).dupe(allocator),
                    try Regex.either(new_args.items).dupe(allocator),
                })).dupe(allocator);
            },

            ._repetition => |val| {
                if (val.min == 0) return re.recursiveCopy(allocator);

                const n = try val.re.normalize(allocator);
                errdefer n.recursiveFree(allocator);

                switch (n.*) {
                    ._literal => {
                        const str = try repeatString(allocator, val.min, n._literal);

                        return Regex.concat(try allocator.dupe(RE, &.{
                            try Regex.literal(str).dupe(allocator),
                            try Regex.zeroOrMore(n).dupe(allocator),
                        })).dupe(allocator);
                    },
                    else => {
                        const repeated = try n.repeat(allocator, val.min);
                        errdefer allocator.free(repeated);
                        n.recursiveFree(allocator);

                        return Regex.concat(&.{
                            &.concat(repeated),
                            &.zeroOrMore(n),
                        }).normalize(allocator);
                    },
                }
            },
            ._concat => |args| {
                if (args.len == 0) return Regex.literal("").dupe(allocator);

                const nargs = try normalizeAll(allocator, args);
                errdefer allocator.free(nargs);

                const flattened = try flattenConcat(nargs, allocator);
                defer allocator.free(flattened);
                allocator.free(nargs);

                var i: usize = 0;
                var combined: ArrayList(RE) = try .initCapacity(allocator, flattened.len);
                defer combined.deinit(allocator);

                errdefer {
                    for (combined.items) |item| item.recursiveFree(allocator);
                    for (i..flattened.len) |index| {
                        flattened[index].recursiveFree(allocator);
                    }
                }

                while (i < flattened.len) {
                    const current = flattened[i];
                    if (current.* != ._literal) {
                        try combined.append(allocator, current);
                        i += 1;
                        continue;
                    }

                    var j: usize = i + 1;
                    while (j < flattened.len and flattened[j].* == ._literal) {
                        j += 1;
                    }

                    const lit = try concatLiterals(allocator, flattened[i..j]);
                    errdefer lit.recursiveFree(allocator);

                    try combined.append(allocator, lit);

                    for (i..j) |index| {
                        const re2 = flattened[index];
                        allocator.free(re2._literal);
                        allocator.destroy(re2);
                    }

                    i = j;
                }

                if (combined.items.len == 1) {
                    const first = combined.items[0];
                    return first;
                }

                const new_args = try combined.toOwnedSlice(allocator);
                errdefer allocator.free(new_args);

                return Regex.concat(new_args).dupe(allocator);
            },
            ._literal => |val| {
                const str = try allocator.dupe(u8, val);
                return Regex.literal(str).dupe(allocator);
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

    fn normalizeAll(allocator: Allocator, args: []const RE) Allocator.Error![]const RE {
        var result: ArrayList(RE) = try .initCapacity(allocator, args.len);
        errdefer result.deinit(allocator);

        for (args) |arg| {
            try result.append(allocator, try normalize(arg, allocator));
        }

        return result.toOwnedSlice(allocator);
    }

    fn concatLiterals(allocator: Allocator, args: []const RE) !RE {
        var size: usize = 0;
        for (args) |arg| {
            switch (arg.*) {
                ._literal => |val| size += val.len,
                else => unreachable,
            }
        }
        var str: ArrayList(u8) = try .initCapacity(allocator, size);
        for (args) |arg| {
            try str.appendSlice(allocator, arg._literal);
        }
        return Regex.literal(try str.toOwnedSlice(allocator)).dupe(allocator);
    }

    // TODO: should be private
    // shallow duplicate
    fn dupe(self: RE, allocator: Allocator) !RE {
        const copy = try allocator.create(Regex);
        copy.* = self.*;
        return copy;
    }

    fn recursiveCopy(self: RE, allocator: Allocator) !RE {
        const result: Regex = balake: switch (self.*) {
            ._literal => |val| {
                const str = try allocator.dupe(u8, val);
                break :balake .literal(str);
            },

            inline ._concat, ._alt => |args, t| {
                var args_buf: ArrayList(RE) = try .initCapacity(allocator, args.len);
                errdefer args_buf.deinit(allocator);

                for (args) |arg| {
                    const arg_copy = try arg.recursiveCopy(allocator);
                    errdefer arg_copy.recursiveFree(allocator);
                    try args_buf.append(allocator, arg_copy);
                }

                break :balake switch (t) {
                    ._concat => .concat(try args_buf.toOwnedSlice(allocator)),
                    ._alt => .either(try args_buf.toOwnedSlice(allocator)),
                    else => comptime unreachable,
                };
            },

            ._repetition => |val| {
                const inner_re = try val.re.recursiveCopy(allocator);
                errdefer inner_re.recursiveFree(allocator);

                return (Regex{
                    ._repetition = .{
                        .re = inner_re,
                        .min = val.min,
                        .max = val.max,
                        .greedy = val.greedy,
                    },
                }).dupe(allocator);
            },
            ._capture => |re| {
                const copy = try re.recursiveCopy(allocator);
                errdefer copy.recursiveFree(allocator);
                return Regex.capture(copy).dupe(allocator);
            },

            // balake, not ey ey ron or jayquelin
            else => break :balake self.*,
        };

        errdefer result.recursiveFree(allocator);
        return result.dupe(allocator);
    }

    fn recursiveFree(self: RE, allocator: Allocator) void {
        switch (self.*) {
            ._literal => |val| allocator.free(val),
            ._repetition => |val| val.re.recursiveFree(allocator),
            ._capture => |re| re.recursiveFree(allocator),
            ._concat, ._alt => |args| {
                for (args) |sub_re| sub_re.recursiveFree(allocator);
                allocator.free(args);
            },
            else => {},
        }
        allocator.destroy(self);
    }

    // No regex copies will be made,
    // so only the ArrayList should be free/deinit'ed,
    // not the individual regexes.
    fn flattenConcat(args: []const RE, allocator: Allocator) ![]const RE {
        // assumes each concat in args is already flattened
        // so no need to do this recursively
        var result: ArrayList(RE) = try .initCapacity(allocator, args.len);
        for (args) |re| {
            switch (re.*) {
                ._concat => |items| try result.appendSlice(allocator, items),
                else => try result.append(allocator, re),
            }
        }
        return result.toOwnedSlice(allocator);
    }

    // same as flattenConcat but with either
    fn flattenEither(args: []const RE, allocator: Allocator) ![]const RE {
        var result: ArrayList(RE) = try .initCapacity(allocator, args.len);
        for (args) |re| {
            switch (re.*) {
                ._alt => |items| try result.appendSlice(allocator, items),
                else => try result.append(allocator, re),
            }
        }
        return result.toOwnedSlice(allocator);
    }

    fn equals(re1: RE, re2: RE) bool {
        if (std.meta.activeTag(re1.*) != std.meta.activeTag(re2.*)) {
            return false;
        }

        return switch (re1.*) {
            ._literal => std.mem.eql(u8, re1._literal, re2._literal),
            ._capture => return equals(re1._capture, re2._capture),
            ._backref => re1._backref == re2._backref,

            ._concat => {
                const a = re1._concat;
                const b = re2._concat;
                if (a.len != b.len) return false;

                for (0..a.len) |i| {
                    if (!equals(
                        a[i],
                        b[i],
                    )) return false;
                }
                return true;
            },
            ._alt => {
                const a = re1._alt;
                const b = re2._alt;
                if (a.len != b.len) return false;

                for (0..a.len) |i| {
                    if (!equals(
                        a[i],
                        b[i],
                    )) return false;
                }
                return true;
            },
            ._repetition => {
                const a = re1._repetition;
                const b = re2._repetition;
                if (!equals(a.re, b.re)) return false;
                return a.min == b.min and
                    a.max == b.max and
                    a.greedy == b.greedy;
            },

            else => true,
        };
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
            const sub_state = state.sliceInput(self.len);
            if (try self.re.match(allocator, sub_state)) |m| {
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

// TODO: I could try wrapping an allocator
// such that the OutOfMemory error is panic'ed instead
// and not returned. That way, a lot of the allocating
// code can be simplified and remove all the
// error handling. This might go against zig philosophy
// or whatnot, but OutOfMemory isn't something I need
// to handle or recover in this case (or in general?)
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
                if (try self.re.match(allocator, state.sliceInput(self.len))) |m| {
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

const AlternationIterator = struct {
    choices: []const Regex.RE,
    state: Regex.SearchState,
    index: usize = 0,

    const Self = @This();

    fn deinit(_: *Self, _: Allocator) void {}

    fn next(self: *Self, allocator: Allocator) !?Regex.Match {
        while (self.index < self.choices.len) {
            defer self.index += 1;
            const re = self.choices[self.index];

            if (try re.match(allocator, self.state)) |m| {
                return m;
            }
        }

        return null;
    }
};

const SingleIterator = struct {
    re: Regex.RE,
    state: Regex.SearchState,
    first: bool = true,

    const Self = @This();

    fn deinit(_: *Self, _: Allocator) void {}

    fn next(self: *Self, allocator: Allocator) !?Regex.Match {
        if (self.first) {
            self.first = false;
            return self.re.match(allocator, self.state);
        }
        return null;
    }
};

const Iterator = union(enum) {
    greedy: GreedyIterator,
    lazy: LazyIterator,
    alternation: AlternationIterator,
    single: SingleIterator,

    const Self = @This();

    fn init(
        re: Regex.RE,
        state: Regex.SearchState,
    ) Self {
        return switch (re.*) {
            ._repetition => |val| {
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
            ._alt => |args| .{
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

const testing = std.testing;

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
        const result = try item.re.search(testing.allocator, item.input);
        std.debug.print("input: {s}, expected: {s}, got: ", .{ item.input, item.expected orelse "<null>" });
        if (result) |m| {
            std.debug.print("{s}\n", .{m.string(item.input)});
        } else {
            std.debug.print("<null>\n", .{});
        }

        if (item.expected) |expected| {
            try testing.expect(result != null);
            try testing.expectEqualSlices(u8, expected, result.?.string(item.input));
        } else {
            try testing.expect(result == null);
        }
    }
}

test "match iterator" {
    const expected: []const []const u8 = &.{
        "aabc",
        "abc",
        "aaabc",
        "abc",
        "aaaabc",
    };
    const re: Regex.RE = &.concat(&.{
        &.oneOrMore(&.literal("a")),
        &.literal("bc"),
    });
    const source = "aabcabc   aaabcasdjfabciasofaaaabca";
    var iterator = re.searchAll(source);

    var i: usize = 0;
    while (try iterator.next(testing.allocator)) |m| {
        try testing.expect(i < expected.len);
        try testing.expectEqualStrings(expected[i], m.string(source));
        i += 1;
    }
}

test "equality" {
    const allocator = testing.allocator;

    const re0: Regex.RE = &.literal("abc");
    try testing.expect(re0.equals(re0));
    try testing.expect(re0.equals(&.literal("abc")));
    try testing.expect(!re0.equals(&.literal("xyz")));

    const re1: Regex.RE = &.concat(&.{
        &.literal("aaaa"),
        &.either(&.{
            &.literal("bbbb"),
            &.capture(&.zeroOrMore(&.any)),
        }),
        &.any,
    });
    const re2 = try re1.recursiveCopy(testing.allocator);
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
    const re: Regex.RE = &.concat(&.{
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
    const actual = try re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: Regex.RE = &.literal("abcdefghijkl");
    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "normalize concat 2" {
    const re: Regex.RE = &.concat(&.{
        &.literal("abc"),
        &.literal("def"),
        &.any,
        &.literal("ghi"),
        &.literal("jkl"),
    });

    const allocator = testing.allocator;
    const actual = try re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: Regex.RE = &.concat(&.{
        &.literal("abcdef"),
        &.any,
        &.literal("ghijkl"),
    });
    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "repetition 1" {
    const allocator = testing.allocator;
    const re: Regex.RE = &.atLeast(2, &.literal("abc"));

    const actual = try re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    const expected: Regex.RE = &.concat(&.{
        &.literal("abcabc"),
        &.zeroOrMore(&.literal("abc")),
    });
    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

test "repetition 2" {
    const allocator = testing.allocator;
    const re: Regex.RE = &.zeroOrMore(&.literal("abc"));

    const actual = try re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    try testing.expectEqualDeep(re, actual);
    try testing.expect(re.equals(actual));
}

test "alternation 1" {
    const allocator = testing.allocator;
    const re: Regex.RE = &.either(&.{
        &.literal("aabcdef"),
        &.literal("aaaaa"),
        &.literal("aaaxyz"),
    });
    const expected: Regex.RE = &.concat(&.{
        &.literal("aa"),
        &.either(&.{
            &.literal("bcdef"),
            &.literal("aaa"),
            &.literal("axyz"),
        }),
    });

    const actual = try re.normalize(allocator);
    defer actual.recursiveFree(allocator);

    try testing.expectEqualDeep(expected, actual);
    try testing.expect(expected.equals(actual));
}

// TODO: re.serialize()
