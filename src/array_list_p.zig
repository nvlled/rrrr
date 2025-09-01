const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const tryAlloc = @import("./alloc.zig").@"try";

// A wrapper to std.ArrayList that wraps methods with Alloc.try
fn ArrayListP(comptime T: type) type {
    return struct {
        data: ArrayList(T),

        const Self = @This();
        const Slice = ArrayList(T).Slice;

        pub fn initCapacity(gpa: Allocator, num: usize) Self {
            return .{
                .data = tryAlloc(ArrayList(T).initCapacity(gpa, num)),
            };
        }

        pub fn deinit(self: *Self, gpa: Allocator) void {
            return self.data.deinit(gpa);
        }

        pub fn items(self: *Self) Slice {
            return self.data.items;
        }

        pub fn append(self: *Self, gpa: Allocator, item: T) void {
            tryAlloc(self.data.append(gpa, item));
        }

        pub fn toOwnedSlice(self: *Self, gpa: Allocator) Slice {
            return tryAlloc(self.data.toOwnedSlice(gpa));
        }
    };
}
