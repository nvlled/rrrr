const std = @import("std");

fn UnwrapError(T: type) type {
    const info: std.builtin.Type = @typeInfo(T);
    switch (info) {
        .error_union => |eu| return eu.payload,
        else => return T,
    }
}

// A function that turns allocation errors, specifically out of memory error,
// into a runtime panic. The reason for this is that out of memory errors
// are non-recoverable, unless the environment has a very low, tight memory requirements,
// a kind of environment this library isn't designed for.
// By turning the allocation errors into panic, the code is
// vastly simplified, especially in this case where there are no
// other failure cases besides allocation.
pub fn @"try"(arg: anytype) UnwrapError(@TypeOf(arg)) {
    const info: std.builtin.Type = @typeInfo(@TypeOf(arg));
    switch (info) {
        .error_union => return arg catch |err| {
            if (err == error.OutOfMemory) @panic("Allocation failed, not enough memory");
            @panic("An error occured while allocating.");
        },
        else => return arg,
    }
}
