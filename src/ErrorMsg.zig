const std = @import("std");
const Token = @import("Token.zig");
const ErrorMsg = @This();

loc: ?Token.Loc,
msg: []const u8,
note: ?*ErrorMsg,

pub fn create(
    allocator: std.mem.Allocator,
    loc: ?Token.Loc,
    comptime format: []const u8,
    args: anytype,
    note: ?*ErrorMsg,
) !*ErrorMsg {
    var err_msg = try allocator.create(ErrorMsg);
    err_msg.* = .{
        .loc = loc,
        .msg = try std.fmt.allocPrint(allocator, comptime format, args),
        .note = note,
    };
    return err_msg;
}

pub fn deinit(err_msg: ErrorMsg, allocator: std.mem.Allocator) void {
    if (err_msg.note) |*note| note.*.deinit(allocator);
    allocator.free(err_msg.msg);
}
