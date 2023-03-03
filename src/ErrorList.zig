const std = @import("std");
const Token = @import("Token.zig");
const ErrorList = @This();

allocator: std.mem.Allocator,
source: [:0]const u8,
// file_name: []const u8,
errors: std.ArrayListUnmanaged(u8) = .{},

pub fn deinit(self: *ErrorList) void {
    self.errors.deinit(self.allocator);
}

pub fn add(
    self: *ErrorList,
    loc: Token.Loc,
    comptime err_fmt: []const u8,
    fmt_args: anytype,
    comptime note_fmt: ?[]const u8,
    note_args: anytype,
) !void {
    var bw = std.io.bufferedWriter(self.errors.writer(self.allocator));
    const b = bw.writer();
    const term = std.debug.TTY.Config{ .escape_codes = {} };
    const loc_extra = loc.extraInfo(self.source);

    // 'file:line:column'
    try term.setColor(b, .Bold);
    try b.print(":{d}:{d} ", .{ loc_extra.line, loc_extra.col });

    // 'error: '
    try term.setColor(b, .Red);
    try b.writeAll("error: ");

    // error message
    try term.setColor(b, .Reset);
    try term.setColor(b, .Bold);
    try b.print(err_fmt, fmt_args);
    try b.writeByte('\n');

    // error line
    try term.setColor(b, .Dim);
    try b.print("{d} │ ", .{loc_extra.line});
    try term.setColor(b, .Reset);
    try b.writeAll(self.source[loc_extra.line_start..loc.start]);
    try term.setColor(b, .Green);
    try b.writeAll(self.source[loc.start..loc.end]);
    try term.setColor(b, .Reset);
    try b.writeAll(self.source[loc.end..loc_extra.line_end]);
    try b.writeByte('\n');

    // error location pointer
    const line_number_length = (std.math.log10(loc_extra.line) + 1) + 3;
    try b.writeByteNTimes(
        ' ',
        line_number_length + (loc_extra.col - 1),
    );
    try term.setColor(b, .Bold);
    try term.setColor(b, .Green);
    try b.writeByte('^');
    try b.writeByteNTimes('~', loc.end - loc.start - 1);
    try b.writeByte('\n');

    // note
    if (note_fmt != null) {
        try term.setColor(b, .Cyan);
        try b.writeAll("note: ");

        // note message
        try term.setColor(b, .Reset);
        try term.setColor(b, .Bold);
        try b.print(note_fmt.?, note_args);
        try b.writeByte('\n');
    } else {
        // style enforce
        if (@TypeOf(note_args) != @TypeOf(null)) {
            @compileError("`note_args` must be `null` when `note_fmt` is null");
        }
    }

    // clean up and flush
    try term.setColor(b, .Reset);
    try bw.flush();
}

pub fn flush(self: *ErrorList) !void {
    _ = try std.io.getStdErr().write(self.errors.items);
}
