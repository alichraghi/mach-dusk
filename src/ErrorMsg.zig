const std = @import("std");
const Token = @import("Token.zig");
const ErrorMsg = @This();

loc: ?Token.Loc,
msg: []const u8,
notes: []const ErrorMsg,

pub fn create(
    allocator: std.mem.Allocator,
    loc: ?Token.Loc,
    comptime format: []const u8,
    args: anytype,
    notes: []const ErrorMsg,
) !ErrorMsg {
    return .{
        .loc = loc,
        .msg = try std.fmt.allocPrint(allocator, format, args),
        .notes = notes,
    };
}

pub fn deinit(err_msg: ErrorMsg, allocator: std.mem.Allocator) void {
    for (err_msg.notes) |*note| note.deinit(allocator);
    allocator.free(err_msg.msg);
}

pub fn printErrors(errors: []const ErrorMsg, source: []const u8, file_path: ?[]const u8) !void {
    var bw = std.io.bufferedWriter(std.io.getStdErr().writer());
    const b = bw.writer();
    const term = std.debug.TTY.Config{ .escape_codes = {} };

    for (errors) |err| {
        const loc = err.loc.?;
        const loc_extra = loc.extraInfo(source);

        {
            // 'file:line:column'
            try term.setColor(b, .Bold);
            try b.print("{?s}:{d}:{d} ", .{ file_path, loc_extra.line, loc_extra.col });
        }

        {
            // 'error: '
            try term.setColor(b, .Red);
            try b.writeAll("error: ");
        }

        {
            // error message
            try term.setColor(b, .Reset);
            try term.setColor(b, .Bold);
            try b.writeAll(err.msg);
            try b.writeByte('\n');
        }

        {
            // error code
            try term.setColor(b, .Dim);
            try b.print("{d} │ ", .{loc_extra.line});
            try term.setColor(b, .Reset);
            try b.writeAll(source[loc_extra.line_start..loc.start]);
            try term.setColor(b, .Green);
            try b.writeAll(source[loc.start..loc.end]);
            try term.setColor(b, .Reset);
            try b.writeAll(source[loc.end..loc_extra.line_end]);
            try b.writeByte('\n');
        }

        {
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
        }

        // note
        for (err.notes) |note| {
            {
                try term.setColor(b, .Cyan);
                try b.writeAll("note: ");
            }

            {
                // note message
                try term.setColor(b, .Reset);
                try term.setColor(b, .Bold);
                try b.writeAll(note.msg);
                try b.writeByte('\n');
            }

            if (note.loc) |note_loc| {
                {
                    // note code
                    try term.setColor(b, .Dim);
                    try b.print("{d} │ ", .{loc_extra.line});
                    try term.setColor(b, .Reset);
                    try b.writeAll(source[loc_extra.line_start..note_loc.start]);
                    try term.setColor(b, .Green);
                    try b.writeAll(source[note_loc.start..note_loc.end]);
                    try term.setColor(b, .Reset);
                    try b.writeAll(source[note_loc.end..loc_extra.line_end]);
                    try b.writeByte('\n');
                }

                // note location pointer
                {
                    const line_number_length = (std.math.log10(loc_extra.line) + 1) + 3;
                    try b.writeByteNTimes(
                        ' ',
                        line_number_length + (loc_extra.col - 1),
                    );
                    try term.setColor(b, .Bold);
                    try term.setColor(b, .Green);
                    try b.writeByte('^');
                    try b.writeByteNTimes('~', note_loc.end - note_loc.start - 1);
                    try b.writeByte('\n');
                }
            }
        }

        // clean up and flush
        try term.setColor(b, .Reset);
        try bw.flush();
    }
}
