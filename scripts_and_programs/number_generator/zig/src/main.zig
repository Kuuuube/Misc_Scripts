const std = @import("std");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();

    std.debug.print("Start number: ", .{});
    var start_number_raw: [10000]u8 = undefined;
    _ = try stdin.read(&start_number_raw);
    var start_number_string = std.mem.splitAny(u8, &start_number_raw, "\n");
    const start_number = try std.fmt.parseInt(usize, start_number_string.first(), 10);
    var current_number = start_number;

    std.debug.print("End number: ", .{});
    var end_number_raw: [10000]u8 = undefined;
    _ = try stdin.read(&end_number_raw);
    var end_number_string = std.mem.splitAny(u8, &end_number_raw, "\n");
    const end_number = try std.fmt.parseInt(usize, end_number_string.first(), 10);

    std.debug.print("Prefix: ", .{});
    var prefix_raw: [10000]u8 = undefined;
    _ = try stdin.read(&prefix_raw);
    var prefix_split = std.mem.splitAny(u8, &prefix_raw, "\n");
    const prefix = prefix_split.first();

    std.debug.print("Suffix: ", .{});
    var suffix_raw: [10000]u8 = undefined;
    _ = try stdin.read(&suffix_raw);
    var suffix_split = std.mem.split(u8, &suffix_raw, "\n");
    const suffix = suffix_split.first();

    const start_time = try std.time.Instant.now();

    const file = try std.fs.cwd().createFile(
        "output.txt",
        .{ .read = true },
    );
    defer file.close();

    var string_format_buf: [30000]u8 = undefined;

    for (start_number..(end_number + 1)) |_| {
        const formatted_string = try std.fmt.bufPrint(&string_format_buf, "{s}{d}{s}\n", .{ prefix, current_number, suffix });
        _ = try file.writeAll(formatted_string);
        current_number += 1;
    }

    const end_time: f64 = @floatFromInt((try std.time.Instant.now()).since(start_time));
    std.debug.print("Generated in: {d:.3}ms\n", .{end_time / std.time.ns_per_ms});
}
