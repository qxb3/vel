const std = @import("std");
const Shell = @import("shell").Shell;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const gpa_alloc = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(gpa_alloc);
    defer arena.deinit();

    const arena_alloc = arena.allocator();

    const shell = Shell.init(arena_alloc);
    try shell.t();
}
