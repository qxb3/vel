const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;

pub const Shell = struct {
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) Shell {
        return .{
            .alloc = alloc
        };
    }

    pub fn t(self: *const Shell) !void {
        var lexer = Lexer.init("echo \"hello world\" 'hi'", self.alloc);
        const tokens = try lexer.lex();

        for (tokens) |token| {
            std.debug.print("TOKEN = {any}\n", .{token});
        }
    }
};
