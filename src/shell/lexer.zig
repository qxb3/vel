const std = @import("std");

pub const TokenType = enum {
    IDENTIFIER,         // words, commands, any non number starting alphanumeric stuff.
    FLAG,               // -foo or --bar.
    STRING_LITERAL,     // "string" or 'string'.
    NUMBER_LITERAL,     // 12345.

    COMMENT,            // #
    DOLLARS,            // $
    BACKSLASH,          // \
    EQUALS,             // =
    OPEN_PAREN,         // (
    CLOSE_PAREN,        // )
    OPEN_BRACE,         // {
    CLOSE_BRACE,        // }
    OPEN_BRACKET,       // [
    CLOSE_BRACKET,      // ]
    SEMI,               // ;
    AMP,                // &
    DOUBLE_AMP,         // &&
    PIPE,               // |
    DOUBLE_PIPE,        // ||
    REDIRECT_OUT,       // >
    REDIRECT_APPEND,    // >>
    REDIRECT_OUT_FD,    // >&
    REDIRECT_IN,        // <
    REDIRECT_IN_FD,     // <&
    HEREDOC,            // <<
};

pub const Token = union(TokenType) {
    IDENTIFIER: []const u8,
    FLAG: []const u8,
    STRING_LITERAL: []const u8,
    NUMBER_LITERAL: i64,

    COMMENT,
    DOLLARS,
    BACKSLASH,
    EQUALS,
    OPEN_PAREN,
    CLOSE_PAREN,
    OPEN_BRACE,
    CLOSE_BRACE,
    OPEN_BRACKET,
    CLOSE_BRACKET,
    SEMI,
    AMP,
    DOUBLE_AMP,
    PIPE,
    DOUBLE_PIPE,
    REDIRECT_OUT,
    REDIRECT_APPEND,
    REDIRECT_OUT_FD,
    REDIRECT_IN,
    REDIRECT_IN_FD,
    HEREDOC,
};

pub const LexerError = error {
    DELIMITER_NOT_CLOSED,
    UNKNOWN_SYMBOL
} || std.mem.Allocator.Error
  || std.fmt.ParseIntError;

pub const Lexer = struct {
    source: []const u8,
    cursor: usize,
    alloc: std.mem.Allocator,

    pub fn init(source: []const u8, alloc: std.mem.Allocator) Lexer {
        return .{
            .source = source,
            .cursor = 0,
            .alloc = alloc
        };
    }

    pub fn lex(self: *Lexer) LexerError![]Token {
        var tokens = std.ArrayList(Token).init(self.alloc);
        defer tokens.deinit();

        while (self.has_next()) {
            const current_char = self.get_current_char();

            // Skips whitespace.
            if (std.ascii.isWhitespace(current_char)) {
                self.advance();
                continue;
            }

            // Checks for identifiers.
            if (
                (!std.ascii.isDigit(current_char) and // Start of the identifier should not start as a number.
                 current_char != '-' and // Start of the identifier should not start with (-).
                 current_char != '.' and // Start of the identifier should not start with (.).
                self.is_identifier(current_char)) or // If alpha numeric.
                current_char == '_' // Can start with _.
            ) {
                const identifier = try self.lex_identifier();
                try tokens.append(identifier);
                continue;
            }

            // Checks for flags.
            if (current_char == '-') {
                const flag = try self.lex_flag();
                try tokens.append(flag);
                continue;
            }

            // Checks for string literals.
            if (current_char == '\'' or current_char == '"') {
                const string_literal = try self.lex_string_literal();
                try tokens.append(string_literal);
                continue;
            }

            // Checks for numbers.
            if (std.ascii.isDigit(current_char)) {
                const number_literal = try self.lex_number_literal();
                try tokens.append(number_literal);
                continue;
            }

            // Checks for the symbols.
            const symbol = try self.lex_symbol();
            try tokens.append(symbol);
        }

        return tokens.toOwnedSlice();
    }

    /// Lex identifiers.
    fn lex_identifier(self: *Lexer) LexerError!Token {
        var buffer = std.ArrayList(u8).init(self.alloc);
        defer buffer.deinit();

        while (self.has_next()) {
            const buffer_char = self.get_current_char();
            if (!self.is_identifier(buffer_char)) break;

            try buffer.append(buffer_char);
            self.advance();
        }

        const token = Token { .IDENTIFIER = buffer.items };
        return token;
    }

    /// Lex flags.
    fn lex_flag(self: *Lexer) LexerError!Token {
        var buffer = std.ArrayList(u8).init(self.alloc);
        defer buffer.deinit();

        while (self.has_next()) {
            const buffer_char = self.get_current_char();
            if (buffer_char != '-' and !self.is_identifier(buffer_char)) break;

            try buffer.append(buffer_char);
            self.advance();
        }

        const token = Token { .FLAG = buffer.items };
        return token;
    }

    /// Lex string literals.
    fn lex_string_literal(self: *Lexer) LexerError!Token {
        var buffer = std.ArrayList(u8).init(self.alloc);
        defer buffer.deinit();

        // Gets the string delimiter can be ' or ".
        const delimiter = self.get_current_char();
        self.advance(); // advance for the delimiter.

        // A marker if the delimiter has been closed.
        var delimiter_closed = false;

        while (self.has_next()) {
            const buffer_char = self.get_current_char();

            // If the current buffer_char is the same as the delimiter,
            // advance and break and also set the delimiter marker that its has been closed.
            if (buffer_char == delimiter) {
                self.advance();
                delimiter_closed = true;

                break;
            }

            try buffer.append(buffer_char);
            self.advance();
        }

        // Return an error if the delimiter has not been closed.
        if (!delimiter_closed) {
            return LexerError.DELIMITER_NOT_CLOSED;
        }

        const token = Token { .STRING_LITERAL = buffer.items };
        return token;
    }

    /// Lex number literals.
    fn lex_number_literal(self: *Lexer) LexerError!Token {
        var buffer = std.ArrayList(u8).init(self.alloc);
        defer buffer.deinit();

        while (self.has_next()) {
            const buffer_char = self.get_current_char();
            if (!std.ascii.isDigit(buffer_char)) break;

            try buffer.append(buffer_char);
            self.advance();
        }

        // Parse the buffer first into i64.
        const number = try std.fmt.parseInt(i64, buffer.items, 10);

        const token = Token { .NUMBER_LITERAL = number };
        return token;
    }

    /// Lex for symbols
    fn lex_symbol(self: *Lexer) LexerError!Token {
        const current_char = self.get_current_char();

        // Double char symbols.
        if (self.peek()) |next_char| {
            if (current_char == '&' and next_char == '&') return self.emit_double(Token.DOUBLE_AMP);
            if (current_char == '|' and next_char == '|') return self.emit_double(Token.DOUBLE_PIPE);
            if (current_char == '>' and next_char == '>') return self.emit_double(Token.REDIRECT_APPEND);
            if (current_char == '>' and next_char == '&') return self.emit_double(Token.REDIRECT_OUT_FD);
            if (current_char == '<' and next_char == '&') return self.emit_double(Token.REDIRECT_IN_FD);
            if (current_char == '<' and next_char == '<') return self.emit_double(Token.HEREDOC);
        }

        // Single char symbols.
        if (current_char == '#')  return self.emit_single(Token.COMMENT);
        if (current_char == '$')  return self.emit_single(Token.DOLLARS);
        if (current_char == '\\') return self.emit_single(Token.BACKSLASH);
        if (current_char == '=')  return self.emit_single(Token.EQUALS);
        if (current_char == '(')  return self.emit_single(Token.OPEN_PAREN);
        if (current_char == ')')  return self.emit_single(Token.CLOSE_PAREN);
        if (current_char == '{')  return self.emit_single(Token.OPEN_BRACE);
        if (current_char == '}')  return self.emit_single(Token.CLOSE_BRACE);
        if (current_char == '[')  return self.emit_single(Token.OPEN_BRACKET);
        if (current_char == ']')  return self.emit_single(Token.CLOSE_BRACKET);
        if (current_char == ';')  return self.emit_single(Token.SEMI);
        if (current_char == '&')  return self.emit_single(Token.AMP);
        if (current_char == '|')  return self.emit_single(Token.PIPE);
        if (current_char == '>')  return self.emit_single(Token.REDIRECT_OUT);
        if (current_char == '<')  return self.emit_single(Token.REDIRECT_IN);

        return LexerError.UNKNOWN_SYMBOL;
    }

    // Emits a single token then advance once.
    fn emit_single(self: *Lexer, token: Token) Token {
        self.advance();

        return token;
    }

    // Emits a double char token then advance twice.
    fn emit_double(self: *Lexer, token: Token) Token {
        self.advance();
        self.advance();

        return token;
    }

    /// Peeks at the next char.
    fn peek(self: *Lexer) ?u8 {
        if (self.cursor + 1 < self.source.len) {
            const peek_char = self.source[self.cursor + 1];
            return peek_char;
        }

        return null;
    }

    /// Gets the current char.
    fn get_current_char(self: *Lexer) u8 {
        return self.source[self.cursor];
    }

    /// Checks if there is still a next char.
    fn has_next(self: *Lexer) bool {
        return self.cursor < self.source.len;
    }

    /// Advance the cursor.
    fn advance(self: *Lexer) void {
        self.cursor += 1;
    }

    /// A wrapper arround std.ascii.isAlphanumeric with some additional conditions.
    fn is_identifier(self: *Lexer, char: u8) bool {
        _ = self;

        return (
            std.ascii.isAlphanumeric(char) or
            char == '_' or
            char == '-' or
            char == '.'
        );
    }
};
