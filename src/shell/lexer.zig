const std = @import("std");

pub const TokenType = enum {
    WORD,               // words, commands, flags, path, any non number starting alphanumeric stuff.
    STRING_LITERAL,     // "string" or 'string'.
    NUMBER_LITERAL,     // 12345.

    COMMENT,            // #
    DOLLARS,            // $
    HOME_EXPAND,        // ~
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
    GTHAN,              // >
    DOUBLE_GTHAN,       // >>
    GTHAN_AMP,          // >&
    LTHAN,              // <
    LTHAN_AMP,          // <&
    DOUBLE_LTHAN,       // <<
};

pub const Token = union(TokenType) {
    WORD: []const u8,
    STRING_LITERAL: []const u8,
    NUMBER_LITERAL: i64,

    COMMENT,
    DOLLARS,
    HOME_EXPAND,
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
    GTHAN,
    DOUBLE_GTHAN,
    GTHAN_AMP,
    LTHAN,
    LTHAN_AMP,
    DOUBLE_LTHAN,
};

pub const LexerError = error {
    DELIMITER_NOT_CLOSED,
    UNKNOWN_SYMBOL
} || std.mem.Allocator.Error
  || std.fmt.ParseIntError;

pub const Lexer = struct {
    source: []const u8,
    start: usize,
    cursor: usize,
    alloc: std.mem.Allocator,

    pub fn init(source: []const u8, alloc: std.mem.Allocator) Lexer {
        return .{
            .source = source,
            .start = 0,
            .cursor = 0,
            .alloc = alloc
        };
    }

    pub fn lex(self: *Lexer) LexerError![]Token {
        var tokens = std.ArrayList(Token).init(self.alloc);

        while (self.has_next()) {
            const current_char = self.get_current_char();

            // Skips whitespace.
            if (std.ascii.isWhitespace(current_char)) {
                self.advance();
                continue;
            }

            // Checks for words.
            if (
                !std.ascii.isDigit(current_char) and   // Cannot start with a number.
                self.is_word(current_char)       // Check if is alpha numeric.
            ) {
                const identifier = try self.lex_word();
                try tokens.append(identifier);
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
    fn lex_word(self: *Lexer) LexerError!Token {
        self.start_cursor();

        while (self.has_next()) {
            const buffer_char = self.get_current_char();
            if (!self.is_word(buffer_char)) break;

            self.advance();
        }

        const word = self.source[self.start..self.cursor];
        const token = Token { .WORD = word };

        return token;
    }

    /// Lex string literals.
    fn lex_string_literal(self: *Lexer) LexerError!Token {
        // Gets the string delimiter can be ' or ".
        const delimiter = self.get_current_char();
        self.advance(); // advance for the delimiter.

        self.start_cursor();

        // A marker if the delimiter has been closed.
        var delimiter_closed = false;

        while (self.has_next()) {
            const buffer_char = self.get_current_char();

            // If the current buffer_char is the same as the delimiter,
            // break and also set the delimiter marker that its has been closed.
            if (buffer_char == delimiter) {
                delimiter_closed = true;
                break;
            }

            self.advance();
        }

        // Return an error if the delimiter has not been closed.
        if (!delimiter_closed) {
            return LexerError.DELIMITER_NOT_CLOSED;
        }

        const string_literal = self.source[self.start..self.cursor];
        const token = Token { .STRING_LITERAL = string_literal };

        self.advance(); // advance for the ending delimiter.

        return token;
    }

    /// Lex number literals.
    fn lex_number_literal(self: *Lexer) LexerError!Token {
        self.start_cursor();

        while (self.has_next()) {
            const buffer_char = self.get_current_char();
            if (!std.ascii.isDigit(buffer_char)) break;

            self.advance();
        }

        // Parse the buffer first into i64.
        const number_literal = self.source[self.start..self.cursor];
        const number = try std.fmt.parseInt(i64, number_literal, 10);
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
            if (current_char == '>' and next_char == '>') return self.emit_double(Token.DOUBLE_GTHAN);
            if (current_char == '>' and next_char == '&') return self.emit_double(Token.GTHAN_AMP);
            if (current_char == '<' and next_char == '&') return self.emit_double(Token.LTHAN_AMP);
            if (current_char == '<' and next_char == '<') return self.emit_double(Token.DOUBLE_LTHAN);
        }

        // Single char symbols.
        if (current_char == '#')  return self.emit_single(Token.COMMENT);
        if (current_char == '$')  return self.emit_single(Token.DOLLARS);
        if (current_char == '~')  return self.emit_single(Token.HOME_EXPAND);
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
        if (current_char == '>')  return self.emit_single(Token.GTHAN);
        if (current_char == '<')  return self.emit_single(Token.LTHAN);

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

    /// Sets the self.start to the cursor.
    fn start_cursor(self: *Lexer) void {
        self.start = self.cursor;
    }

    /// Advance the cursor.
    fn advance(self: *Lexer) void {
        self.cursor += 1;
    }

    /// A wrapper arround std.ascii.isAlphanumeric with some additional conditions.
    fn is_word(self: *Lexer, char: u8) bool {
        _ = self;

        return (
            std.ascii.isAlphanumeric(char) or
            char == '_' or
            char == '-' or
            char == '.' or
            char == '/'
        );
    }

    pub fn deinit(self: *Lexer) void {
        self.alloc.free(self.source);
        self.alloc.free(self.source);
    }
};
