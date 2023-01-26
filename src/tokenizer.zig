const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const Tag = enum {
        eof,
        invalid,

        identifier,
        /// float literal with no suffix
        float,
        /// float literal with an 'f' suffix
        float_f,
        /// float literal with an 'h' suffix
        float_h,
        /// integer literal with no suffix
        int,
        /// integer literal with an 'i' suffix
        int_i,
        /// integer literal with a 'u' suffix
        int_u,

        /// '&'
        @"and",
        /// '&&'
        and_and,
        /// '->'
        arrow,
        /// '@'
        attr,
        /// '/'
        division,
        /// '!'
        bang,
        /// '{'
        brace_left,
        /// '}'
        brace_right,
        /// '['
        bracket_left,
        /// ']'
        bracket_right,
        /// ':'
        colon,
        /// ','
        comma,
        /// '='
        equal,
        /// '=='
        equal_equal,
        /// '>'
        greater_than,
        /// '>='
        greater_than_equal,
        /// '>>'
        shift_right,
        /// '<'
        less_than,
        /// '<='
        less_than_equal,
        /// '<<'
        shift_left,
        /// '%'
        mod,
        /// '-'
        minus,
        /// '--'
        minus_minus,
        /// '!='
        not_equal,
        /// '.'
        period,
        /// '+'
        plus,
        /// '++'
        plus_plus,
        /// '|'
        @"or",
        /// '||'
        or_or,
        /// '('
        paren_left,
        /// ')'
        paren_right,
        /// ';'
        semicolon,
        /// '*'
        star,
        /// '~'
        tilde,
        /// '_'
        underscore,
        /// '^'
        xor,
        /// '+='
        plus_equal,
        /// '-='
        minus_equal,
        /// '*='
        times_equal,
        /// '/='
        division_equal,
        /// '%='
        modulo_equal,
        /// '&='
        and_equal,
        /// '|='
        or_equal,
        /// '^='
        xor_equal,
        /// '>>='
        shift_right_equal,
        /// '<<='
        shift_left_equal,

        /// 'array'
        keyword_array,
        /// 'atomic'
        keyword_atomic,
        /// 'bitcast'
        keyword_bitcast,
        /// 'bool'
        keyword_bool,
        /// 'break'
        keyword_break,
        /// 'case'
        keyword_case,
        /// 'const'
        keyword_const,
        /// 'continue'
        keyword_continue,
        /// 'continuing'
        keyword_continuing,
        /// 'discard'
        keyword_discard,
        /// 'default'
        keyword_default,
        /// 'else'
        keyword_else,
        /// 'enable'
        keyword_enable,
        /// 'f16'
        keyword_f16,
        /// 'f32'
        keyword_f32,
        /// 'fallthrough'
        keyword_fallthrough,
        /// 'false'
        keyword_false,
        /// 'fn'
        keyword_fn,
        /// 'for'
        keyword_for,
        /// 'i32'
        keyword_i32,
        /// 'if'
        keyword_if,
        /// 'let'
        keyword_let,
        /// 'loop'
        keyword_loop,
        /// 'mat2x2'
        keyword_mat2x2,
        /// 'mat2x3'
        keyword_mat2x3,
        /// 'mat2x4'
        keyword_mat2x4,
        /// 'mat3x2'
        keyword_mat3x2,
        /// 'mat3x3'
        keyword_mat3x3,
        /// 'mat3x4'
        keyword_mat3x4,
        /// 'mat4x2'
        keyword_mat4x2,
        /// 'mat4x3'
        keyword_mat4x3,
        /// 'mat4x4'
        keyword_mat4x4,
        /// 'override'
        keyword_override,
        /// 'ptr'
        keyword_ptr,
        /// 'return'
        keyword_return,
        /// 'sampler'
        keyword_sampler,
        /// 'sampler_comparison'
        keyword_comparison_sampler,
        /// 'static_assert'
        keyword_static_assert,
        /// 'struct'
        keyword_struct,
        /// 'switch'
        keyword_switch,
        /// 'texture_depth_2d'
        keyword_texture_depth_2d,
        /// 'texture_depth_2d_array'
        keyword_texture_depth_2d_array,
        /// 'texture_depth_cube'
        keyword_texture_depth_cube,
        /// 'texture_depth_cube_array'
        keyword_texture_depth_cube_array,
        /// 'texture_depth_multisampled_2d'
        keyword_texture_depth_multisampled_2d,
        /// 'texture_external'
        keyword_texture_external,
        /// 'texture_multisampled_2d'
        keyword_texture_multisampled_2d,
        /// 'texture_1d'
        keyword_texture_sampled_1d,
        /// 'texture_2d'
        keyword_texture_sampled_2d,
        /// 'texture_2d_array'
        keyword_texture_sampled_2d_array,
        /// 'texture_3d'
        keyword_texture_sampled_3d,
        /// 'texture_cube'
        keyword_texture_sampled_cube,
        /// 'texture_cube_array'
        keyword_texture_sampled_cube_array,
        /// 'texture_storage_1d'
        keyword_texture_storage_1d,
        /// 'texture_storage_2d'
        keyword_texture_storage_2d,
        /// 'texture_storage_2d_array'
        keyword_texture_storage_2d_array,
        /// 'texture_storage_3d'
        keyword_texture_storage_3d,
        /// 'true'
        keyword_true,
        /// 'type'
        keyword_type,
        /// 'u32'
        keyword_u32,
        /// 'var'
        keyword_var,
        /// 'vec2'
        keyword_vec2,
        /// 'vec3'
        keyword_vec3,
        /// 'vec4'
        keyword_vec4,
        /// 'while'
        keyword_while,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .eof,
                .invalid,
                .identifier,
                .float,
                .float_f,
                .float_h,
                .int,
                .int_i,
                .int_u,
                => null,
                .@"and" => "&",
                .and_and => "&&",
                .arrow => "->",
                .attr => "@",
                .division => "/",
                .bang => "!",
                .brace_left => "{",
                .brace_right => "}",
                .bracket_left => "[",
                .bracket_right => "]",
                .colon => ":",
                .comma => ",",
                .equal => "=",
                .equal_equal => "==",
                .greater_than => ">",
                .greater_than_equal => ">=",
                .shift_right => ">>",
                .less_than => "<",
                .less_than_equal => "<=",
                .shift_left => "<<",
                .mod => "%",
                .minus => "-",
                .minus_minus => "--",
                .not_equal => "!=",
                .period => ".",
                .plus => "+",
                .plus_plus => "++",
                .@"or" => "|",
                .or_or => "||",
                .paren_left => "(",
                .paren_right => ")",
                .semicolon => ";",
                .star => "*",
                .tilde => "~",
                .underscore => "_",
                .xor => "^",
                .plus_equal => "+=",
                .minus_equal => "-=",
                .times_equal => "*=",
                .division_equal => "/=",
                .modulo_equal => "%=",
                .and_equal => "&=",
                .or_equal => "|=",
                .xor_equal => "^=",
                .shift_right_equal => ">>=",
                .shift_left_equal => "<<=",
                .keyword_array => "array",
                .keyword_atomic => "atomic",
                .keyword_bitcast => "bitcast",
                .keyword_bool => "bool",
                .keyword_break => "break",
                .keyword_case => "case",
                .keyword_const => "const",
                .keyword_continue => "continue",
                .keyword_continuing => "continuing",
                .keyword_discard => "discard",
                .keyword_default => "default",
                .keyword_else => "else",
                .keyword_enable => "enable",
                .keyword_f16 => "f16",
                .keyword_f32 => "f32",
                .keyword_fallthrough => "fallthrough",
                .keyword_false => "false",
                .keyword_fn => "fn",
                .keyword_for => "for",
                .keyword_i32 => "i32",
                .keyword_if => "if",
                .keyword_let => "let",
                .keyword_loop => "loop",
                .keyword_mat2x2 => "mat2x2",
                .keyword_mat2x3 => "mat2x3",
                .keyword_mat2x4 => "mat2x4",
                .keyword_mat3x2 => "mat3x2",
                .keyword_mat3x3 => "mat3x3",
                .keyword_mat3x4 => "mat3x4",
                .keyword_mat4x2 => "mat4x2",
                .keyword_mat4x3 => "mat4x3",
                .keyword_mat4x4 => "mat4x4",
                .keyword_override => "override",
                .keyword_ptr => "ptr",
                .keyword_return => "return",
                .keyword_sampler => "sampler",
                .keyword_comparison_sampler => "sampler_comparison",
                .keyword_static_assert => "static_assert",
                .keyword_struct => "struct",
                .keyword_switch => "switch",
                .keyword_texture_depth_2d => "texture_depth_2d",
                .keyword_texture_depth_2d_array => "texture_depth_2d_array",
                .keyword_texture_depth_cube => "texture_depth_cube",
                .keyword_texture_depth_cube_array => "texture_depth_cube_array",
                .keyword_texture_depth_multisampled_2d => "texture_depth_multisampled_2d",
                .keyword_texture_external => "texture_external",
                .keyword_texture_multisampled_2d => "texture_multisampled_2d",
                .keyword_texture_sampled_1d => "texture_1d",
                .keyword_texture_sampled_2d => "texture_2d",
                .keyword_texture_sampled_2d_array => "texture_2d_array",
                .keyword_texture_sampled_3d => "texture_3d",
                .keyword_texture_sampled_cube => "texture_cube",
                .keyword_texture_sampled_cube_array => "texture_cube_array",
                .keyword_texture_storage_1d => "texture_storage_1d",
                .keyword_texture_storage_2d => "texture_storage_2d",
                .keyword_texture_storage_2d_array => "texture_storage_2d_array",
                .keyword_texture_storage_3d => "texture_storage_3d",
                .keyword_true => "true",
                .keyword_type => "type",
                .keyword_u32 => "u32",
                .keyword_var => "var",
                .keyword_vec2 => "vec2",
                .keyword_vec3 => "vec3",
                .keyword_vec4 => "vec4",
                .keyword_while => "while",
            };
        }
    };

    pub const keywords = std.ComptimeStringMap(Tag, .{
        .{ "array", .keyword_array },
        .{ "atomic", .keyword_atomic },
        .{ "bitcast", .keyword_bitcast },
        .{ "bool", .keyword_bool },
        .{ "break", .keyword_break },
        .{ "case", .keyword_case },
        .{ "const", .keyword_const },
        .{ "continue", .keyword_continue },
        .{ "continuing", .keyword_continuing },
        .{ "discard", .keyword_discard },
        .{ "default", .keyword_default },
        .{ "else", .keyword_else },
        .{ "enable", .keyword_enable },
        .{ "f16", .keyword_f16 },
        .{ "f32", .keyword_f32 },
        .{ "fallthrough", .keyword_fallthrough },
        .{ "false", .keyword_false },
        .{ "fn", .keyword_fn },
        .{ "for", .keyword_for },
        .{ "i32", .keyword_i32 },
        .{ "if", .keyword_if },
        .{ "let", .keyword_let },
        .{ "loop", .keyword_loop },
        .{ "mat2x2", .keyword_mat2x2 },
        .{ "mat2x3", .keyword_mat2x3 },
        .{ "mat2x4", .keyword_mat2x4 },
        .{ "mat3x2", .keyword_mat3x2 },
        .{ "mat3x3", .keyword_mat3x3 },
        .{ "mat3x4", .keyword_mat3x4 },
        .{ "mat4x2", .keyword_mat4x2 },
        .{ "mat4x3", .keyword_mat4x3 },
        .{ "mat4x4", .keyword_mat4x4 },
        .{ "override", .keyword_override },
        .{ "ptr", .keyword_ptr },
        .{ "return", .keyword_return },
        .{ "sampler", .keyword_sampler },
        .{ "sampler_comparison", .keyword_comparison_sampler },
        .{ "static_assert", .keyword_static_assert },
        .{ "struct", .keyword_struct },
        .{ "switch", .keyword_switch },
        .{ "texture_depth_2d", .keyword_texture_depth_2d },
        .{ "texture_depth_2d_array", .keyword_texture_depth_2d_array },
        .{ "texture_depth_cube", .keyword_texture_depth_cube },
        .{ "texture_depth_cube_array", .keyword_texture_depth_cube_array },
        .{ "texture_depth_multisampled_2d", .keyword_texture_depth_multisampled_2d },
        .{ "texture_external", .keyword_texture_external },
        .{ "texture_multisampled_2d", .keyword_texture_multisampled_2d },
        .{ "texture_1d", .keyword_texture_sampled_1d },
        .{ "texture_2d", .keyword_texture_sampled_2d },
        .{ "texture_2d_array", .keyword_texture_sampled_2d_array },
        .{ "texture_3d", .keyword_texture_sampled_3d },
        .{ "texture_cube", .keyword_texture_sampled_cube },
        .{ "texture_cube_array", .keyword_texture_sampled_cube_array },
        .{ "texture_storage_1d", .keyword_texture_storage_1d },
        .{ "texture_storage_2d", .keyword_texture_storage_2d },
        .{ "texture_storage_2d_array", .keyword_texture_storage_2d_array },
        .{ "texture_storage_3d", .keyword_texture_storage_3d },
        .{ "true", .keyword_true },
        .{ "type", .keyword_type },
        .{ "u32", .keyword_u32 },
        .{ "var", .keyword_var },
        .{ "vec2", .keyword_vec2 },
        .{ "vec3", .keyword_vec3 },
        .{ "vec4", .keyword_vec4 },
        .{ "while", .keyword_while },
    });
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    const State = enum {
        start,
        invalid,
        identifier,
        underscore,
        number,
        float,
        int_i,
        int_u,
        float_f,
        float_h,
        block_comment,
        ampersand,
        bang,
        equal,
        greater,
        shift_right,
        less,
        shift_left,
        minus,
        mod,
        pipe,
        plus,
        slash,
        star,
        xor,
    };

    pub fn dump(self: *Tokenizer, token: Token) void {
        std.debug.print("\x1b[0;33m{s} \x1b[0;90m\"{s}\"\x1b[0m\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // Skip the UTF-8 BOM if present
        const src_start: usize = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
        return Tokenizer{
            .buffer = buffer[src_start..],
            .index = 0,
        };
    }

    pub fn next(self: *Tokenizer) Token {
        var state: State = .start;
        var result = Token{
            .tag = .eof,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        while (true) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            result.tag = .invalid;
                            result.loc.start = self.index;
                            self.index += 1;
                            result.loc.end = self.index;
                            return result;
                        }
                        break;
                    },
                    ' ', '\n', '\t', '\r' => result.loc.start = self.index + 1,
                    'a'...'z', 'A'...'Z' => state = .identifier,
                    '0'...'9' => state = .number,

                    '&' => state = .ampersand,
                    '!' => state = .bang,
                    '=' => state = .equal,
                    '>' => state = .greater,
                    '<' => state = .less,
                    '-' => state = .minus,
                    '%' => state = .mod,
                    '|' => state = .pipe,
                    '+' => state = .plus,
                    '/' => state = .slash,
                    '*' => state = .star,
                    '_' => state = .underscore,
                    '^' => state = .xor,

                    '@' => {
                        result.tag = .attr;
                        self.index += 1;
                        break;
                    },
                    '[' => {
                        result.tag = .bracket_left;
                        self.index += 1;
                        break;
                    },
                    ']' => {
                        result.tag = .bracket_right;
                        self.index += 1;
                        break;
                    },
                    '{' => {
                        result.tag = .brace_left;
                        self.index += 1;
                        break;
                    },
                    '}' => {
                        result.tag = .brace_right;
                        self.index += 1;
                        break;
                    },
                    ':' => {
                        result.tag = .colon;
                        self.index += 1;
                        break;
                    },
                    ',' => {
                        result.tag = .comma;
                        self.index += 1;
                        break;
                    },
                    '(' => {
                        result.tag = .paren_left;
                        self.index += 1;
                        break;
                    },
                    ')' => {
                        result.tag = .paren_right;
                        self.index += 1;
                        break;
                    },
                    '.' => {
                        result.tag = .period;
                        self.index += 1;
                        break;
                    },
                    ';' => {
                        result.tag = .semicolon;
                        self.index += 1;
                        break;
                    },
                    '~' => {
                        result.tag = .tilde;
                        self.index += 1;
                        break;
                    },

                    else => {
                        state = .invalid;
                        result.tag = .invalid;
                    },
                },
                .invalid => break,

                .identifier => switch (c) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                    else => {
                        result.tag = .identifier;
                        if (Token.keywords.get(self.buffer[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                },
                .underscore => switch (c) { // TODO: two underscore `__` https://www.w3.org/TR/WGSL/#identifiers
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => state = .identifier,
                    else => {
                        result.tag = .underscore;
                        break;
                    },
                },

                .number => switch (c) {
                    '0'...'9' => {},
                    '.' => state = .float,
                    'i' => state = .int_i,
                    'u' => state = .int_u,
                    'f' => state = .float_f,
                    'h' => state = .float_h,
                    else => {
                        result.tag = .int;
                        break;
                    },
                },
                .float => switch (c) {
                    '0'...'9', '.' => {},
                    'i' => state = .int_i,
                    'u' => state = .int_u,
                    'f' => state = .float_f,
                    'h' => state = .float_h,
                    else => {
                        result.tag = .float;
                        break;
                    },
                },
                .int_i => switch (c) {
                    'i' => {},
                    else => {
                        result.tag = .int_i;
                        break;
                    },
                },
                .int_u => switch (c) {
                    'u' => {},
                    else => {
                        result.tag = .int_u;
                        break;
                    },
                },
                .float_f => switch (c) {
                    'f' => {},
                    else => {
                        result.tag = .float_f;
                        break;
                    },
                },
                .float_h => switch (c) {
                    'h' => {},
                    else => {
                        result.tag = .float_h;
                        break;
                    },
                },

                .block_comment => switch (c) {
                    0 => break,
                    '\n' => {
                        state = .start;
                        result.loc.start = self.index + 1;
                    },
                    else => {},
                },

                .ampersand => switch (c) {
                    '&' => {
                        result.tag = .and_and;
                        self.index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .and_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"and";
                        break;
                    },
                },
                .bang => switch (c) {
                    '=' => {
                        result.tag = .not_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .bang;
                        break;
                    },
                },
                .equal => switch (c) {
                    '=' => {
                        result.tag = .equal_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .equal;
                        break;
                    },
                },
                .greater => switch (c) {
                    '>' => state = .shift_right,
                    '=' => {
                        result.tag = .greater_than_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .greater_than;
                        break;
                    },
                },
                .shift_right => switch (c) {
                    '=' => {
                        result.tag = .shift_right_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .shift_right;
                        break;
                    },
                },
                .less => switch (c) {
                    '<' => state = .shift_left,
                    '=' => {
                        result.tag = .less_than_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .less_than;
                        break;
                    },
                },
                .shift_left => switch (c) {
                    '=' => {
                        result.tag = .shift_left_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .shift_left;
                        break;
                    },
                },
                .minus => switch (c) {
                    '-' => {
                        result.tag = .minus_minus;
                        self.index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .minus_equal;
                        self.index += 1;
                        break;
                    },
                    '>' => {
                        result.tag = .arrow;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .minus;
                        break;
                    },
                },
                .mod => switch (c) {
                    '=' => {
                        result.tag = .modulo_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .mod;
                        break;
                    },
                },
                .pipe => switch (c) {
                    '|' => {
                        result.tag = .or_or;
                        self.index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .or_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .@"or";
                        break;
                    },
                },
                .plus => switch (c) {
                    '+' => {
                        result.tag = .plus_plus;
                        self.index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .plus_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .plus;
                        break;
                    },
                },
                .slash => switch (c) {
                    '/' => state = .block_comment,
                    '=' => {
                        result.tag = .division_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .division;
                        break;
                    },
                },
                .star => switch (c) {
                    '=' => {
                        result.tag = .times_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .star;
                        break;
                    },
                },
                .xor => switch (c) {
                    '=' => {
                        result.tag = .xor_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .xor;
                        break;
                    },
                },
            }
        }

        result.loc.end = self.index;
        return result;
    }
};

const expect = std.testing.expect;
test "tokenize identifier and numbers" {
    comptime var str: [:0]const u8 =
        \\_ __ _iden iden 100u 100.8i 2.000f 10.0 // cc
        \\// commnet
        \\
    ;
    var tokenizer = Tokenizer.init(str);
    try expect(tokenizer.next().tag == .underscore);
    try expect(tokenizer.next().tag == .identifier);
    try expect(tokenizer.next().tag == .identifier);
    try expect(tokenizer.next().tag == .identifier);
    try expect(tokenizer.next().tag == .int_u);
    try expect(tokenizer.next().tag == .int_i);
    try expect(tokenizer.next().tag == .float_f);
    try expect(tokenizer.next().tag == .float);
    try expect(tokenizer.next().tag == .eof);
}

test "tokenize other" {
    comptime var str: [:0]const u8 = "";
    inline for (std.meta.fields(Token.Tag)) |field| comptime {
        str = str ++ " " ++ (Token.Tag.lexeme(@intToEnum(Token.Tag, field.value)) orelse "");
    };

    var tokenizer = Tokenizer.init(str);

    comptime var i = 9; // skip identifiers and nums
    inline while (i < std.meta.fields(Token.Tag).len) : (i += 1) {
        const tag = @intToEnum(Token.Tag, i);
        try expect(tokenizer.next().tag == tag);
    }
    try expect(tokenizer.next().tag == .eof);
}
