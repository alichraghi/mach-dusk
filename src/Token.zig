const std = @import("std");

tag: Tag,
loc: Loc,

pub const Loc = struct {
    start: u32,
    end: u32,

    pub const Extra = struct {
        line: u32,
        col: u32,
        line_start: u32,
        line_end: u32,
    };

    pub fn asStr(self: Loc, source: []const u8) []const u8 {
        return source[self.start..self.end];
    }

    pub fn extraInfo(self: Loc, source: []const u8) Extra {
        var result = Extra{
            .line = 1,
            .col = 1,
            .line_start = 0,
            .line_end = @intCast(u32, source.len),
        };

        for (source[0..self.start]) |c, i| {
            if (c == '\n') {
                result.line += 1;
                result.line_start = @intCast(u32, i) + 1;
            }
        }

        for (source[self.end..]) |c, i| {
            if (c == '\n') {
                result.line_end = self.end + @intCast(u32, i);
                break;
            }
        }

        result.col += self.start - result.line_start;
        return result;
    }
};

pub const Tag = enum {
    eof,
    invalid,

    ident,
    /// any number literal
    number,

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

    pub fn lexeme(self: Tag) ?[]const u8 {
        return switch (self) {
            .eof,
            .invalid,
            .ident,
            .number,
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

    pub fn symbol(self: Tag) []const u8 {
        return self.lexeme() orelse switch (self) {
            .eof => "EOF",
            .invalid => "invalid bytes",
            .ident => "an identifier",
            .number => "a number literal",
            else => unreachable,
        };
    }

    pub const Group = enum {
        ident,
        scalar,
        literal,
        vector,
        matrix,
        sampler,
        array,
        atomic,
        other,
    };

    pub fn group(self: Tag) Group {
        return switch (self) {
            .ident => .ident,

            .keyword_i32,
            .keyword_u32,
            .keyword_f32,
            .keyword_f16,
            .keyword_bool,
            => .scalar,

            .number,
            .keyword_true,
            .keyword_false,
            => .literal,

            .keyword_vec2,
            .keyword_vec3,
            .keyword_vec4,
            => .vector,

            .keyword_mat2x2,
            .keyword_mat2x3,
            .keyword_mat2x4,
            .keyword_mat3x2,
            .keyword_mat3x3,
            .keyword_mat3x4,
            .keyword_mat4x2,
            .keyword_mat4x3,
            .keyword_mat4x4,
            => .matrix,

            .keyword_sampler,
            .keyword_comparison_sampler,
            => .sampler,

            .keyword_array => .array,

            .keyword_atomic => .atomic,

            else => .other,
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
