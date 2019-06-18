package crest

using import "core:fmt"
      import "core:os"

using import "shared:workbench/logging"
using import "shared:workbench/basic"

Token_Type :: enum {
	None,
	Eof,

	Ident,
	Comment,
	Block_Comment_Start,
	Block_Comment_End,

	// Keywords
	Null,
	If,
	Else,
	For,
	While,
	Proc,
	Var,
	Return,
	In,
	Break,
	Continue,
	Enum,
	Struct_Keyword,
	Union,
	Const,
	Sizeof,
	Cast,
	Do,
	Switch,
	Case,

	// Directives
	Directive_Odin_Proc,
	Directive_Include,
	Directive_Assert,
	Directive_Type,

	Integer_Literal,
	Float_Literal,
	String_Literal,

	// Delims
	Colon,
	Semicolon,
	Dot,
	Comma,
	Arrow,

	Left_Paren,
	Right_Paren,
	Left_Curly,
	Right_Curly,
	Left_Square,
	Right_Square,

	Operator,
}

Operator :: enum {
	Multiply,                      // *
	MULTIPLICATIVE_BEGIN = Multiply,
	Divide,                        // /
	Mod,                           // %
	Mod_Mod,                       // %%
	Bit_And,                       // &
	Bit_Shift_Left,                // <<
	Bit_Shift_Right,               // >>
	MULTIPLICATIVE_END = Bit_Shift_Right,

	Plus,                          // +
	ADDITIVE_BEGIN = Plus,
	Minus,                         // -
	Bit_Xor,                       // ^
	Bit_Or,                        // |
	ADDITIVE_END = Bit_Or,

	Boolean_Equal,                 // ==
	COMPARATIVE_BEGIN = Boolean_Equal,
	Boolean_Not_Equal,             // !=
	Boolean_Less_Than,             // <
	Boolean_Greater_Than,          // >
	Boolean_Less_Than_Or_Equal,    // <=
	Boolean_Greater_Than_Or_Equal, // >=
	COMPARATIVE_END = Boolean_Greater_Than_Or_Equal,

	Boolean_Or,                    // ||
	Boolean_And,                   // &&

	Boolean_Not,                   // !
	Bit_Not,                       // ~

	Caret,                         // &    note(josh): at lex time we don't know if a ^ is a bitwise XOR or a var deref   (^var), so we use this in lexing and then the parser will overwrite it with the correct value (Bit_Xor or Dereference)
	Ampersand,                     // &    note(josh): at lex time we don't know if a & is a bitwise AND or a var address (&var), so we use this in lexing and then the parser will overwrite it with the correct value (Bit_And or Address)

	Address,                       // &var
	Dereference,                   // ^var

	Dot_Dot,                       // ..

	Assign,                        // =
	ASSIGN_BEGIN = Assign,
	Plus_Assign,                   // +=
	Minus_Assign,                  // -=
	Multiply_Assign,               // *=
	Divide_Assign,                 // /=
	Mod_Assign,                    // %=
	Or_Assign,                     // |=
	Bit_And_Assign,                // &=
	Bit_Xor_Assign,                // ^=
	Bit_Shift_Left_Assign,         // <<=
	Bit_Shift_Right_Assign,        // >>=
	ASSIGN_END = Bit_Shift_Right_Assign,
}

Token :: struct {
	kind: Token_Type,
	operator: Operator,
	site: Site,
	text: string,
}

Lexer :: struct {
	filename:     string,
	program_text: string,

	lex_idx:  int,
	lex_char: int,
	lex_line: int,
}

lexers: [dynamic]Lexer;

push_new_lexer_text :: proc(filename, text: string) {
	bytes := make([]byte, len(text)+1);
	copy(bytes, cast([]byte)text);
	bytes[len(text)] = 0;
	program_text := cast(string)bytes[:];

	lexer := Lexer{filename, program_text, 0, 1, 1};
	append(&lexers, lexer);
}

pop_lexer :: proc() {
	pop(&lexers);
}

is_letter :: proc(r: u8) -> bool {
	switch cast(rune)r {
		case 'a'..'z', 'A'..'Z': {
			return true;
		}
	}
	return false;
}

get_current_lexer :: proc() -> ^Lexer{
	return &lexers[len(lexers)-1];
}

// kinda slow since we will end up lexing peeked tokens more than once
peek :: proc() -> Token {
	lexer := get_current_lexer();
	using lexer;

	idx := lex_idx;
	char := lex_char;
	line := lex_line;

	token := next_token();
	lex_idx = idx;
	lex_char = char;
	lex_line = line;
	return token;
}

// todo: remove this
dec :: proc() {
	lexer := get_current_lexer();
	using lexer;

	lex_idx -= 1;
	lex_char -= 1;
}

inc :: proc() {
	lexer := get_current_lexer();
	using lexer;

	r := program_text[lex_idx];
	lex_idx += 1;

	if r == '\n' {
		lex_char = 1;
		lex_line += 1;
	}
	else if r == '\t' {
		lex_char += 4;
	}
	else {
		lex_char += 1;
	}
}

next_token :: proc(loc := #caller_location) -> Token {
	lexer := get_current_lexer();
	using lexer;

	if is_whitespace(program_text[lex_idx]) {
		for is_whitespace(program_text[lex_idx]) {
			inc();
		}
	}

	token_type: Token_Type;
	token_operator: Operator;
	token_text: string;

	token_start_char := lex_char;
	token_start_line := lex_line;

	switch (cast(rune)program_text[lex_idx]) {
		case '\x00': {
			token_type = .Eof;
		}
		case '#': {
			start := lex_idx;
			for is_letter(program_text[lex_idx+1]) {
				inc();
			}

			token_text = program_text[start:lex_idx+1];
			switch token_text {
				case "#odin":    token_type = .Directive_Odin_Proc;
				case "#include": token_type = .Directive_Include;
				case "#assert":  token_type = .Directive_Assert;
				case "#type":    token_type = .Directive_Type;
				case: {
					logln("Invalid hash directive: ", token_text);
					return Token{};
				}
			}
		}
		case ';': {
			token_type = .Semicolon;
			token_text = ";";
		}
		case ':': {
			token_type = .Colon;
			token_text = ":";
		}
		case ',': {
			token_type = .Comma;
			token_text = ",";
		}
		case '(': {
			token_type = .Left_Paren;
			token_text = "(";
		}
		case ')': {
			token_type = .Right_Paren;
			token_text = ")";
		}
		case '{': {
			token_type = .Left_Curly;
			token_text = "{";
		}
		case '}': {
			token_type = .Right_Curly;
			token_text = "}";
		}
		case '[': {
			token_type = .Left_Square;
			token_text = "[";
		}
		case ']': {
			token_type = .Right_Square;
			token_text = "]";
		}
		case '.': {
			token_type = .Dot;
			token_text = ".";
			if program_text[lex_idx+1] == '.' {
				inc();
				token_type = .Operator;
				token_operator = .Dot_Dot;
				token_text = "..";
			}
		}
		case '=': {
			token_type = .Operator;
			token_operator = .Assign;
			token_text = "=";
			if program_text[lex_idx+1] == '=' {
				inc();
				token_type = .Operator;
				token_operator = .Boolean_Equal;
				token_text = "==";
			}
		}
		case '!': {
			token_type = .Operator;
			token_operator = .Boolean_Not;
			token_text = "!";
			if program_text[lex_idx+1] == '=' {
				inc();
				token_type = .Operator;
				token_operator = .Boolean_Not_Equal;
				token_text = "!=";
			}
		}
		case '+': {
			token_type = .Operator;
			token_operator = .Plus;
			token_text = "+";
			if program_text[lex_idx+1] == '=' {
				inc();
				token_type = .Operator;
				token_operator = .Plus_Assign;
				token_text = "+=";
			}
		}
		case '-': {
			token_type = .Operator;
			token_operator = .Minus;
			token_text = "-";
			if program_text[lex_idx+1] == '=' {
				inc();
				token_type = .Operator;
				token_operator = .Minus_Assign;
				token_text = "-=";
			}
			else if program_text[lex_idx+1] == '>' {
				inc();
				token_type = .Arrow;
				token_text = "->";
			}
		}
		case '*': {
			token_type = .Operator;
			token_operator = .Multiply;
			token_text = "*";
			if program_text[lex_idx+1] == '=' {
				inc();
				token_type = .Operator;
				token_operator = .Multiply_Assign;
				token_text = "*=";
			}
		}
		case '/': {
			if program_text[lex_idx+1] == '/' {
				token_type = .Comment;
				start := lex_idx;
				for program_text[lex_idx] != '\n' && program_text[lex_idx] != '\x00' {
					inc();
				}

				token_text = program_text[start:lex_idx];
			}
			else if program_text[lex_idx+1] == '*' {
				token_type = .Comment;
				start := lex_idx;

				inc();
				inc();
				num_nested := 1;
				for true {
					if program_text[lex_idx] == '/' && program_text[lex_idx+1] == '*' {
						num_nested += 1;
					}
					else if program_text[lex_idx] == '*' && program_text[lex_idx+1] == '/' {
						num_nested -= 1;
					}

					inc();
					if num_nested == 0 {
						break;
					}
				}

				inc();
				token_text = program_text[start:lex_idx];
			}
			else {
				token_type = .Operator;
				token_operator = .Divide;
				token_text = "/";
				if program_text[lex_idx+1] == '=' {
					inc();
					token_type = .Operator;
					token_operator = .Divide_Assign;
					token_text = "/=";
				}
			}
		}
		case '%': {
			token_type = .Operator;
			token_operator = .Mod;
			token_text = "%";
			if program_text[lex_idx+1] == '=' {
				inc();
				token_type = .Operator;
				token_operator = .Mod_Assign;
				token_text = "%=";
			}
			else if program_text[lex_idx+1] == '%' {
				inc();
				token_type = .Operator;
				token_operator = .Mod_Mod;
				token_text = "%%";
			}
		}
		case '^': {
			token_type = .Operator;
			token_operator = .Caret;
			token_text = "^";
			if program_text[lex_idx+1] == '=' {
				inc();
				token_type = .Operator;
				token_operator = .Bit_Xor_Assign;
				token_text = "^=";
			}
		}
		case '<': {
			token_type = .Operator;
			token_operator = .Boolean_Less_Than;
			token_text = "<";
			inc();
			switch (cast(rune)program_text[lex_idx]) {
				case '=': {
					token_type = .Operator;
					token_operator = .Boolean_Less_Than_Or_Equal;
					token_text = "<=";
				}
				case '<': {
					token_type = .Operator;
					token_operator = .Bit_Shift_Left;
					token_text = "<<";
					if program_text[lex_idx+1] == '=' {
						inc();
						token_type = .Operator;
						token_operator = .Bit_Shift_Left_Assign;
						token_text = "<<=";
					}
				}
				case: {
					dec();
				}
			}
		}
		case '>': {
			token_type = .Operator;
			token_operator = .Boolean_Greater_Than;
			token_text = ">";
			inc();
			switch (cast(rune)program_text[lex_idx]) {
				case '=': {
					token_type = .Operator;
					token_operator = .Boolean_Greater_Than_Or_Equal;
					token_text = ">=";
				}
				case '>': {
					token_type = .Operator;
					token_operator = .Bit_Shift_Right;
					token_text = ">>";
					if program_text[lex_idx+1] == '=' {
						inc();
						token_type = .Operator;
						token_operator = .Bit_Shift_Right_Assign;
						token_text = ">>=";
					}
				}
				case: {
					dec();
				}
			}
		}
		case '|': {
			token_type = .Operator;
			token_operator = .Bit_Or;
			token_text = "|";
			inc();
			switch (cast(rune)program_text[lex_idx]) {
				case '=': {
					token_type = .Operator;
					token_operator = .Or_Assign;
					token_text = "|=";
				}
				case '|': {
					token_type = .Operator;
					token_operator = .Boolean_Or;
					token_text = "||";
				}
				case: {
					dec();
				}
			}
		}
		case '&': {
			token_type = .Operator;
			token_operator = .Ampersand;
			token_text = "&";
			inc();
			switch (cast(rune)program_text[lex_idx]) {
				case '=': {
					token_type = .Operator;
					token_operator = .Bit_And_Assign;
					token_text = "&=";
				}
				case '&': {
					token_type = .Operator;
					token_operator = .Boolean_And;
					token_text = "&&";
				}
				case: {
					dec();
				}
			}
		}
		case '"': {
			token_type = .String_Literal;

			inc();
			start := lex_idx;
			escaped := false;
			for program_text[lex_idx] != '"' || escaped {
				escaped = program_text[lex_idx] == '\\';
				inc();
			}

			token_text = program_text[start:lex_idx];
		}
		case: {
			if is_letter(program_text[lex_idx]) || program_text[lex_idx] == '_' {
				start := lex_idx;
				for is_letter(program_text[lex_idx]) || program_text[lex_idx] == '_' || is_digit(program_text[lex_idx]) {
					inc();
				}
				token_text = program_text[start:lex_idx];
				dec();

				switch token_text {
					case "null":        { token_type = .Null; }
					case "if":          { token_type = .If; }
					case "else":        { token_type = .Else; }
					case "for":         { token_type = .For; }
					case "while":       { token_type = .While; }
					case "proc":        { token_type = .Proc; }
					case "var":         { token_type = .Var; }
					case "return":      { token_type = .Return; }
					case "in":          { token_type = .In; }
					case "break":       { token_type = .Break; }
					case "continue":    { token_type = .Continue; }
					case "enum":        { token_type = .Enum; }
					case "struct":      { token_type = .Struct_Keyword; }
					case "union":       { token_type = .Union; }
					case "const":       { token_type = .Const; }
					case "sizeof":      { token_type = .Sizeof; }
					case "cast":        { token_type = .Cast; }
					case "do":          { token_type = .Do; }
					case "switch":      { token_type = .Switch; }
					case "case":        { token_type = .Case; }
					case:               { token_type = .Ident; }
				}
			}
			else if is_digit(program_text[lex_idx]) {
				token_type = .Integer_Literal;

				start := lex_idx;
				// todo: handle case with two dots in a float
				for is_digit(program_text[lex_idx]) || (program_text[lex_idx] == '.') {
					if program_text[lex_idx] == '.' {
						token_type = .Float_Literal;
					}

					inc();
				}

				token_text = program_text[start:lex_idx];
				dec();
			}
			else {
				fmt.println("Unknown token:", cast(rune)program_text[lex_idx], "at line", token_start_line, "column", token_start_char);
				assert(false);
			}
		}
	}

	inc();

	assert(token_type != .None);
	token := Token{token_type, token_operator, Site{lexer.filename, token_start_line, token_start_char}, intern_string(token_text)};

	return token;
}

interned_strings := make([dynamic]^string, 0, 10);

intern_string :: proc(str: string) -> string {
	for interned in interned_strings {
		if interned^ == str {
			return interned^;
		}
	}

	interned_str := new_clone(str);
	append(&interned_strings, interned_str);
	return interned_str^;
}