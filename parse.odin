package crest

using import "core:fmt"
      import rt "core:runtime"
	  import "core:os"

using import "shared:workbench/logging"

Ast_Typespec_Ptr :: struct {
	using base: ^Ast_Node,
	typespec: ^Ast_Node,
}

Ast_Typespec_Array :: struct {
	using base: ^Ast_Node,
	size: int,
	typespec: ^Ast_Node,
}

Ast_Typespec_Dynamic_Array :: struct {
	using base: ^Ast_Node,
	typespec: ^Ast_Node,
}

Ast_Typespec_Slice :: struct {
	using base: ^Ast_Node,
	typespec: ^Ast_Node,
}

Ast_Typespec_Union :: struct {
	using base: ^Ast_Node,
	types: [dynamic]^Ast_Node,
}

Ast_Directive :: struct {
	using base: ^Ast_Node,
	directive: string,
}

Ast_Directive_Include :: struct {
	using base: ^Ast_Node,
	filename: string,
}

Ast_Comment :: struct {
	using base: ^Ast_Node,
	text: string,
}

Ast_Proc :: struct {
	using base: ^Ast_Node,
	name: string,
	params: [dynamic]^Ast_Var,
	return_typespec: ^Ast_Node,
	flags: u32,
	block: ^Ast_Block,

	return_type: ^Type,
	sym: ^Symbol,
}

Ast_Var :: struct {
	using base: ^Ast_Node,
	name: string,
	typespec: ^Ast_Node,
	expr: ^Ast_Node,
	sym: ^Symbol,
}

Ast_Struct :: struct {
	using base: ^Ast_Node,
	name: string,
	fields: [dynamic]^Ast_Var,
	sym: ^Symbol,
}

Ast_Identifier :: struct {
	using base: ^Ast_Node,
	name: string,
	sym: ^Symbol,
}

Ast_Assign :: struct {
	using base: ^Ast_Node,
	op: Token_Type,
	left: ^Ast_Node,
	right: ^Ast_Node,
}

Ast_If :: struct {
	using base: ^Ast_Node,
	condition: ^Ast_Node,
	block: ^Ast_Block,
	else_ifs: [dynamic]^Ast_Else_If,
	else_block: ^Ast_Block,
}
Ast_Else_If :: struct {
	using base: ^Ast_Node,
	condition: ^Ast_Node,
	block: ^Ast_Block,
}

Ast_For_I :: struct {
	using base: ^Ast_Node,
	var: ^Ast_Var,
	condition: ^Ast_Node,
	post_stmt: ^Ast_Node,
	block: ^Ast_Block,
}
Ast_For_Each :: struct {
	using base: ^Ast_Node,
	var: ^Ast_Var,
	array: ^Ast_Node,
	block: ^Ast_Block,
}
Ast_While :: struct {
	using base: ^Ast_Node,
	condition: ^Ast_Node,
	block: ^Ast_Block,
}

Ast_Return :: struct {
	using base: ^Ast_Node,
	procedure: ^Ast_Proc,
	expr: ^Ast_Node,
}

Ast_Block :: struct {
	using base: ^Ast_Node,
	stmts: [dynamic]^Ast_Node,
	symbols: [dynamic]^Symbol,
}

Ast_Call :: struct {
	using base: ^Ast_Node,
	procedure: ^Ast_Node,
	params: [dynamic]^Ast_Node,
}

Ast_Unary :: struct {
	using base: ^Ast_Node,
	op: Token,
	rhs: ^Ast_Node,
}

Ast_Range :: struct {
	using base: ^Ast_Node,
	lhs: ^Ast_Node,
	min: ^Ast_Node,
	max: ^Ast_Node,
}

Ast_Binary :: struct {
	using base: ^Ast_Node,
	op: Token,
	lhs: ^Ast_Node,
	rhs: ^Ast_Node,
}

Ast_Cast :: struct {
	using base: ^Ast_Node,
	typespec: ^Ast_Node,
	rhs: ^Ast_Node,
}

Ast_Sizeof :: struct {
	using base: ^Ast_Node,
	typespec: ^Ast_Node,
}

Ast_Subscript :: struct {
	using base: ^Ast_Node,
	left: ^Ast_Node,
	index: ^Ast_Node,
}

Ast_Slice :: struct {
	using base: ^Ast_Node,
	array: ^Ast_Node,
	range: ^Ast_Range,
}

Ast_Selector :: struct {
	using base: ^Ast_Node,
	left: ^Ast_Node,
	field: string,
}

Ast_Paren :: struct {
	using base: ^Ast_Node,
	nested_expr: ^Ast_Node,
}

Ast_String :: struct {
	using base: ^Ast_Node,
	text: string,
}

Ast_Number :: struct {
	using base: ^Ast_Node,
	is_float: bool,
	int_number: i64,
	float_number: f64,
}

Ast_Null :: struct {
	using base: ^Ast_Node,
}

Ast_Node :: struct {
	derived: union {
		Ast_Directive_Include,
		Ast_Struct,
		Ast_Proc,
		Ast_Var,
		Ast_Directive,
		Ast_Comment,
		Ast_Assign,
		Ast_If,
		Ast_Else_If,
		Ast_For_I,
		Ast_For_Each,
		Ast_While,
		Ast_Return,
		Ast_Block,

		Ast_Identifier,
		Ast_Call,
		Ast_Unary,
		Ast_Range,
		Ast_Binary,
		Ast_Cast,
		Ast_Sizeof,
		Ast_Subscript,
		Ast_Slice,
		Ast_Selector,
		Ast_Paren,
		Ast_String,
		Ast_Number,
		Ast_Null,

		Ast_Typespec_Ptr,
		Ast_Typespec_Array,
		Ast_Typespec_Dynamic_Array,
		Ast_Typespec_Slice,
		Ast_Typespec_Union,
	},

	serial: int,
	site: Site,
	parent: ^Ast_Block,
	root_token: Token,
	check_state: Check_State,
	depends: [dynamic]^Ast_Node,
	inferred_type: ^Type,
}

Site :: struct {
	filename: string,
	line: int,
	column: int,
}

PROC_IS_ODIN_PROC : u32 : 1 << 0;

current_workspace: ^Workspace;

parse_workspace :: proc(workspace: ^Workspace, filename, text: string, loc := #caller_location) -> bool {
	assert(len(text) > 0);
	assert(workspace != nil);

	// Push new workspace
	old_workspace := current_workspace;
	current_workspace = workspace;
	defer current_workspace = old_workspace;

	old_block := current_block;
	block := node(Token{}, Ast_Block{{}, nil, nil});
	current_block = block;
	defer current_block = old_block;

	block.stmts = parse_text_to_stmt_list(filename, text);
	for s in block.stmts {
		depend(block, s);
	}

	workspace.syntax_tree = block;

	init_builtin_types(workspace);

	if !resolve_identifiers() {
		return false;
	}

	if !typecheck_workspace(workspace) {
		return false;
	}

	// gen_odin(workspace);
	return true;
}

parse_text_to_stmt_list :: proc(filename, text: string, loc := #caller_location) -> [dynamic]^Ast_Node {
	if !push_new_lexer_text(filename, text) {
		assert(false);
		return nil;
	}
	defer pop_lexer();

	list := parse_stmt_list();
	return list;
}

parse_stmt_list :: proc() -> [dynamic]^Ast_Node {
	using Token_Type;

	stmts: [dynamic]^Ast_Node;

	for !is_token(Eof) && !is_token(Right_Curly) {
		stmt := parse_stmt();
		if include, ok := stmt.derived.(Ast_Directive_Include); ok {
			bytes, ok := os.read_entire_file(include.filename);
			assert(ok, tprint("Couldn't open file ", include.filename));
			text := cast(string)bytes;
			include_stmts := parse_text_to_stmt_list(include.filename, text);
			for include_stmt in include_stmts {
				append(&stmts, include_stmt);
			}
		}
		else {
			append(&stmts, stmt);
		}
	}

	return stmts;
}


_alloc_node :: inline proc(token: Token, derived: $T, loc := #caller_location) -> ^T {
	@static last_serial: int;

	last_serial += 1;

	ptr := new(Ast_Node);
	derived.base = ptr;
	ptr^ = Ast_Node{
		derived,
		last_serial,
		token.site,
		current_block,
		token,
		Check_State.Unchecked,
		nil,
		nil,
	};

	return cast(^T)ptr;
}

node :: inline proc(token: Token, derived: $T, loc := #caller_location) -> ^T {
	ptr := _alloc_node(token, derived);
	append(&current_workspace.nodes_to_typecheck, ptr.base);
	return ptr;
}

depend :: inline proc(node: ^$T, depends_on: ^$S, loc := #caller_location) {
	assert(node != nil, tprint("node was nil at ", pretty_location(loc)));
	assert(depends_on != nil, tprint("depends_on was nil at ", pretty_location(loc)));
	append(&node.depends, depends_on);
	append(&current_workspace.all_depends, Depend_Entry{node, depends_on});
}

unexpected_token :: proc(token: Token, loc: rt.Source_Code_Location, expected: ..Token_Type) {
	print("Unexpected token: ", token.text, " at ", site(token.site), ".");

	if len(expected) > 0 {
		print(" Expected: ", expected, "\n");
	}
	else {
		print("\n");
	}

	println(loc);
}

is_token :: inline proc(kinds: ..Token_Type) -> bool {
	token := peek();
	for kind in kinds {
		if token.kind == kind {
			return true;
		}
	}

	return false;
}

expect :: proc(kinds: ..Token_Type, loc := #caller_location) -> Token {
	token := next_token(loc);
	for kind in kinds {
		if token.kind == kind {
			return token;
		}
	}

	unexpected_token(token, loc, ..kinds);
	assert(false);
	return {};
}






is_assign_op :: proc() -> bool {
	using Token_Type;
	kind := peek().kind;
	return cast(i32)kind > cast(i32)ASSIGN_BEGIN && cast(i32)kind < cast(i32)ASSIGN_END;
}

is_postfix_op :: proc() -> bool {
	using Token_Type;

	switch peek().kind {
		case Dot, Left_Paren, Left_Square: {
			return true;
		}
	}

	return false;
}

is_unary_op :: proc() -> bool {
	using Token_Type;
	switch peek().kind {
		case Plus, Minus, Xor, And, Not: {
			return true;
		}
	}

	return false;
}

is_mul_op :: proc() -> bool {
	using Token_Type;
	kind := peek().kind;
	return cast(i32)kind > cast(i32)MUL_BEGIN && cast(i32)kind < cast(i32)MUL_END;
}

is_add_op :: proc() -> bool {
	using Token_Type;
	kind := peek().kind;
	return cast(i32)kind > cast(i32)ADD_BEGIN && cast(i32)kind < cast(i32)ADD_END;
}

is_cmp_op :: proc() -> bool {
	using Token_Type;
	kind := peek().kind;
	return cast(i32)kind > cast(i32)CMP_BEGIN && cast(i32)kind < cast(i32)CMP_END;
}

is_ident_or_type_keyword :: proc(kind: Token_Type) -> bool {
	using Token_Type;

	switch kind {
		case Ident, BUILTIN_TYPES_BEGIN..BUILTIN_TYPES_END: {
			return true;
		}
	}

	return false;
}

parse_typespec :: proc() -> ^Ast_Node {
	using Token_Type;

	token := peek();
	if is_ident_or_type_keyword(token.kind) {
		token = next_token();
		symbol := node(token, Ast_Identifier{{}, token.text, nil});
		queue_identifier_for_resolving(symbol);
		return symbol.base;
	}

	if token.kind == Union {
		next_token();
		expect(Left_Curly);

		types: [dynamic]^Ast_Node;
		for peek().kind != Right_Curly && peek().kind != Eof {
			spec := parse_typespec();
			append(&types, spec);
			expect(Comma);
		}

		expect(Right_Curly);

		union_node := node(token, Ast_Typespec_Union{{}, types});

		// todo: factor this into the loop above
		for t in types {
			depend(union_node, t);
		}

		return union_node.base;
	}

	if token.kind == Proc {
		assert(false, "cannot do proc types yet");
	}

	modifier := expect(Left_Square, Xor);
	if modifier.kind == Left_Square {
		if peek().kind == Right_Square {
			next_token();
			typespec := parse_typespec();
			slice := node(token, Ast_Typespec_Slice{{}, typespec});
			depend(slice, typespec);
			return slice.base;
		}

		if peek().kind == Range {
			next_token();
			expect(Right_Square);
			typespec := parse_typespec();
			dynamic_array := node(token, Ast_Typespec_Dynamic_Array{{}, typespec});
			depend(dynamic_array, typespec);
			return dynamic_array.base;
		}

		array_length := parse_expr();

		len_number, ok := array_length.derived.(Ast_Number);
		assert(ok);
		assert(!len_number.is_float);

		expect(Right_Square);

		typespec := parse_typespec();
		array := node(token, Ast_Typespec_Array{{}, cast(int)len_number.int_number, typespec});
		depend(array, typespec);
		return array.base;
	}

	assert(modifier.kind == Xor);
	typespec := parse_typespec();
	ptr := node(token, Ast_Typespec_Ptr{{}, typespec});
	depend(ptr, typespec);
	return ptr.base;
}

import "core:strconv"

parse_operand :: proc() -> ^Ast_Node {
	using Token_Type;

	token := next_token();
	switch token.kind {
		case Null: {
			expr := node(token, Ast_Null{});
			return expr.base;
		}
		case Integer_Literal: {
			num := node(token, Ast_Number{{}, false, strconv.parse_i64(token.text), 0});
			return num.base;
		}
		case Float_Literal: {
			num := node(token, Ast_Number{{}, true, 0, strconv.parse_f64(token.text)});
			return num.base;
		}
		case String_Literal: {
			str := node(token, Ast_String{{}, token.text});
			return str.base;
		}
		case Left_Paren: {
			nested := parse_expr();
			node := node(token, Ast_Paren{{}, nested});
			expect(Right_Paren);
			depend(node, nested);
			return node.base;
		}
		case Sizeof: {
			expect(Left_Paren);
			typespec := parse_typespec();
			expect(Right_Paren);
			expr := node(token, Ast_Sizeof{{}, typespec});
			depend(expr, typespec);
			return expr.base;
		}
		case Ident: {
			ident := token.text;
			sym := node(token, Ast_Identifier{{}, token.text, nil});
			queue_identifier_for_resolving(sym);
			return sym.base;
		}
		case: {
			unexpected_token(token, #location());
			assert(false);
			return nil;
		}
	}
}

parse_base_expr :: proc() -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_operand();

	for is_postfix_op() {
		op := next_token();
		next_expr: ^Ast_Node;
		switch op.kind {
			case Dot: {
				sym_token := expect(Ident);
				selector := node(token, Ast_Selector{{}, current_expr, sym_token.text});
				depend(selector, current_expr);
				next_expr = selector.base;
			}
			case Left_Paren: {
				parameters: [dynamic]^Ast_Node;
				for !is_token(Right_Paren) {
					param := parse_expr();
					append(&parameters, param);

					if is_token(Comma) {
						next_token();
					}
				}
				expect(Right_Paren);
				next_expr = node(token, Ast_Call{{}, current_expr, parameters}).base;

				depend(next_expr, current_expr);

				// todo: factor into the loop above
				for p in parameters {
					depend(next_expr, p);
				}
			}
			case Left_Square: {
				expression: ^Ast_Node;

				if !is_token(Range) {
					expression = parse_expr();
				}

				if is_token(Range) {
					next_token();

					min := expression;
					max: ^Ast_Node;
					if !is_token(Right_Square) {
						max = parse_expr();
					}

					range := node(op, Ast_Range{{}, current_expr, min, max});
					if min != nil do depend(range, min);
					if max != nil do depend(range, max);
					next_expr = node(token, Ast_Slice{{}, current_expr, range}).base;
					depend(next_expr, current_expr);
					depend(next_expr, range);
				}
				else {
					next_expr = node(token, Ast_Subscript{{}, current_expr, expression}).base;
					depend(next_expr, current_expr);
				}

				expect(Right_Square);
			}
			case: {
				err := aprintln("is_postfix_op() returned true, but we must be missing a case in the switch for", op.kind);
				assert(false, err);
			}
		}

		assert(next_expr != nil);
		current_expr = next_expr;
	}

	return current_expr;
}

parse_unary_expr :: proc() -> ^Ast_Node {
	using Token_Type;

	if is_unary_op() {
		op := next_token();
		rhs := parse_unary_expr();
		node := node(op, Ast_Unary{{}, op, rhs});
		depend(node, rhs);
		return node.base;
	}
	else if is_token(Cast) {
		cast_keyword := next_token();
		expect(Left_Paren);
		target_type := parse_typespec();
		expect(Right_Paren);
		rhs := parse_expr();
		node := node(cast_keyword, Ast_Cast{{}, target_type, rhs});
		depend(node, rhs);
		depend(node, target_type);
		return node.base;
	}

	return parse_base_expr();
}

parse_mul_expr :: proc() -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_unary_expr();
	for is_mul_op() {
		op := next_token();
		rhs := parse_unary_expr();
		lhs := current_expr;
		current_expr = node(token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(current_expr, rhs);
		depend(current_expr, lhs);
	}
	return current_expr;
}

parse_add_expr :: proc() -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_mul_expr();
	for is_add_op() {
		op := next_token();
		rhs := parse_mul_expr();
		lhs := current_expr;
		current_expr = node(token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(current_expr, rhs);
		depend(current_expr, lhs);
	}
	return current_expr;
}

parse_cmp_expr :: proc() -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_add_expr();
	for is_cmp_op() {
		op := next_token();
		rhs := parse_add_expr();
		lhs := current_expr;
		current_expr = node(token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(current_expr, rhs);
		depend(current_expr, lhs);
	}
	return current_expr;
}

parse_and_expr :: proc() -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_cmp_expr();
	for is_token(And_And) {
		op := next_token();
		rhs := parse_cmp_expr();
		lhs := current_expr;
		current_expr = node(token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(current_expr, rhs);
		depend(current_expr, lhs);
	}

	return current_expr;
}

parse_or_expr :: proc() -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_and_expr();
	for is_token(Or_Or) {
		op := next_token();
		rhs := parse_and_expr();
		lhs := current_expr;
		current_expr = node(token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(current_expr, rhs);
		depend(current_expr, lhs);
	}

	return current_expr;
}

parse_expr :: inline proc(loc := #caller_location) -> ^Ast_Node {
	return parse_or_expr();
}

parse_var_decl :: proc(require_var := true, only_name := false) -> ^Ast_Var {
	using Token_Type;

	root_token: Token;
	name_token: Token;

	if require_var || peek().kind == Var {
		root_token = expect(Var);
		name_token = expect(Ident);
	}
	else {
		root_token = expect(Ident);
		name_token = root_token;
	}

	name := name_token.text;

	decl := create_symbol(current_block, name, nil);

	typespec: ^Ast_Node;
	value: ^Ast_Node;

	if only_name {
		return node(root_token, Ast_Var{{}, name, nil, nil, decl});
	}

	if is_token(Colon) {
		next_token();
		typespec = parse_typespec();
	}

	if !is_token(In) {
		if is_token(Assign) {
			next_token();
			value = parse_expr();
		}
		else {
			if typespec == nil {
				println("Must provide either a type or a value in a variable declaration:", site(root_token.site));
			}
		}
	}

	var := node(root_token, Ast_Var{{}, name, typespec, value, decl});
	if typespec != nil {
		depend(var, typespec);
	}
	if value != nil {
		depend(var, value);
	}

	return var;
}

try_parse_directive :: proc() -> ^Ast_Directive {
	using Token_Type;

	token := peek();
	switch token.kind {
		case Directive_Odin: {
			directive := next_token();
			return node(directive, Ast_Directive{{}, directive.text});
		}
		case: {
			return nil;
		}
	}
}

currently_parsing_procedure: ^Ast_Proc;

try_parse_proc_directives :: proc() -> u32 {
	flags: u32;
	for true {
		directive := try_parse_directive();
		if directive == nil do break;
		switch directive.directive {
			case "#odin": {
				flags |= PROC_IS_ODIN_PROC;
			}
		}
	}

	return flags;
}

parse_proc_decl :: proc() -> ^Ast_Proc {
	using Token_Type;

	proc_token := expect(Proc);

	procedure_stmt := node(proc_token, Ast_Proc{});

	was_parsing_procedure := currently_parsing_procedure;
	currently_parsing_procedure = procedure_stmt;
	defer currently_parsing_procedure = was_parsing_procedure;

	name_token := expect(Ident);
	name := name_token.text;
	expect(Left_Paren);

	params: [dynamic]^Ast_Var;
	for !is_token(Right_Paren) {
		param := parse_var_decl(false);
		append(&params, param);

		if is_token(Comma) {
			next_token();
		}
	}

	expect(Right_Paren);

	flags := try_parse_proc_directives();

	return_type: ^Ast_Node;
	if !is_token(Left_Curly) && !is_token(Semicolon) {
		return_type = parse_typespec();
	}

	block: ^Ast_Block;
	if peek().kind != Semicolon {
		block = parse_block();
	}
	else {
		expect(Semicolon);
	}

	decl := create_symbol(current_block, name, nil);

	procedure_stmt.name = name;
	procedure_stmt.params = params;
	procedure_stmt.return_typespec = return_type;
	procedure_stmt.flags = flags;
	procedure_stmt.block = block;
	procedure_stmt.sym = decl;

	if return_type != nil {
		depend(procedure_stmt, return_type);
	}
	if block != nil {
		depend(procedure_stmt, block);
	}
	// todo: factor into loop above
	for p in params {
		depend(procedure_stmt, p);
	}


	return procedure_stmt;
}

parse_struct_decl :: proc() -> ^Ast_Struct {
	using Token_Type;

	struct_token := expect(Struct);
	name_token := expect(Ident);

	fields: [dynamic]^Ast_Var;
	block := parse_block();
	field_stmts := block.stmts;
	for _, idx in field_stmts {
		field := &field_stmts[idx].derived.(Ast_Var);
		append(&fields, field);
	}

	decl := create_symbol(current_block, name_token.text, nil);
	s := node(struct_token, Ast_Struct{{}, name_token.text, fields, decl});

	depend(s, block);
	return s;
}

parse_range_or_single_expr :: proc() -> (^Ast_Node, bool) {
	using Token_Type;

	token := peek();
	expr1 := parse_expr();
	if is_token(Range) {
		next_token();
		expr2 := parse_expr();
		node := node(token, Ast_Range{{}, nil, expr1, expr2});
		depend(node, expr1);
		depend(node, expr2);
		return node.base, true;
	}

	return expr1, false;
}

parse_if_stmt :: proc() -> ^Ast_If {
	using Token_Type;

	if_token := next_token();
	if_condition := parse_expr();
	if_block := parse_block();
	if_stmt := node(if_token, Ast_If{{}, if_condition, if_block, nil, nil});

	for is_token(Else) {
		else_token := next_token();

		if is_token(If) {
			next_token();

			else_if_condition := parse_expr();
			else_if_block := parse_block();

			else_if := node(else_token, Ast_Else_If{{}, else_if_condition, else_if_block});
			append(&if_stmt.else_ifs, else_if);
		}
		else {
			assert(if_stmt.else_block == nil);
			block := parse_block();
			if_stmt.else_block = block;
		}
	}

	depend(if_stmt, if_condition);
	depend(if_stmt, if_block);

	// todo: factor into loop above
	for ie in if_stmt.else_ifs {
		depend(if_stmt, ie);
	}

	depend(if_stmt, if_stmt.else_block);

	return if_stmt;
}

parse_loop :: proc() -> ^Ast_Node {
	using Token_Type;

	root_token := next_token();
	if root_token.kind == While {
		condition := parse_expr();
		block := parse_block();
		while_stmt := node(root_token, Ast_While{{}, condition, block});
		depend(while_stmt, condition);
		depend(while_stmt, block);
		return while_stmt.base;
	}
	else {
		assert(root_token.kind == For);
		if is_token(Var) || is_token(Semicolon) {
			// for i
			var: ^Ast_Var;
			if !is_token(Semicolon) {
				var = parse_var_decl(true);
			}
			expect(Semicolon);
			condition: ^Ast_Node;
			if !is_token(Semicolon) {
				condition = parse_expr();
			}
			expect(Semicolon);

			post_stmt: ^Ast_Node;
			if !is_token(Left_Curly) {
				post_stmt = parse_stmt();
			}

			block := parse_block();
			loop := node(root_token, Ast_For_I{{}, var, condition, post_stmt, block});

			if var != nil do depend(loop, var);
			if condition != nil do depend(loop, condition);
			if post_stmt != nil do depend(loop, post_stmt);
			depend(loop, block);

			return loop.base;
		}
		else {
			// for each
			var := parse_var_decl(false);
			expect(In);
			expr, is_range := parse_range_or_single_expr();

			block := parse_block();
			loop := node(root_token, Ast_For_Each{{}, var, expr, block});

			depend(loop, var);
			depend(loop, block);

			return loop.base;
		}
	}
}

parse_stmt :: proc() -> ^Ast_Node {
	using Token_Type;

	token := peek();
	switch (token.kind) {
		case Comment: {
			comment := next_token();
			return node(comment, Ast_Comment{{}, comment.text}).base;
		}
		case Directive_Include: {
			directive := expect(Directive_Include);
			filename := expect(String_Literal);
			return node(directive, Ast_Directive_Include{{}, filename.text}).base;
		}
		case Left_Curly: {
			block := parse_block();
			return block.base;
		}
		case Proc: {
			decl := parse_proc_decl();
			return decl.base;
		}
		case Var: {
			var := parse_var_decl();
			expect(Semicolon);
			// todo: this should be fine but not sure
			return var.base;
		}
		case Struct: {
			s := parse_struct_decl();
			return s.base;
		}
		case Switch: {
			assert(false);
			return nil;
		}
		case For, While: {
			loop := parse_loop();
			return loop;
		}
		case If: {
			i := parse_if_stmt();
			return i.base;
		}
		case Return: {
			ret_token := next_token();
			expr := parse_expr();
			assert(currently_parsing_procedure != nil);
			stmt := node(ret_token, Ast_Return{{}, currently_parsing_procedure, expr});
			expect(Semicolon);
			depend(stmt, expr);
			depend(stmt, currently_parsing_procedure);
			return stmt.base;
		}
		case: {
			parse_assign_stmt :: proc(lhs: ^Ast_Node) -> ^Ast_Node {
				if !is_assign_op() {
					t := peek();
					println("Syntax error: Expected assign operator, got", t.kind, "at", site(t.site));
					assert(false);
				}

				op := next_token();
				rhs := parse_expr();
				stmt := node(op, Ast_Assign{{}, op.kind, lhs, rhs});
				expect(Semicolon);

				depend(stmt, lhs);
				depend(stmt, rhs);

				return stmt.base;
			}

			root_token := peek();
			expr := parse_expr();
			switch kind in expr.derived {
				case Ast_Call: {
					expect(Semicolon);
					return expr;
				}
				case Ast_Unary: {
					if (cast(^Ast_Unary)expr).op.kind == Xor { // dereference assignment
						stmt := parse_assign_stmt(expr);
						return stmt;
					}
					else {
						println("Invalid unary expression at statement level.");
						assert(false);
					}
				}
				case Ast_Subscript, Ast_Selector, Ast_Identifier: {
					stmt := parse_assign_stmt(expr);
					return stmt;
				}
				case: {
					err := aprintln(expr, "is not a valid statement.");
					assert(false, err);
					return nil;
				}
			}
		}
	}

	err := aprintln("Unsupported token:", token.kind);
	assert(false, err);
	return nil;
}

current_block: ^Ast_Block;

parse_block :: proc(loc := #caller_location) -> ^Ast_Block {
	using Token_Type;

	old_current_block := current_block;
	curly := expect(Left_Curly);
	current_block = node(curly, Ast_Block{{}, nil, nil});

	assert(current_block != nil);

	stmts := parse_stmt_list();
	for stmt in stmts {
		append(&current_block.stmts, stmt);
		depend(current_block, stmt);
	}

	expect(Right_Curly);
	new_block := current_block;
	current_block = old_current_block;

	return new_block;
}

site :: proc(the_site: Site) -> string {
	return aprint(the_site.filename, "(", the_site.line, ":", the_site.column, ")");
}
