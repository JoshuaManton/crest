package crest

using import "core:fmt"
      import rt "core:runtime"
	  import "core:os"

using import "shared:workbench/logging"

parse_workspace :: proc(ws: ^Workspace, filename: string) -> bool {
	assert(ws != nil);

	assert(ws.global_scope == nil);
	ws.global_scope = node(ws, Token{}, Ast_Block{{}, nil, nil});

	if !parse_file(ws, filename, ws.global_scope) {
		return false;
	}

	return true;
}

parse_file :: proc(ws: ^Workspace, filename: string, scope: ^Ast_Block, loc := #caller_location) -> bool {
	bytes, file_ok := os.read_entire_file(filename);
	if !file_ok {
		logln("Couldn't open file: ", filename);
		return false;
	}

	text := cast(string)bytes;
	assert(len(text) > 0);

	push_new_lexer_text(filename, text);
	defer pop_lexer();

	old_block := ws.current_scope;
	ws.current_scope = scope;
	defer ws.current_scope = old_block;

	parse_stmt_list(ws);

	return true;
}

parse_stmt_list :: proc(ws: ^Workspace) {
	using Token_Type;

	for !is_token(Eof) && !is_token(Right_Curly) {
		stmt := parse_stmt(ws);
		if include, ok := stmt.derived.(Ast_Directive_Include); ok {
			parse_file(ws, include.filename, ws.current_scope);
		}
		else {
			depend(ws, ws.current_scope, stmt);
			append(&ws.current_scope.stmts, stmt);
		}
	}
}


_alloc_node :: inline proc(ws: ^Workspace, token: Token, derived: $T, loc := #caller_location) -> ^T {
	@static last_serial: int;

	last_serial += 1;

	ptr := new(Ast_Node);
	derived.base = ptr;
	ptr^ = Ast_Node{
		derived,
		last_serial,
		ws.current_scope,
		token,
		Check_State.Unchecked,
		nil,
		nil,
		nil,
	};

	return cast(^T)ptr;
}

node :: inline proc(ws: ^Workspace, token: Token, derived: $T, loc := #caller_location) -> ^T {
	assert(ws != nil);

	ptr := _alloc_node(ws, token, derived);
	append(&ws.nodes_to_typecheck, ptr.base);
	return ptr;
}

depend :: inline proc(ws: ^Workspace, node: ^$T, depends_on: ^$S, loc := #caller_location) {
	assert(ws != nil);
	assert(node != nil, tprint("node was nil at ", pretty_location(loc)));
	assert(depends_on != nil, tprint("depends_on was nil at ", pretty_location(loc)));
	append(&node.depends, depends_on);
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

	if is_token(Dot, Left_Paren, Left_Square) do return true;

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

parse_typespec :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := peek();
	if is_ident_or_type_keyword(token.kind) {
		token = next_token();
		symbol := node(ws, token, Ast_Identifier{{}, token.text, nil});
		queue_identifier_for_resolving(ws, symbol);
		return symbol.base;
	}

	if token.kind == Union {
		next_token();
		expect(Left_Curly);

		types: [dynamic]^Ast_Node;
		for peek().kind != Right_Curly && peek().kind != Eof {
			spec := parse_typespec(ws);
			append(&types, spec);
			expect(Comma);
		}

		expect(Right_Curly);

		union_node := node(ws, token, Ast_Typespec_Union{{}, types[:]});

		// todo: factor this into the loop above
		for t in types {
			depend(ws, union_node, t);
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
			typespec := parse_typespec(ws);
			slice := node(ws, token, Ast_Typespec_Slice{{}, typespec});
			depend(ws, slice, typespec);
			return slice.base;
		}

		if peek().kind == Dot_Dot {
			next_token();
			expect(Right_Square);
			typespec := parse_typespec(ws);
			dynamic_array := node(ws, token, Ast_Typespec_Dynamic_Array{{}, typespec});
			depend(ws, dynamic_array, typespec);
			return dynamic_array.base;
		}

		array_length_expr := parse_expr(ws);
		expect(Right_Square);

		typespec := parse_typespec(ws);
		depend(ws, typespec, array_length_expr);

		array := node(ws, token, Ast_Typespec_Array{{}, array_length_expr, typespec});
		depend(ws, array, typespec);
		return array.base;
	}

	assert(modifier.kind == Xor);
	typespec := parse_typespec(ws);
	ptr := node(ws, token, Ast_Typespec_Ptr{{}, typespec});
	depend(ws, ptr, typespec);
	return ptr.base;
}

import "core:strconv"

parse_operand :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := next_token();
	switch token.kind {
		case Null: {
			expr := node(ws, token, Ast_Null{});
			return expr.base;
		}
		case Integer_Literal: {
			num := node(ws, token, Ast_Number{{}, strconv.parse_i64(token.text)});
			return num.base;
		}
		case Float_Literal: {
			num := node(ws, token, Ast_Number{{}, strconv.parse_f64(token.text)});
			return num.base;
		}
		case String_Literal: {
			str := node(ws, token, Ast_String{{}, token.text});
			return str.base;
		}
		case Left_Paren: {
			nested := parse_expr(ws);
			node := node(ws, token, Ast_Paren{{}, nested});
			expect(Right_Paren);
			depend(ws, node, nested);
			return node.base;
		}
		case Sizeof: {
			expect(Left_Paren);
			typespec := parse_typespec(ws);
			expect(Right_Paren);
			expr := node(ws, token, Ast_Sizeof{{}, typespec});
			depend(ws, expr, typespec);
			return expr.base;
		}
		case Ident: {
			ident := token.text;
			sym := node(ws, token, Ast_Identifier{{}, token.text, nil});
			queue_identifier_for_resolving(ws, sym);
			return sym.base;
		}
		case: {
			unexpected_token(token, #location());
			assert(false);
			return nil;
		}
	}
}

parse_base_expr :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_operand(ws);

	for is_postfix_op() {
		op := next_token();
		next_expr: ^Ast_Node;
		switch op.kind {
			case Dot: {
				sym_token := expect(Ident);
				selector := node(ws, token, Ast_Selector{{}, current_expr, sym_token.text});
				depend(ws, selector, current_expr);
				next_expr = selector.base;
			}
			case Left_Paren: {
				parameters: [dynamic]^Ast_Node;
				for !is_token(Right_Paren) {
					param := parse_expr(ws);
					append(&parameters, param);

					if is_token(Comma) {
						next_token();
					}
				}
				expect(Right_Paren);
				next_expr = node(ws, token, Ast_Call{{}, current_expr, parameters[:]}).base;

				depend(ws, next_expr, current_expr);

				// todo: factor into the loop above
				for p in parameters {
					depend(ws, next_expr, p);
				}
			}
			case Left_Square: {
				expression: ^Ast_Node;

				if !is_token(Dot_Dot) {
					expression = parse_expr(ws);
				}

				if is_token(Dot_Dot) {
					next_token();

					min := expression;
					max: ^Ast_Node;
					if !is_token(Right_Square) {
						max = parse_expr(ws);
					}

					range := node(ws, op, Ast_Range{{}, current_expr, min, max});
					if min != nil do depend(ws, range, min);
					if max != nil do depend(ws, range, max);
					next_expr = node(ws, token, Ast_Slice{{}, current_expr, range}).base;
					depend(ws, next_expr, current_expr);
					depend(ws, next_expr, range);
				}
				else {
					next_expr = node(ws, token, Ast_Subscript{{}, current_expr, expression}).base;
					depend(ws, next_expr, current_expr);
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

parse_unary_expr :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	if is_unary_op() {
		op := next_token();
		rhs := parse_unary_expr(ws);
		node := node(ws, op, Ast_Unary{{}, op, rhs});
		depend(ws, node, rhs);
		return node.base;
	}
	else if is_token(Cast) {
		cast_keyword := next_token();
		expect(Left_Paren);
		target_type := parse_typespec(ws);
		expect(Right_Paren);
		rhs := parse_expr(ws);
		node := node(ws, cast_keyword, Ast_Cast{{}, target_type, rhs});
		depend(ws, node, rhs);
		depend(ws, node, target_type);
		return node.base;
	}

	return parse_base_expr(ws);
}

parse_mul_expr :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_unary_expr(ws);
	for is_mul_op() {
		op := next_token();
		rhs := parse_unary_expr(ws);
		lhs := current_expr;
		current_expr = node(ws, token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(ws, current_expr, rhs);
		depend(ws, current_expr, lhs);
	}
	return current_expr;
}

parse_add_expr :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_mul_expr(ws);
	for is_add_op() {
		op := next_token();
		rhs := parse_mul_expr(ws);
		lhs := current_expr;
		current_expr = node(ws, token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(ws, current_expr, rhs);
		depend(ws, current_expr, lhs);
	}
	return current_expr;
}

parse_cmp_expr :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_add_expr(ws);
	for is_cmp_op() {
		op := next_token();
		rhs := parse_add_expr(ws);
		lhs := current_expr;
		current_expr = node(ws, token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(ws, current_expr, rhs);
		depend(ws, current_expr, lhs);
	}
	return current_expr;
}

parse_and_expr :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_cmp_expr(ws);
	for is_token(And_And) {
		op := next_token();
		rhs := parse_cmp_expr(ws);
		lhs := current_expr;
		current_expr = node(ws, token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(ws, current_expr, rhs);
		depend(ws, current_expr, lhs);
	}

	return current_expr;
}

parse_or_expr :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := peek();
	current_expr := parse_and_expr(ws);
	for is_token(Or_Or) {
		op := next_token();
		rhs := parse_and_expr(ws);
		lhs := current_expr;
		current_expr = node(ws, token, Ast_Binary{{}, op, current_expr, rhs}).base;
		depend(ws, current_expr, rhs);
		depend(ws, current_expr, lhs);
	}

	return current_expr;
}

parse_expr :: inline proc(ws: ^Workspace, loc := #caller_location) -> ^Ast_Node {
	return parse_or_expr(ws);
}

parse_var_decl :: proc(ws: ^Workspace, require_var := true, only_name := false) -> ^Ast_Var {
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

	decl := create_symbol(ws.current_scope, name, nil);

	typespec: ^Ast_Node;
	value: ^Ast_Node;

	if only_name {
		return node(ws, root_token, Ast_Var{{}, name, nil, nil, decl, false});
	}

	if is_token(Colon) {
		next_token();
		typespec = parse_typespec(ws);
	}

	if !is_token(In) {
		if is_token(Assign) {
			next_token();
			value = parse_expr(ws);
		}
		else {
			if typespec == nil {
				println("Must provide either a type or a value in a variable declaration:", site(root_token.site));
			}
		}
	}

	var := node(ws, root_token, Ast_Var{{}, name, typespec, value, decl, false});
	if typespec != nil {
		depend(ws, var, typespec);
	}
	if value != nil {
		depend(ws, var, value);
	}

	return var;
}

try_parse_directive :: proc(ws: ^Workspace) -> ^Ast_Directive {
	using Token_Type;

	token := peek();
	switch token.kind {
		case Directive_Odin_Proc: {
			directive := next_token();
			return node(ws, directive, Ast_Directive{{}, directive.text});
		}
		case: {
			return nil;
		}
	}
}

currently_parsing_procedure: ^Ast_Proc;

PROC_IS_ODIN_PROC : u32 : 1 << 0;

try_parse_proc_directives :: proc(ws: ^Workspace) -> u32 {
	flags: u32;
	for true {
		directive := try_parse_directive(ws);
		if directive == nil do break;
		switch directive.directive {
			case "#odin": {
				flags |= PROC_IS_ODIN_PROC;
			}
		}
	}

	return flags;
}

parse_proc_decl :: proc(ws: ^Workspace) -> ^Ast_Proc {
	using Token_Type;

	proc_token := expect(Proc);

	procedure_stmt := node(ws, proc_token, Ast_Proc{});

	was_parsing_procedure := currently_parsing_procedure;
	currently_parsing_procedure = procedure_stmt;
	defer currently_parsing_procedure = was_parsing_procedure;

	name_token := expect(Ident);
	name := name_token.text;
	expect(Left_Paren);

	params: [dynamic]^Ast_Var;
	for !is_token(Right_Paren) {
		param := parse_var_decl(ws, false);
		append(&params, param);
		depend(ws, procedure_stmt, param);

		if is_token(Comma) {
			next_token();
		}
	}

	expect(Right_Paren);

	flags := try_parse_proc_directives(ws);

	return_type: ^Ast_Node;
	if !is_token(Left_Curly) && !is_token(Semicolon) {
		return_type = parse_typespec(ws);
	}

	block: ^Ast_Block;
	if !is_token(Semicolon) {
		block = parse_block(ws);
	}
	else {
		expect(Semicolon);
	}

	decl := create_symbol(ws.current_scope, name, nil);

	procedure_stmt.name = name;
	procedure_stmt.params = params[:];
	procedure_stmt.return_typespec = return_type;
	procedure_stmt.flags = flags;
	procedure_stmt.block = block;
	procedure_stmt.sym = decl;

	if currently_parsing_procedure.parent == ws.global_scope {
		procedure_stmt.output_name = name;
	}
	else {
		procedure_stmt.output_name = aprint(name, procedure_stmt.base.serial);
	}

	if return_type != nil {
		depend(ws, procedure_stmt, return_type);
	}

	// note(josh): I don't think we need to depend on proc bodies
	// if block != nil {
	// 	depend(ws, procedure_stmt, block);
	// }

	append(&ws.all_procedures, procedure_stmt);

	return procedure_stmt;
}

parse_struct_decl :: proc(ws: ^Workspace) -> ^Ast_Node {
	struct_token := expect(Token_Type.Type_Keyword);
	name_token := expect(Token_Type.Ident);
	decl := create_symbol(ws.current_scope, name_token.text, nil);

	n: ^Ast_Node;

	if is_token(Token_Type.Left_Curly) {
		fields: [dynamic]^Ast_Var;
		block := parse_block(ws);
		field_stmts := block.stmts;
		for _, idx in field_stmts {
			field := &field_stmts[idx].derived.(Ast_Var);
			append(&fields, field);
		}

		n = node(ws, struct_token, Ast_Struct{{}, name_token.text, fields[:], decl});
		depend(ws, n, block);
	}
	else {
		other_type := parse_typespec(ws);
		n = node(ws, struct_token, Ast_Typedef{{}, name_token.text, other_type, decl});
		depend(ws, n, other_type);
		expect(Token_Type.Semicolon);
	}

	return n;
}

parse_range_or_single_expr :: proc(ws: ^Workspace) -> (^Ast_Node, bool) {
	using Token_Type;

	token := peek();
	expr1 := parse_expr(ws);
	if is_token(Dot_Dot) {
		next_token();
		expr2 := parse_expr(ws);
		node := node(ws, token, Ast_Range{{}, nil, expr1, expr2});
		depend(ws, node, expr1);
		depend(ws, node, expr2);
		return node.base, true;
	}

	return expr1, false;
}

parse_if_stmt :: proc(ws: ^Workspace) -> ^Ast_If {
	using Token_Type;

	if_token := next_token();
	if_condition := parse_expr(ws);
	if_block := parse_block(ws);
	if_stmt := node(ws, if_token, Ast_If{{}, if_condition, if_block, nil, nil});

	else_ifs: [dynamic]^Ast_Else_If;

	for is_token(Else) {
		else_token := next_token();

		if is_token(If) {
			next_token();

			else_if_condition := parse_expr(ws);
			else_if_block := parse_block(ws);

			else_if := node(ws, else_token, Ast_Else_If{{}, else_if_condition, else_if_block});
			append(&else_ifs, else_if);
		}
		else {
			assert(if_stmt.else_block == nil);
			block := parse_block(ws);
			if_stmt.else_block = block;
		}
	}

	if_stmt.else_ifs = else_ifs[:];

	depend(ws, if_stmt, if_condition);
	depend(ws, if_stmt, if_block);

	// todo: factor into loop above
	for ie in if_stmt.else_ifs {
		depend(ws, if_stmt, ie);
	}

	depend(ws, if_stmt, if_stmt.else_block);

	return if_stmt;
}

parse_loop :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	root_token := next_token();
	if root_token.kind == While {
		condition := parse_expr(ws);
		block := parse_block(ws);
		while_stmt := node(ws, root_token, Ast_While{{}, condition, block});
		depend(ws, while_stmt, condition);
		depend(ws, while_stmt, block);
		return while_stmt.base;
	}
	else {
		assert(root_token.kind == For);
		if is_token(Var) || is_token(Semicolon) {
			// for i
			var: ^Ast_Var;
			if !is_token(Semicolon) {
				var = parse_var_decl(ws, true);
			}
			expect(Semicolon);
			condition: ^Ast_Node;
			if !is_token(Semicolon) {
				condition = parse_expr(ws);
			}
			expect(Semicolon);

			post_stmt: ^Ast_Node;
			if !is_token(Left_Curly) {
				post_stmt = parse_stmt(ws);
			}

			block := parse_block(ws);
			loop := node(ws, root_token, Ast_For_I{{}, var, condition, post_stmt, block});

			if var != nil do depend(ws, loop, var);
			if condition != nil do depend(ws, loop, condition);
			if post_stmt != nil do depend(ws, loop, post_stmt);
			depend(ws, loop, block);

			return loop.base;
		}
		else {
			// for each
			var := parse_var_decl(ws, false);
			expect(In);
			expr, is_range := parse_range_or_single_expr(ws);

			block := parse_block(ws);
			loop := node(ws, root_token, Ast_For_Each{{}, var, expr, block});

			depend(ws, loop, var);
			depend(ws, loop, block);

			return loop.base;
		}
	}
}

parse_stmt :: proc(ws: ^Workspace) -> ^Ast_Node {
	using Token_Type;

	token := peek();
	switch (token.kind) {
		case .Comment: {
			comment := next_token();
			return node(ws, comment, Ast_Comment{{}, comment.text}).base;
		}
		case .Directive_Include: {
			directive := expect(Directive_Include);
			filename := expect(String_Literal);
			return node(ws, directive, Ast_Directive_Include{{}, filename.text}).base;
		}
		case .Directive_Assert: {
			directive := expect(Directive_Assert);
			condition := parse_expr(ws);
			n := node(ws, directive, Ast_Directive_Assert{{}, condition}).base;
			depend(ws, n, condition);
			return n;
		}
		case .Left_Curly: {
			block := parse_block(ws);
			return block.base;
		}
		case .Proc: {
			decl := parse_proc_decl(ws);
			return decl.base;
		}
		case .Var: {
			var := parse_var_decl(ws);
			expect(Semicolon);
			if currently_parsing_procedure != nil {
				append(&currently_parsing_procedure.var_declarations, var);
			}
			return var.base;
		}
		case .Const: {
			const_token := next_token();
			var := parse_var_decl(ws, false);
			var.base.root_token = const_token;
			expect(Semicolon);
			var.is_constant = true;
			if var.expr == nil {
				error(var.base, "Constants must be defined with an expression.");
				assert(false); // todo(josh): error handling in parser code
			}
			return var.base;
		}
		case .Type_Keyword: {
			s := parse_struct_decl(ws);
			return s;
		}
		case .Switch: {
			assert(false);
			return nil;
		}
		case .For, While: {
			loop := parse_loop(ws);
			return loop;
		}
		case .If: {
			i := parse_if_stmt(ws);
			return i.base;
		}
		case .Return: {
			ret_token := next_token();
			expr := parse_expr(ws);
			assert(currently_parsing_procedure != nil);
			stmt := node(ws, ret_token, Ast_Return{{}, currently_parsing_procedure, expr});
			expect(Semicolon);
			depend(ws, stmt, expr);
			depend(ws, stmt, currently_parsing_procedure);
			return stmt.base;
		}
		case: {
			parse_assign_stmt :: proc(ws: ^Workspace, lhs: ^Ast_Node) -> ^Ast_Node {
				if !is_assign_op() {
					t := peek();
					println("Syntax error: Expected assign operator, got", t.kind, "at", site(t.site));
					assert(false);
				}

				op := next_token();
				rhs := parse_expr(ws);
				stmt := node(ws, op, Ast_Assign{{}, op.kind, lhs, rhs});
				expect(Semicolon);

				depend(ws, stmt, lhs);
				depend(ws, stmt, rhs);

				return stmt.base;
			}

			root_token := peek();
			expr := parse_expr(ws);
			switch kind in expr.derived {
				case Ast_Call: {
					expect(Semicolon);
					return expr;
				}
				case Ast_Unary: {
					if (cast(^Ast_Unary)expr).op.kind == Xor { // dereference assignment
						stmt := parse_assign_stmt(ws, expr);
						return stmt;
					}
					else {
						println("Invalid unary expression at statement level.");
						assert(false);
					}
				}
				case Ast_Subscript, Ast_Selector, Ast_Identifier: {
					stmt := parse_assign_stmt(ws, expr);
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

parse_block :: proc(ws: ^Workspace, loc := #caller_location) -> ^Ast_Block {
	using Token_Type;

	curly := expect(Left_Curly);

	new_scope := node(ws, curly, Ast_Block{{}, nil, nil});
	old_current_block := ws.current_scope;
	ws.current_scope = new_scope;
	defer ws.current_scope = old_current_block;

	parse_stmt_list(ws);

	expect(Right_Curly);
	return new_scope;
}

site :: proc(the_site: Site) -> string {
	return aprint(the_site.filename, "(", the_site.line, ":", the_site.column, ")");
}
