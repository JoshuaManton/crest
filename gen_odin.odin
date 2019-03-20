package crest

using import "core:fmt"
	  import "core:os"
	  import "core:strings"

using import "shared:workbench/logging"

semicolon :: proc(output_code: ^[dynamic]u8) do output(output_code, ";");
newline   :: proc(output_code: ^[dynamic]u8) do output(output_code, "\n");

indent_level := 0;
indent :: proc(output_code: ^[dynamic]u8) {
	for i in 0..indent_level-1 {
		output(output_code, "\t");
	}
}
push_indent :: proc(loc := #caller_location) do indent_level += 1;
pop_indent  :: proc(loc := #caller_location) do indent_level -= 1;

output :: inline proc(output_code: ^[dynamic]u8, strings: ..string) {
	for str in strings {
		append_string(output_code, str);
	}
}

type_to_odin_string :: proc(canonical_type: ^Type, loc := #caller_location) -> string {
	assert(canonical_type != nil, aprintln(loc));

	if canonical_type == type_float       do return "f32";
	if canonical_type == type_untyped_int do return "i32";

	#complete
	switch kind in canonical_type.kind {
		case Type_Primitive: {
			return kind.name;
		}
		case Type_Struct: {
			return kind.name;
		}
		case Type_Dynamic_Array: {
			return aprint("[dynamic]", type_to_odin_string(kind.array_of));
		}
		case Type_Array: {
			return aprint("[", kind.length, "]", type_to_odin_string(kind.array_of));
		}
		case Type_Slice: {
			return aprint("[]", type_to_odin_string(kind.slice_of));
		}
		case Type_Ptr: {
			assert(kind.ptr_to != canonical_type);
			return aprint("^", type_to_odin_string(kind.ptr_to));
		}
		case Type_Proc: {
			str: strings.Builder;
			sbprint(&str, "#type proc(");
			comma := "";
			for param in kind.params {
				sbprint(&str, comma, type_to_odin_string(param.inferred_type));
				comma = ", ";
			}

			sbprint(&str, ")");

			if kind.return_type != nil {
				sbprint(&str, " -> ", type_to_odin_string(kind.return_type));
			}

			return strings.to_string(str);
		}
		// @UnionTypes
		// case Type_Union: {
		// 	buf: strings.Builder;
		// 	sbprint(&buf, "union {");
		// 	comma := "";
		// 	for canonical_type in kind.types {
		// 		sbprint(&buf, comma, type_to_odin_string(canonical_type));
		// 		comma = ", ";
		// 	}

		// 	sbprint(&buf, "}");
		// 	return strings.to_string(buf);
		// }
		case: {
			logln("Unhandled case in type_to_odin_string(): ", kind);
			return "";
		}
	}

	assert(false);
	return "";
}

expr_to_string :: proc(node: ^Ast_Node) -> string {
	assert(node != nil);
	buf: strings.Builder;

	if node.constant_value != nil {
		#complete
		switch const_kind in node.constant_value {
			case i64:    sbprint(&buf, const_kind);
			case TypeID: sbprint(&buf, cast(i64)const_kind);
			case f64:    sbprint(&buf, const_kind);
			case bool:   sbprint(&buf, const_kind);
			case string: sbprint(&buf, "\"", const_kind, "\"");
			case: {
				unhandledcase(const_kind);
			}
		}

		return strings.to_string(buf);
	}

	switch kind in &node.derived {
		case Ast_Binary: {
			sbprint(&buf, expr_to_string(kind.lhs), " ", kind.op.text, " ", expr_to_string(kind.rhs));
		}

		case Ast_Unary: {
			using Token_Type;
			switch kind.op.kind {
				case Plus, Minus, And, Not: {
					sbprint(&buf, kind.op.text, expr_to_string(kind.rhs));
				}
				case Xor: {
					sbprint(&buf, expr_to_string(kind.rhs), kind.op.text);
				}
				case: unhandledcase(kind.op);
			}
		}

		case Ast_Selector: {
			sbprint(&buf, expr_to_string(kind.left), ".", kind.field);
		}

		case Ast_Number: {
			#complete
			switch value in kind.value {
				case f64: sbprint(&buf, value);
				case i64: sbprint(&buf, value);
			}
		}

		case Ast_Call: {
			sbprint(&buf, expr_to_string(kind.procedure), "(");
			comma := "";
			for _, idx in kind.params {
				param := kind.params[idx];
				sbprint(&buf, comma, expr_to_string(param));
				comma = ", ";
			}
			sbprint(&buf, ")");
		}

		case Ast_String: {
			sbprint(&buf, "\"", kind.text, "\"");
		}

		case Ast_Cast: {
			sbprint(&buf, "cast(", type_to_odin_string(node.expr_type), ")", expr_to_string(kind.rhs));
		}

		case Ast_Identifier: {
			return kind.name;
		}

		case Ast_Slice: {
			sbprint(&buf, expr_to_string(kind.array), "[");
			if kind.range.min != nil {
				sbprint(&buf, expr_to_string(kind.range.min));
			}
			sbprint(&buf, ":");
			if kind.range.max != nil {
				sbprint(&buf, expr_to_string(kind.range.max));
			}

			sbprint(&buf, "]");
		}

		case Ast_Subscript: {
			sbprint(&buf, expr_to_string(kind.left), "[");
			sbprint(&buf, expr_to_string(kind.index), "]");
		}

		case: {
			unhandledcase(kind^);
			return "";
		}
	}

	return strings.to_string(buf);
}

print_var_decl :: proc(output_code: ^[dynamic]u8, name: string, type: ^Type, expr: ^Ast_Node, is_constant: bool = false) {
	assert(type != nil);
	output(output_code, name, ": ", type_to_odin_string(type));
	if expr != nil {
		if is_constant {
			output(output_code, " : ", expr_to_string(expr));
		}
		else {
			output(output_code, " = ", expr_to_string(expr));
		}
	}
}

print_struct_decl :: proc(output_code: ^[dynamic]u8, decl: ^Ast_Struct) {
	output(output_code, decl.name, " :: struct {\n");
	push_indent();
	for _, idx in decl.fields {
		field := decl.fields[idx];
		indent(output_code);
		print_var_decl(output_code, field.name, field.var_type, nil);
		output(output_code, ",\n");
	}
	pop_indent();
	indent(output_code);
	output(output_code, "}\n\n");
}

print_typedef :: proc(output_code: ^[dynamic]u8, decl: ^Ast_Typedef) {
	// todo(josh): kinda lame that we need access to a workspace for this thing only
	output(output_code, decl.name, " :: distinct ", type_to_odin_string(get_type(current_workspace, decl.other.base.constant_value.(TypeID))), ";\n");
}

print_proc_decl :: proc(output_code: ^[dynamic]u8, decl: ^Ast_Proc) {
	if decl.flags & PROC_IS_ODIN_PROC > 0 {
		output(output_code, "// #odin proc ", decl.name, "\n");
		return;
	}

	output(output_code, decl.name, " :: proc(");
	comma := "";
	for _, idx in decl.params {
		param := decl.params[idx];
		output(output_code, comma);
		comma = ", ";
		print_var_decl(output_code, param.name, param.var_type, param.expr);
	}

	output(output_code, ")");

	if decl.return_type != nil {
		output(output_code, " -> ", type_to_odin_string(decl.return_type));
	}

	output(output_code, " ");
	if decl.block != nil {
		print_block(output_code, decl.block);
	}
	output(output_code, "\n");
}

print_site :: proc(output_code: ^[dynamic]u8, s: Site) {
	output(output_code, "// ", site(s), "\n");
	indent(output_code);
}

print_while_loop :: proc(output_code: ^[dynamic]u8, loop: ^Ast_While) {
	output(output_code, "for ", expr_to_string(loop.condition), " ");
	print_block(output_code, loop.block);
}

print_if_stmt :: proc(output_code: ^[dynamic]u8, if_stmt: ^Ast_If) {
	output(output_code, "if ", expr_to_string(if_stmt.condition), " ");
	print_block(output_code, if_stmt.block);

	for _, idx in if_stmt.else_ifs {
		else_if := if_stmt.else_ifs[idx];
		indent(output_code);
		output(output_code, "else if ", expr_to_string(else_if.condition), " ");
		print_block(output_code, else_if.block);
	}

	if if_stmt.else_block != nil {
		indent(output_code);
		output(output_code, "else ");
		print_block(output_code, if_stmt.else_block);
	}
}

print_assign_stmt :: proc(output_code: ^[dynamic]u8, assign: ^Ast_Assign) {
	using Token_Type;
	op := "";

	switch assign.op {
		case Assign:             { op = " = "; }
		case Plus_Assign:        { op = " += "; }
		case Minus_Assign:       { op = " -= "; }
		case Multiply_Assign:    { op = " *= "; }
		case Divide_Assign:      { op = " /= "; }
		case Mod_Assign:         { op = " %= "; }
		case Or_Assign:          { op = " |= "; }
		case And_Assign:         { op = " &= "; }
		case Xor_Assign:         { op = " ^= "; }
		case LShift_Assign:      { op = " <<= "; }
		case RShift_Assign:      { op = " >>= "; }
		case:                    { assert(false); }
	}

	output(output_code, expr_to_string(assign.left), op, expr_to_string(assign.right), ";\n");
}

print_return_stmt :: proc(output_code: ^[dynamic]u8, ret: ^Ast_Return) {
	output(output_code, "return");
	if ret.expr != nil {
		output(output_code, " ", expr_to_string(ret.expr));
	}
	output(output_code, ";\n");
}

print_loop :: proc(output_code: ^[dynamic]u8, loop: ^Ast_Node) {
	output(output_code, "for ");
	switch kind in &loop.derived {
		case Ast_While: {
			output(output_code, expr_to_string(kind.condition), " ");
			print_block(output_code, loop.parent);
		}
		case Ast_For_Each: {
			output(output_code, kind.var.name, " in ", expr_to_string(kind.array), " ");
			print_block(output_code, loop.parent);
		}
		case: {
			logln("Unhandled for loop kind in print_loop(): ", kind^);
		}
	}
}

print_call :: proc(output_code: ^[dynamic]u8, call: ^Ast_Call) {
	output(output_code, expr_to_string(call.procedure), "(");
	comma := "";
	for param in call.params {
		output(output_code, comma, expr_to_string(param));
		comma = ", ";
	}
	output(output_code, ")");
}

// print_switch_stmt :: proc(output_code: ^[dynamic]u8, switch_stmt: ^Ast_Node) {
// 	output(output_code, "switch ", switch_stmt.var.name.name, " in ", expr_to_string(switch_stmt.expr), " {\n");
// 	push_indent();
// 	for block in switch_stmt.blocks {
// 		indent(output_code);
// 		output(output_code, "case");
// 		if block.condition != nil {
// 			output(output_code, " ", type_to_odin_string(block.condition.inferred_type), ": ");
// 		}
// 		else {
// 			output(output_code, ": ");
// 		}

// 		print_block(output_code, block.block);
// 	}
// 	pop_indent();
// 	indent(output_code);
// 	output(output_code, "}\n");
// }

print_stmt :: proc(output_code: ^[dynamic]u8, stmt: ^Ast_Node) {
	switch kind in &stmt.derived {
		case Ast_Struct: {
			print_site(output_code, stmt.root_token.site);
			print_struct_decl(output_code, kind);
		}
		case Ast_Proc: {
			print_site(output_code, stmt.root_token.site);
			print_proc_decl(output_code, kind);
		}
		case Ast_Typedef: {
			print_site(output_code, stmt.root_token.site);
			print_typedef(output_code, kind);
		}
		case Ast_Call: {
			print_call(output_code, kind);
			output(output_code, ";\n");
		}
		case Ast_Block: {
			print_block(output_code, kind);
		}
		case Ast_Return: {
			print_return_stmt(output_code, kind);
		}
		case Ast_Var: {
			print_var_decl(output_code, kind.name, kind.var_type, kind.expr, kind.is_constant);
			output(output_code, ";\n");
		}
		case Ast_Assign: {
			print_assign_stmt(output_code, kind);
		}
		// case Ast_Switch: {
		// 	print_switch_stmt(output_code, kind);
		// }
		case Ast_If: {
			print_if_stmt(output_code, kind);
		}
		case Ast_While: {
			print_while_loop(output_code, kind);
		}
		case Ast_Comment: {
			output(output_code, kind.text, "\n");
		}

		case: {
			logln("Unhandled case in print_stmt(): ", kind^);
		}
	}
}

print_block :: proc(output_code: ^[dynamic]u8, block: ^Ast_Block) {
	assert(block != nil);

	with_curlies := true;
	if block.base.parent == nil {
		with_curlies = false;
	}

	if with_curlies {
		output(output_code, "{\n");
		push_indent();
	}

	for _, idx in block.stmts {
		stmt := block.stmts[idx];
		if stmt.do_not_print do continue;
		indent(output_code);
		assert(stmt != nil);
		print_stmt(output_code, stmt);
	}

	if with_curlies {
		pop_indent();
		indent(output_code);
		output(output_code, "}\n");
	}
}

@private
current_workspace: ^Workspace; // @Jank

gen_odin :: proc(workspace: ^Workspace) -> string {
	ODIN_PREAMBLE ::
`package output

using import "core:fmt"

type_id :: distinct int;

`;
	old_workspace := current_workspace;
	current_workspace = workspace;
	defer current_workspace = old_workspace;

	output_code_buffer: [dynamic]u8;
	output(&output_code_buffer, ODIN_PREAMBLE);

	print_block(&output_code_buffer, workspace.global_scope);
	output_code := cast(string)output_code_buffer[:];

	os.write_entire_file("output/output.odin", cast([]u8)output_code);

	return output_code;
}
