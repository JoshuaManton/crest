package crest

using import "core:fmt"
	  import "core:os"
	  import "core:strings"

using import "shared:workbench/logging"

semicolon :: proc(output_code: ^[dynamic]u8) do output(output_code, ";");
newline   :: proc(output_code: ^[dynamic]u8) do output(output_code, "\n");

indent_level := 0;
indent :: proc(output_code: ^[dynamic]u8) {
	for i in 0..indent_level {
		output(output_code, "\t");
	}
}
push_indent :: proc() do indent_level += 1;
pop_indent  :: proc() do indent_level -= 1;

output :: inline proc(output_code: ^[dynamic]u8, strings: ..string) {
	for str in strings {
		append_string(output_code, str);
	}
}

type_to_odin_string :: proc(canonical_type: ^Type, loc := #caller_location) -> string {
	assert(canonical_type != nil, aprintln(loc));

	if canonical_type == type_float do return "f32";
	if canonical_type == type_untyped_int do return "int";

	switch kind in canonical_type.kind {
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
		case Type_Union: {
			buf: strings.Builder;
			sbprint(&buf, "union {");
			comma := "";
			for canonical_type in kind.types {
				sbprint(&buf, comma, type_to_odin_string(canonical_type));
				comma = ", ";
			}

			sbprint(&buf, "}");
			return strings.to_string(buf);
		}
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
	switch kind in &node.derived {
		case Ast_Binary: {
			sbprint(&buf, expr_to_string(kind.lhs), " ", kind.op.text, " ", expr_to_string(kind.rhs));
		}
		case Ast_Unary: {
			sbprint(&buf, kind.op.text, expr_to_string(kind.rhs));
		}
		case Ast_Selector: {
			sbprint(&buf, expr_to_string(kind.left), ".", kind.field);
		}
		case Ast_Number: {
			if kind.is_float {
				sbprint(&buf, kind.float_number);
			}
			else {
				sbprint(&buf, kind.int_number);
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
			sbprint(&buf, "cast(", type_to_odin_string(node.inferred_type), ")", expr_to_string(kind.rhs));
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
		case: {
			logln("Unhandled case in expr_to_string(): ", kind^);
			return "";
		}
	}

	return strings.to_string(buf);
}

print_var_decl :: proc(output_code: ^[dynamic]u8, decl: ^Ast_Var) {
	assert(decl != nil);
	type := decl.inferred_type;
	assert(type != nil);
	output(output_code, decl.name, ": ", type_to_odin_string(type));
	if decl.expr != nil {
		output(output_code, " = ", expr_to_string(decl.expr));
	}
}

print_struct_decl :: proc(output_code: ^[dynamic]u8, decl: ^Ast_Struct) {
	output(output_code, decl.name, " :: struct {\n");
	push_indent();
	for _, idx in decl.fields {
		field := decl.fields[idx];
		indent(output_code);
		print_var_decl(output_code, field);
		output(output_code, ",\n");
	}
	pop_indent();
	indent(output_code);
	output(output_code, "}\n\n");
}

print_proc_decl :: proc(output_code: ^[dynamic]u8, decl: ^Ast_Proc) {
	if decl.flags & PROC_IS_ODIN_PROC > 0 {
		return;
	}

	output(output_code, decl.name, " :: proc(");
	comma := "";
	for _, idx in decl.params {
		param := decl.params[idx];
		output(output_code, comma);
		comma = ", ";
		print_var_decl(output_code, param);
	}

	output(output_code, ")");

	if decl.return_type != nil {
		output(output_code, " -> ", type_to_odin_string(decl.return_type));
	}

	output(output_code, " ");
	print_block(output_code, decl.block);
	output(output_code, "\n");
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
			print_struct_decl(output_code, kind);
		}
		case Ast_Proc: {
			print_proc_decl(output_code, kind);
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
			print_var_decl(output_code, kind);
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
	if block.parent == nil {
		with_curlies = false;
	}

	if with_curlies {
		output(output_code, "{\n");
		push_indent();
	}

	for _, idx in block.stmts {
		indent(output_code);
		stmt := block.stmts[idx];
		assert(stmt != nil);
		print_stmt(output_code, stmt);
	}

	if with_curlies {
		pop_indent();
		indent(output_code);
		output(output_code, "}\n");
	}
}

gen_odin :: proc(workspace: ^Workspace) -> string {
	ODIN_PREAMBLE ::
`package output

using import "core:fmt"

`;


	output_code_buffer: [dynamic]u8;
	output(&output_code_buffer, ODIN_PREAMBLE);
	print_block(&output_code_buffer, workspace.syntax_tree);
	output_code := cast(string)output_code_buffer[:];

	os.write_entire_file("output/output.odin", cast([]u8)output_code);

	return output_code;
}
