package crest

// using import "core:fmt"
// 	  import "core:os"
// 	  import "core:strconv"

// using import "shared:workbench/"

// semicolon :: proc() do output(";");
// newline   :: proc() do output("\n");

// indent_level := 0;
// indent :: proc() {
// 	for i in 0..indent_level {
// 		output("\t");
// 	}
// }
// push_indent :: proc() do indent_level += 1;
// pop_indent  :: proc() do indent_level -= 1;

// output_code := make([dynamic]byte, 0, 2048);
// output :: inline proc(strings: ...string) {
// 	for str in strings {
// 		append_string(&output_code, str);
// 	}
// }

// generated_var_names := 0;
// generate_var_name :: proc(name: string) -> string {
// 	buf: String_Buffer;
// 	sbprint(&buf, name, generated_var_names);
// 	generated_var_names += 1;

// 	return to_string(buf);
// }

// type_to_c_string :: proc(canonical_type: ^Type, name: string, loc := #caller_location) -> string {
// 	assert(canonical_type != nil, aprintln("canonical_type was nil from", loc));

// 	switch kind in canonical_type.kind {
// 		case Type_Struct: {
// 			return aprint(kind.name, (name != "" ? " " : ""), name);
// 		}
// 		case Type_Array: {
// 			new_name := aprint(name, "[", kind.size, "]");
// 			return type_to_c_string(kind.array_of, new_name);
// 		}
// 		case Type_Ptr: {
// 			_, nested := kind.ptr_to.kind.(Type_Struct);
// 			nested = !nested;

// 			if nested {
// 				if _, is_ptr_to_ptr := kind.ptr_to.kind.(Type_Ptr); is_ptr_to_ptr {
// 					nested = false;
// 				}
// 			}

// 			new_name := aprint((nested ? "(" : ""), "*", name, (nested ? ")" : ""));
// 			return type_to_c_string(kind.ptr_to, new_name);
// 		}
// 		case: {
// 			log("Unhandled case in type_to_c_string(): ", kind);
// 			return "";
// 		}
// 	}

// 	assert(false);
// 	return "";
// }

// expr_to_string :: proc(expr: ^Ast_Node) -> string {
// 	buf: String_Buffer;
// 	switch kind in &expr.derived {
// 		case Ast_Binary: {
// 			sbprint(&buf, expr_to_string(kind.left), " ", kind.op.text, " ", expr_to_string(kind.right));
// 		}
// 		case Ast_Selector: {
// 			sbprint(&buf, expr_to_string(kind.left), ".", kind.field);
// 		}
// 		case Ast_Number: {
// 			return kind.text;
// 		}
// 		case Ast_Cast: {
// 			sbprint(&buf, "(", type_to_c_string(expr.inferred_type, ""), ")", expr_to_string(kind.expr));
// 		}
// 		case Ast_Call: {
// 			sbprint(&buf, expr_to_string(kind.procedure), "(");
// 			comma := "";
// 			for i in 0..len(kind.params) {
// 				param := kind.params[i];
// 				sbprint(&buf, comma, expr_to_string(param));
// 				comma = ", ";
// 			}
// 			sbprint(&buf, ")");
// 		}
// 		case Ast_String: {
// 			sbprint(&buf, "\"", kind.text, "\"");
// 		}
// 		case Ast_Symbol: {
// 			return kind.name;
// 		}
// 		case: {
// 			log("Unhandled case in expr_to_string(): ", kind^);
// 			return "";
// 		}
// 	}

// 	return to_string(buf);
// }

// print_var_decl :: proc(stmt: ^Ast_Node, decl: ^Ast_Var) {
// 	output(type_to_c_string(stmt.inferred_type, decl.name));
// }

// print_struct_decl :: proc(stmt: ^Ast_Node, decl: ^Ast_Struct) {
// 	output("typedef struct {\n");
// 	push_indent();
// 	for i in 0..len(decl.fields) {
// 		field := decl.fields[i];
// 		var := &field.derived.(Ast_Var);
// 		indent();
// 		print_var_decl(field, var);
// 		output(";\n");
// 	}
// 	pop_indent();
// 	indent();
// 	output("} ", decl.name, ";\n\n");
// }

// print_proc_decl :: proc(stmt: ^Ast_Node, decl: ^Ast_Proc) {
// 	output(type_to_c_string(decl.return_type, ""), " ", decl.name, "(");
// 	comma := "";
// 	for i in 0..len(decl.params) {
// 		param := decl.params[i];
// 		var := &param.derived.(Ast_Var);
// 		output(comma);
// 		comma = ", ";
// 		print_var_decl(param, var);
// 	}

// 	output(") ");
// 	print_block(decl.block, true);
// 	output("\n");
// }

// print_if_stmt :: proc(stmt: ^Ast_Node, if_stmt: ^Ast_If) {
// 	output("if (", expr_to_string(if_stmt.condition), ") ");
// 	print_block(if_stmt.block, true);

// 	for i in 0..len(if_stmt.else_ifs) {
// 		else_if := &if_stmt.else_ifs[i].derived.(Ast_Else_If);
// 		indent();
// 		output("else if (", expr_to_string(else_if.condition), ") ");
// 		print_block(else_if.block, true);
// 	}

// 	if if_stmt.else_block != nil {
// 		indent();
// 		output("else ");
// 		print_block(if_stmt.else_block, true);
// 	}
// }

// print_assign_stmt :: proc(stmt: ^Ast_Node, assign: ^Ast_Assign) {
// 	output(expr_to_string(assign.left), " = ", expr_to_string(assign.right), ";\n");
// }

// print_return_stmt :: proc(stmt: ^Ast_Node, ret: ^Ast_Return) {
// 	output("return");
// 	if ret.expr != nil {
// 		output(" ", expr_to_string(ret.expr));
// 	}
// 	output(";\n");
// }

// print_for_loop :: proc(stmt: ^Ast_Node, loop: ^Ast_For) {
// 	switch kind in &loop.kind {
// 		case Ast_For_Condition: {
// 			output("while (");
// 			output(expr_to_string(kind.condition), ") ");
// 			print_block(loop.block, true);
// 		}
// 		case Ast_For_Each: {
// 			i := generate_var_name("i");
// 			output("for (int ", i, " = 0; ", i, " < ", kind.array.inferred_type.kind.(Type_Array).size, "; ", i, "++) {\n");
// 			push_indent();
// 			indent();
// 			loop_var := kind.var.derived.(Ast_Var);
// 			output(type_to_c_string(kind.var.inferred_type, loop_var.name), " = ", expr_to_string(kind.array), "[", i, "];\n");
// 			print_block(loop.block, false);
// 			pop_indent();
// 			indent();
// 			output("}\n");
// 		}
// 		case: {
// 			log("Unhandled for loop kind in print_for_loop(): ", kind^);
// 		}
// 	}
// }

// print_call :: proc(node: ^Ast_Node, call: ^Ast_Call) {
// 	output(expr_to_string(node));
// 	output(";\n");
// }

// print_node :: proc(node: ^Ast_Node) {
// 	switch kind in &node.derived {
// 		case Ast_Struct: {
// 			print_struct_decl(node, kind);
// 		}
// 		case Ast_Call: {
// 			print_call(node, kind);
// 		}
// 		case Ast_Proc: {
// 			print_proc_decl(node, kind);
// 		}
// 		case Ast_Return: {
// 			print_return_stmt(node, kind);
// 		}
// 		case Ast_Var: {
// 			print_var_decl(node, kind);
// 			output(";\n");
// 		}
// 		case Ast_Assign: {
// 			print_assign_stmt(node, kind);
// 		}
// 		case Ast_If: {
// 			print_if_stmt(node, kind);
// 		}
// 		case Ast_For: {
// 			print_for_loop(node, kind);
// 		}
// 		case: {
// 			log("Unhandled case in print_stmt(): ", kind^);
// 		}
// 	}
// }

// print_block :: proc(_block: ^Ast_Node, with_curlies: bool) {
// 	block := &_block.derived.(Ast_Block);

// 	if with_curlies {
// 		output("{\n");
// 		push_indent();
// 	}

// 	for i in 0..len(block.stmts) {
// 		indent();
// 		stmt := block.stmts[i];
// 		print_node(stmt);
// 	}

// 	if with_curlies {
// 		pop_indent();
// 		indent();
// 		output("}\n");
// 	}
// }

// make_program :: proc(block: ^Ast_Node) {
// 	print_block(block, false);

// 	print("\n\nC OUTPUT\n\n", cast(string)output_code[..], "\n");
// 	os.write_entire_file("output.c", output_code[..]);
// }