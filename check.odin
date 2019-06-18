package crest

using import "core:fmt"
	  import "core:os"
	  import "core:strings"

using import "shared:workbench/reflection"
using import "shared:workbench/logging"

// todo(josh): these are goofy
INT_SIZE :: 4;
POINTER_SIZE :: 8;
SLICE_SIZE :: POINTER_SIZE + INT_SIZE;

type_i8:    ^Type;
type_i16:   ^Type;
type_i32:   ^Type;
type_i64:   ^Type;
type_int:   ^Type;

type_u8:    ^Type;
type_u16:   ^Type;
type_u32:   ^Type;
type_u64:   ^Type;
type_uint:  ^Type;

type_f32:   ^Type;
type_f64:   ^Type;
type_float: ^Type;

type_bool: ^Type;

type_string: ^Type;
type_rawptr: ^Type;
// type_list: ^Type;
// type_slice: ^Type;

type_untyped_int: ^Type;
type_untyped_float: ^Type;

type_type_id: ^Type;

init_builtin_types :: proc(using ws: ^Workspace) {
	if type_i8 == nil {
		type_i8   = make_type(ws, 1, Type_Primitive{"i8"},   Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_i16  = make_type(ws, 2, Type_Primitive{"i16"},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_i32  = make_type(ws, 4, Type_Primitive{"i32"},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_i64  = make_type(ws, 8, Type_Primitive{"i64"},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_u8   = make_type(ws, 1, Type_Primitive{"u8"},   Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_u16  = make_type(ws, 2, Type_Primitive{"u16"},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_u32  = make_type(ws, 4, Type_Primitive{"u32"},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_u64  = make_type(ws, 8, Type_Primitive{"u64"},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_f32  = make_type(ws, 4, Type_Primitive{"f32"},  Type_Flags.Number | Type_Flags.Float   | Type_Flags.Signed);
		type_f64  = make_type(ws, 8, Type_Primitive{"f64"},  Type_Flags.Number | Type_Flags.Float   | Type_Flags.Signed);
		type_bool = make_type(ws, 1, Type_Primitive{"bool"}, {});

		type_int   = type_i32;
		type_uint  = type_u32;
		type_float = type_f32;

		type_rawptr = make_type(ws, POINTER_SIZE, Type_Primitive{"rawptr"}, Type_Flags.Pointer);

		create_declaration(ws.global_scope, "i8",      type_i8);
		create_declaration(ws.global_scope, "i16",     type_i16);
		create_declaration(ws.global_scope, "i32",     type_i32);
		create_declaration(ws.global_scope, "i64",     type_i64);
		create_declaration(ws.global_scope, "int",     type_i32);
		create_declaration(ws.global_scope, "u8",      type_u8);
		create_declaration(ws.global_scope, "u16",     type_u16);
		create_declaration(ws.global_scope, "u32",     type_u32);
		create_declaration(ws.global_scope, "u64",     type_u64);
		create_declaration(ws.global_scope, "uint",    type_u32);
		create_declaration(ws.global_scope, "f32",     type_f32);
		create_declaration(ws.global_scope, "f64",     type_f64);
		create_declaration(ws.global_scope, "float",   type_f32);
		create_declaration(ws.global_scope, "bool",    type_bool);
		create_declaration(ws.global_scope, "rawptr",  type_rawptr);



		string_fields := [?]Field {
			{"data",   get_or_make_type_ptr_to(ws, type_u8)},
			{"length", type_int},
		};
		type_string = make_type_struct(ws, "string", string_fields[:]); create_declaration(ws.global_scope, "string", type_string);

		// slice_fields := [?]Field{
		// 	{"data",   get_or_make_type_ptr_to(ws, type_u8)},
		// 	{"length", type_int},
		// };
		// type_slice = make_type_struct(ws, "slice", slice_fields[:]);

		// dynamic_array_fields := [?]Field{
		// 	{"data",   get_or_make_type_ptr_to(ws, type_u8)},
		// 	{"length", type_int},
		// };
		// type_dynamic_array = make_type_struct(ws, "dynamic array", dynamic_array_fields[:]);

		// todo:(josh): currently operators are preserved when making a distinct type, so adding two type_ids together will not error as it should
		type_type_id = make_type(ws, INT_SIZE, Type_Primitive{"type_id"}, {}); create_declaration(ws.global_scope, "type_id", type_type_id);

		type_untyped_int   = make_type(ws, 0, Type_Untyped{"untyped_int"  }, Type_Flags.Untyped | Type_Flags.Number | Type_Flags.Integer);
		type_untyped_float = make_type(ws, 0, Type_Untyped{"untyped_float"}, Type_Flags.Untyped | Type_Flags.Number | Type_Flags.Float);



		for t in ([?]^Type{type_i8, type_i16, type_i32, type_i64, type_untyped_int}) {
			add_equality_operators(t, i64);
			add_less_and_greater_than_operators(t, i64);
			add_math_operators(t, i64);
			add_mod_and_mod_mod(t, i64);
		}
		// uints
		for t in ([?]^Type{type_u8, type_u16, type_u32, type_u64}) {
			add_equality_operators(t, i64);
			add_less_and_greater_than_operators(t, i64);
			add_math_operators(t, i64);
		}
		// floats
		for t in ([?]^Type{type_f32, type_f64, type_untyped_float}) {
			add_equality_operators(t, f64);
			add_less_and_greater_than_operators(t, f64);
			add_math_operators(t, f64);
		}

		// string
		add_equality_operators(type_string, string);
		type_add_operator(type_string, .Plus, type_string, proc(a, b: Constant_Value) -> Constant_Value { return aprint(a.(string), b.(string)); }, true);
		// bool
		add_equality_operators(type_bool, bool);
		// type_id
		add_equality_operators(type_type_id, TypeID);



		add_math_operators :: proc(type: ^Type, $Constant_Type: typeid) {
			assert(type != nil);
			type_add_operator(type, .Plus,     type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) + b.(Constant_Type); });
			type_add_operator(type, .Minus,    type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) - b.(Constant_Type); });
			type_add_operator(type, .Multiply, type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) * b.(Constant_Type); });
			type_add_operator(type, .Divide,   type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) / b.(Constant_Type); });
		}

		add_mod_and_mod_mod :: proc(type: ^Type, $Constant_Type: typeid) {
			assert(type != nil);
			type_add_operator(type, .Mod,     type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) %  b.(Constant_Type); });
			type_add_operator(type, .Mod_Mod, type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) %% b.(Constant_Type); });
		}

		add_equality_operators :: proc(type: ^Type, $Constant_Type: typeid) {
			assert(type != nil);
			type_add_operator(type, .Boolean_Equal,     type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) == b.(Constant_Type); });
			type_add_operator(type, .Boolean_Not_Equal, type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) != b.(Constant_Type); });
		}

		add_less_and_greater_than_operators :: proc(type: ^Type, $Constant_Type: typeid) {
			assert(type != nil);
			type_add_operator(type, .Boolean_Less_Than,             type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) <  b.(Constant_Type); });
			type_add_operator(type, .Boolean_Greater_Than,          type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) >  b.(Constant_Type); });
			type_add_operator(type, .Boolean_Less_Than_Or_Equal,    type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) <= b.(Constant_Type); });
			type_add_operator(type, .Boolean_Greater_Than_Or_Equal, type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) >= b.(Constant_Type); });

		}
	}
}

typecheck_workspace :: proc(ws: ^Workspace) -> bool {
	using Check_State;

	init_builtin_types(ws);

	if !resolve_identifiers(ws) {
		return false;
	}

	for len(ws.nodes_to_typecheck) > 0 {
		checked_this_iteration := false;

		check_loop:
		for idx := len(ws.nodes_to_typecheck)-1; idx >= 0; idx -= 1 {
			node := ws.nodes_to_typecheck[idx];

			assert(node.check_state == Unchecked);

			for dependency in node.depends {
				if dependency.check_state != Checked {
					continue check_loop;
				}
			}

			check_result := typecheck_one_node(ws, node);
			#complete
			switch check_result {
				case Check_Result.Ok: {
					unordered_remove(&ws.nodes_to_typecheck, idx);
					checked_this_iteration = true;
				}
				case Check_Result.Not_Checked: {
					// no probs, we'll try again later
				}
				case Check_Result.Error: {
					return false;
				}
				case: {
					unhandledcase(check_result);
				}
			}
		}

		if !checked_this_iteration {
			logln("Made no progress, writing dependency graph to dependencies.txt...");
			sb: strings.Builder;
			sbprint(&sb, "digraph Dependencies {\n");

			for node in ws.nodes_to_typecheck {
				for depend in node.depends {
					if depend.check_state == Checked do continue;

					sbprint(&sb, "\"", get_union_type_info(node.derived),   " ", node.check_state,   " ", site(node.root_token.site), "\"");
					sbprint(&sb, " -> ", );
					sbprint(&sb, "\"", get_union_type_info(depend.derived), " ", depend.check_state, " ", site(depend.root_token.site), "\"", "\n");
				}
			}

			sbprint(&sb, "}\n");
			os.write_entire_file("dependencies.txt", cast([]u8)strings.to_string(sb)[:]);
			return false;
		}
	}

	return true;
}

Check_Result :: enum {
	Ok,
	Not_Checked,
	Error,
}
typecheck_one_node :: proc(using ws: ^Workspace, node: ^Ast_Node) -> Check_Result {
	#complete
	switch kind in &node.derived {
		case Ast_Number: {
			t: ^Type;
			#complete
			switch value in kind.value {
				case f64: {
					t = type_untyped_float;
					node.constant_value = value;
				}
				case i64: {
					t = type_untyped_int;
					node.constant_value = value;
				}
				case u64: {
					t = type_untyped_int;
					node.constant_value = value;
				}
			}

			complete_expr(node, t);
			return .Ok;
		}

		case Ast_String: {
			node.constant_value = kind.text;
			complete_expr(node, type_string);
			return .Ok;
		}

		case Ast_Unary: {
			// todo(josh): this is probably severely incomplete
			assert(kind.rhs.expr_type != nil);
			rhs_type := kind.rhs.expr_type;
			result_type := rhs_type;
			switch kind.op {
				case .Boolean_Not: {
					if rhs_type != type_bool {
						error(node, "Unary operator `!` is only valid for type `bool`. Given: ", type_to_string(rhs_type));
						return .Error;
					}
					if kind.rhs.constant_value != nil {
						node.constant_value = !kind.rhs.constant_value.(bool);
					}
				}
				case .Plus: {
					if !is_numeric_type(rhs_type) {
						error(node, "Unary operator `+` is only valid for numeric types. Given: ", type_to_string(rhs_type));
						return .Error;
					}
					// unary `+` doesn't actually do anything
				}
				case .Minus: {
					if !is_numeric_type(rhs_type) {
						error(node, "Unary operator `-` is only valid for numeric types. Given: ", type_to_string(rhs_type));
						return .Error;
					}

					if kind.rhs.constant_value != nil {
						switch constant_kind in kind.rhs.constant_value {
							case i64: node.constant_value = -constant_kind;
							case f64: node.constant_value = -constant_kind;
							case:     assert(false, tprint(constant_kind));
						}
					}
				}
				case .Dereference: {
					ptr, ok := kind.rhs.expr_type.kind.(Type_Ptr);
					if !ok {
						error(node, "Cannot dereference a non-pointer type ", type_to_string(rhs_type));
						return .Error;
					}
					result_type = ptr.ptr_to;
				}
				case .Address: { // take address
					result_type = get_or_make_type_ptr_to(ws, rhs_type);
				}
				case .Ampersand: assert(false, "Ampersand should have gotten converted to Address by now");
				case: {
					logln(site(node.root_token.site));
					unhandledcase(kind.op);
				}
			}

			complete_expr(node, result_type);
			return .Ok;
		}

		case Ast_Binary: {
			ltype := kind.lhs.expr_type;
			rtype := kind.rhs.expr_type;

			if !is_assignable_to(rtype, ltype) {
				type_mismatch(ltype, kind.rhs);
				return .Error;
			}

			// result_type := get_result_type(ltype, rtype);
			// if result_type == nil {
			// 	type_mismatch(ltype, kind.rhs, node);
			// 	return .Error;
			// }

			operator_info, ok := type_get_operator(ltype, kind.op);
			if !ok {
				error(kind.lhs, "Binary operator ", kind.op, " does not exist for type ", type_to_string(ltype));
				return .Error;
			}

			both_constant := kind.lhs.constant_value != nil && kind.rhs.constant_value != nil;
			if both_constant {
				assert(operator_info.constant_evaluation_procedure != nil);
				result := operator_info.constant_evaluation_procedure(kind.lhs.constant_value, kind.rhs.constant_value);
				assert(result != nil);
				node.constant_value = result;
			}
			else {
				if operator_info.constant_only {
					error(kind.lhs, "Binary operator ", kind.op, " for type ", type_to_string(ltype), " requires two constant values.");
				}
			}

			complete_expr(node, operator_info.result_type);
			return .Ok;
		}

		case Ast_Selector: {
			assert(kind.left.expr_type != nil);
			left := kind.left.expr_type;
			left_struct, ok := left.kind.(Type_Struct);
			if !ok {
				error(node, "Selector only works for struct types right now.");
				return .Error;
			}
			for field, field_idx in left_struct.fields {
				if field == kind.field {
					complete_expr(node, left_struct.types[field_idx]);
					return .Ok;
				}
			}

			error(node, "Type ", type_to_string(left), " doesn't have a field '", kind.field, "'");
			return .Error;
		}

		case Ast_Subscript: {
			assert(kind.left.expr_type != nil);
			assert(kind.index.expr_type != nil);
			if !is_assignable_to(kind.index.expr_type, type_int) {
				type_mismatch(type_int, kind.index);
				return .Error;
			}

			left := kind.left.expr_type;
			t := (&left.kind.(Type_Array)).array_of;
			complete_expr(node, t);
			return .Ok;
		}

		case Ast_Cast: {
			target := get_type(ws, kind.typespec.base.constant_value.(TypeID));
			// todo: check if valid cast (can't cast string to int, for example)
			complete_expr(node, target);
			return .Ok;
		}

		case Ast_Identifier: {
			assert(kind.declaration != nil);
			// Symbols aren't AST nodes, so the Ast_Identifier can't depend() on it like normal. Oh well.
			if kind.declaration.kind == nil {
				return Check_Result.Not_Checked;
			}

			#complete
			switch decl_type in kind.declaration.kind {
				case Type_Decl: {
					assert(decl_type.type.id != 0);
					complete_expr(node, type_type_id);
				}
				case Proc_Decl: {
					assert(decl_type.type != nil);
					complete_expr(node, decl_type.type);
				}
				case Var_Decl: {
					assert(decl_type.type != nil);
					complete_expr(node, decl_type.type);
				}
				case: {
					unhandledcase(decl_type);
				}
			}

			if kind.constant_value != nil {
				node.constant_value = kind.constant_value;
			}
			return .Ok;
		}

		case Ast_Directive: {
			if kind.directive == "#odin" {
				complete_node(node);
				return .Ok;
			}
		}

		case Ast_Directive_Assert: {
			value, ok := kind.condition.constant_value.(bool);
			if kind.condition.expr_type != type_bool || !ok {
				error(kind.condition, "#assert condition must be a constant boolean");
				return .Error;
			}
			if value != true {
				error(node, "Assertion failed.");
				return .Error;
			}
			complete_node(node);
			return .Ok;
		}

		case Ast_Directive_Include: {
			complete_node(node);
			return .Ok;
		}

		case Ast_Block: {
			complete_node(node);
			return .Ok;
		}

		case Ast_Var: {
			declared_type: ^Type;
			if kind.typespec != nil {
				declared_type = get_type(ws, kind.typespec.base.constant_value.(TypeID));
			}
			expr_type: ^Type;
			if kind.expr != nil {
				expr_type = kind.expr.expr_type;
			}

			true_type: ^Type;
			if declared_type != nil && expr_type != nil {
				// they specified a type _and_ have an expression, so make sure the types match
				if !is_assignable_to(expr_type, declared_type) {
					type_mismatch(declared_type, kind.expr);
					return .Error;
				}
				true_type = declared_type;
			}

			if declared_type != nil {
				true_type = declared_type;
			}
			else if expr_type != nil {
				true_type = expr_type;
			}
			else {
				error(node, "Either a type or value is required for a variable declaration.");
				return .Error;
			}

			if is_untyped_type(true_type) {
				if      true_type == type_untyped_int   do true_type = type_int;
				else if true_type == type_untyped_float do true_type = type_float;
				else do assert(false, tprint("Unhandled untyped type: ", true_type.kind));
			}


			assert(true_type != nil);

			kind.type = true_type;

			if kind.is_constant && true_type == type_type_id {
				assert(kind.expr != nil);
				node.do_not_print = true; // don't print type aliases
				complete_declaration(kind.declaration, Type_Decl{get_type(ws, kind.expr.constant_value.(TypeID))});
			}
			else {
				complete_declaration(kind.declaration, Var_Decl{true_type, kind});
			}

			if kind.is_constant {
				assert(kind.expr != nil);
				if kind.expr.constant_value == nil {
					error(node, "Constants require a constant value expression.");
					return .Error;
				}
				node.constant_value = kind.expr.constant_value;
				kind.declaration.constant_value = node.constant_value;
			}

			complete_node(node);

			if ws.current_scope == ws.global_scope {
				append(&ws.all_global_variables, kind);
			}

			return .Ok;
		}

		case Ast_Proc: {
			if kind.return_typespec != nil {
				kind.return_type = get_type(ws, kind.return_typespec.base.constant_value.(TypeID));
			}

			t := get_or_make_type_proc(ws, kind);
			kind.signature_type = t;
			complete_node(node);
			complete_declaration(kind.declaration, Proc_Decl{t, kind});
			return .Ok;
		}

		case Ast_Struct: {
			fields: [dynamic]Field;
			for field in kind.fields {
				append(&fields, Field{field.name, field.type});
			}
			t := make_type_struct(ws, kind.name, fields[:]);
			complete_node(node);
			complete_declaration(kind.declaration, Type_Decl{t});
			kind.declaration.constant_value = t.id;
			return .Ok;
		}

		case Ast_Typedef: {
			t := make_type_distinct(ws, kind.name, get_type(ws, kind.other.base.constant_value.(TypeID)));
			complete_node(node);
			complete_declaration(kind.declaration, Type_Decl{t});
			kind.declaration.constant_value = t.id;
			return .Ok;
		}

		case Ast_Call: {
			target_type := kind.procedure.expr_type;
			proc_type, ok := target_type.kind.(Type_Proc);
			assert(ok);

			if len(kind.args) != len(proc_type.params) {
				error(node, "Expected ", len(proc_type.params), " parameters, got ", len(kind.args));
				return .Error;
			}

			for _, idx in kind.args {
				arg := kind.args[idx];
				param := proc_type.params[idx];
				if !is_assignable_to(arg.expr_type, param.inferred_type) {
					type_mismatch(param.inferred_type, arg);
					return .Error;
				}
			}

			complete_expr(node, proc_type.return_type, true);
			return .Ok;
		}

		// case Ast_Switch: {
		// 	complete_node(node);
		// 	return .Ok;
		// }

		// case Ast_Range: {
		// 	min_type: ^Type;
		// 	max_type: ^Type;

		// 	if kind.min == nil {
		// 		min_type = type_int;
		// 	}
		// 	else {
		// 		assert(kind.min.expr_type != nil);
		// 		min_type = kind.min.expr_type;
		// 		ensure_types_match(type_int, kind.min, node);
		// 	}

		// 	if kind.max == nil {
		// 		max_type = type_int;
		// 	}
		// 	else {
		// 		assert(kind.max.expr_type != nil);
		// 		max_type = kind.max.expr_type;
		// 		ensure_types_match(type_int, kind.max, node);
		// 	}

		// 	t: ^Type;
		// 	if kind.lhs != nil {
		// 		assert(kind.lhs.expr_type != nil);
		// 		t = kind.lhs.expr_type;
		// 	}
		// 	else {
		// 		t = type_int;
		// 	}
		// 	complete_expr(node, t);
		// 	return .Ok;
		// }

		// case Ast_Slice: {
		// 	assert(kind.array.expr_type != nil);
		// 	array_type := kind.array.expr_type;
		// 	array_of: ^Type;
		// 	switch array_kind in array_type.kind {
		// 		case Type_Array: {
		// 			array_of = array_kind.array_of;
		// 		}
		// 		case Type_List: {
		// 			array_of = array_kind.array_of;
		// 		}
		// 		case Type_Slice: {
		// 			array_of = array_kind.slice_of;
		// 		}
		// 		case: {
		// 			error(kind.array, "Cannot slice type: ", type_to_string(array_type));
		// 			return .Error;
		// 		}
		// 	}
		// 	slice_type := get_or_make_type_slice_of(ws, array_of);
		// 	complete_expr(node, slice_type);
		// 	return .Ok;
		// }

		case Ast_For_I: {
			panic("Unsupported");
		}

		case Ast_Sizeof: {
			panic("Unsupported");
		}

		case Ast_Null: {
			complete_node(node);
			return .Ok;
		}

		case Ast_Paren: {
			assert(kind.nested_expr.expr_type != nil);
			complete_node(node);
			return .Ok;
		}

		case Ast_If: {
			if !is_assignable_to(kind.condition.expr_type, type_bool) {
				type_mismatch(type_bool, kind.condition);
				return .Error;
			}
			complete_node(node);
			return .Ok;
		}

		case Ast_Else_If: {
			if !is_assignable_to(kind.condition.expr_type, type_bool) {
				type_mismatch(type_bool, kind.condition);
				return .Error;
			}
			complete_node(node);
			return .Ok;
		}

		case Ast_While: {
			if !is_assignable_to(kind.condition.expr_type, type_bool) {
				type_mismatch(type_bool, kind.condition);
				return .Error;
			}
			complete_node(node);
			return .Ok;
		}

		// case Ast_For_Each: {
		// 	assert(kind.array.expr_type != nil);
		// 	assert(kind.var.base.check_state == .Checked);

		// 	array_expr_type := kind.array.expr_type;
		// 	if !is_slice_type(array_expr_type) && !is_array_type(array_expr_type) && !is_list_type(array_expr_type) {
		// 		error(kind.array, "Cannot iterate over type: ", type_to_string(array_expr_type));
		// 		return .Error;
		// 	}

		// 	array_of := get_base_type(kind.array.expr_type);
		// 	if array_of !=
		// 	if is_assignable_to()
		// 	ensure_types_match(t, kind.var.base, kind.var.base);
		// 	complete_node(node);
		// 	return .Ok;
		// }

		case Ast_Assign: {
			assert(kind.left.expr_type != nil);
			assert(kind.right.expr_type != nil);

			if !is_assignable_to(kind.right.expr_type, kind.left.expr_type) {
				type_mismatch(kind.left.expr_type, kind.right);
				return .Error;
			}
			complete_node(node);
			return .Ok;
		}

		case Ast_Return: {
			assert(kind.procedure.signature_type != nil);
			return_type := kind.procedure.signature_type.kind.(Type_Proc).return_type;
			expr_type := kind.expr.expr_type;
			if !is_assignable_to(expr_type, return_type) {
				type_mismatch(return_type, kind.expr);
				return .Error;
			}
			complete_node(node);
			return .Ok;
		}

		case Ast_Type_Expression: {
			node.constant_value = kind.typespec.base.constant_value.(TypeID);
			complete_expr(node, type_type_id);
			return .Ok;
		}

		case Ast_Typespec: {
			#complete
			switch typespec_kind in kind.kind {
				case Typespec_Identifier: {
					switch decl_type in typespec_kind.ident.declaration.kind {
						case Type_Decl: {
							assert(decl_type.type != nil);
							complete_typespec(kind, decl_type.type);
							return .Ok;
						}
						case: {
							error(typespec_kind.ident, "Expected a type, got ", typespec_kind.ident.name);
							return .Error;
						}
					}
				}

				case Typespec_List: {
					list_of := get_type(ws, typespec_kind.typespec.base.constant_value.(TypeID));
					t := get_or_make_type_list_of(ws, list_of);
					complete_typespec(kind, t);
					return .Ok;
				}

				case Typespec_Array: {
					if typespec_kind.length_expr.constant_value == nil {
						error(typespec_kind.length_expr, "Array sizes require a constant integer value.");
						return .Error;
					}
					array_length, ok := typespec_kind.length_expr.constant_value.(i64);
					if !ok {
						error(typespec_kind.length_expr, "Array sizes require a constant integer value.");
						return .Error;
					}
					array_of := get_type(ws, typespec_kind.typespec.base.constant_value.(TypeID));
					t := get_or_make_type_array_of(ws, cast(uint)array_length, array_of);
					complete_typespec(kind, t);
					return .Ok;
				}

				case Typespec_Slice: {
					slice_of := get_type(ws, typespec_kind.typespec.base.constant_value.(TypeID));
					t := get_or_make_type_slice_of(ws, slice_of);
					complete_typespec(kind, t);
					return .Ok;
				}

				case Typespec_Ptr: {
					ptr_to := get_type(ws, typespec_kind.typespec.base.constant_value.(TypeID));
					t := get_or_make_type_ptr_to(ws, ptr_to);
					complete_typespec(kind, t);
					return .Ok;
				}
				case Typespec_Union: {
					unhandledcase(typespec_kind);
					return .Error;
				}
				case: {
					unhandledcase(typespec_kind);
					return .Error;
				}
			}
			unreachable();
			return .Error;
		}

		// todo(josh): union literals, disabling support for this for now
		// case Ast_Typespec_Union: {
		// 	types: [dynamic]^Type;
		// 	biggest_nested_type: uint;
		// 	for spec in kind.types {
		// 		assert(spec.inferred_type != nil);
		// 		canonical_type := spec.inferred_type;
		// 		if canonical_type.size > biggest_nested_type do biggest_nested_type = canonical_type.size;
		// 		append(&types, canonical_type);
		// 	}

		// 	union_type: ^Type;
		// 	if all_types != nil {
		// 		type_loop:
		// 		for other_type in all_types {
		// 			other_union, ok := other_type.kind.(Type_Union);
		// 			if !ok do continue;
		// 			if len(types) == len(other_union.types) do continue;
		// 			if (cast(^Type)&other_union).size != biggest_nested_type do continue;

		// 			for _, idx in types {
		// 				if types[idx] != other_union.types[idx] {
		// 					continue type_loop;
		// 				}
		// 			}
		// 			union_type = other_type;
		// 			break type_loop;
		// 		}
		// 	}

		// 	if union_type == nil {
		// 		union_type = make_type(biggest_nested_type, Type_Union{types});
		// 	}

		// 	append(&all_types, union_type);
		// 	node_complete(node, union_type);
		// 	return .Ok;
		// }

		case Ast_Comment: {
			complete_node(node);
			return .Ok;
		}

		case: {
			unhandledcase(kind^);
		}
	}

	unreachable();
	return .Error;
}



// get_result_type :: proc(lhs: ^Type, rhs: ^Type) -> ^Type {
// 	if lhs == nil && rhs != nil do return nil;
// 	if lhs != nil && rhs == nil do return nil;

// 	if lhs == rhs do return lhs;

// 	if is_untyped_type(lhs) do return rhs;

// 	return lhs;
// }

type_mismatch :: proc(wanted: ^Type, given: ^Ast_Node, loc := #caller_location) {
	error(given, "Type mismatch: wanted ", type_to_string(wanted), " given ", type_to_string(given.expr_type));
}

// ensure_types_match :: proc(wanted: ^Type, given: ^Ast_Node, error_node: ^Ast_Node, loc := #caller_location) -> bool {
// 	if !types_match(wanted, given.expr_type, loc) {
// 		type_mismatch(wanted, given, error_node, loc);
// 		return false;
// 	}
// 	return true;
// }

is_assignable_to :: inline proc(rhs: ^Type, lhs: ^Type, loc := #caller_location) -> bool {
	//
	if lhs == nil && rhs != nil do return false;
	if lhs != nil && rhs == nil do return false;

	//
	if lhs == rhs do return true;

	if is_untyped_type(rhs) {
		if is_integer_type(rhs) && is_integer_type(lhs) {
			return true;
		}
		if is_float_type(rhs)   && is_float_type(lhs) {
			return true;
		}

		// any number can be assigned to floats
		if is_numeric_type(rhs) && is_float_type(lhs) {
			return true;
		}
	}

	// todo(josh): untyped_numbers
	//
	// if is_signed_integer_type(wanted) && given == type_untyped_int do return true;

	// if is_float_type(wanted) && given == type_untyped_int   do return true;
	// if is_float_type(wanted) && given == type_untyped_float do return true;

	//
	// @UnionTypes
	// if union_type, ok := wanted.kind.(Type_Union); ok {
	// 	for kind in union_type.types {
	// 		if given == kind do return true;
	// 	}

	// 	return false;
	// }

	return false;
}



is_numeric_type :: proc(t: ^Type) -> bool {
	if t.flags & cast(u32)Type_Flags.Number > 0 {
		return true;
	}
	return false;
}

is_integer_type :: proc(t: ^Type) -> bool {
	if t.flags & cast(u32)Type_Flags.Integer > 0 {
		return true;
	}
	return false;
}

is_float_type :: proc(t: ^Type) -> bool {
	if t.flags & cast(u32)Type_Flags.Float > 0 {
		return true;
	}
	return false;
}

is_signed_type :: proc(t: ^Type) -> bool {
	if t.flags & cast(u32)Type_Flags.Signed > 0 {
		return true;
	}
	return false;
}

is_unsigned_type :: proc(t: ^Type) -> bool {
	if t.flags & cast(u32)Type_Flags.Unsigned > 0 {
		return true;
	}
	return false;
}

is_untyped_type :: proc(t: ^Type) -> bool {
	if t.flags & cast(u32)Type_Flags.Untyped > 0 {
		return true;
	}
	return false;
}

is_pointer_type :: proc(t: ^Type) -> bool {
	if t.flags & cast(u32)Type_Flags.Pointer > 0 {
		return true;
	}
	return false;
}

is_array_type :: proc(t: ^Type) -> bool {
	switch kind in t.kind {
		case Type_Array: return true;
	}
	return false;
}
is_slice_type :: proc(t: ^Type) -> bool {
	switch kind in t.kind {
		case Type_Slice: return true;
	}
	return false;
}
is_list_type :: proc(t: ^Type) -> bool {
	switch kind in t.kind {
		case Type_List: return true;
	}
	return false;
}




// note(josh): need some kind of system for this, 8 is just what the VM's alignment is so we'll use that for now
TARGET_PLATFORM_ALIGNMENT :: 8;

make_type :: proc(ws: ^Workspace, size: uint, derived: $T, flags: Type_Flags, aligned_size_override : uint = 0, loc := #caller_location) -> ^Type {
	new_type := new(Type);
	new_type.id = cast(TypeID)len(ws.all_types)+1;
	new_type.packed_size = size;
	if aligned_size_override == 0 {
		new_type.aligned_size = align_forward(size, TARGET_PLATFORM_ALIGNMENT);
	}
	else {
		new_type.aligned_size = aligned_size_override;
	}
	assert(new_type.aligned_size % TARGET_PLATFORM_ALIGNMENT == 0);
	new_type.register_size = cast(u64)new_type.aligned_size / TARGET_PLATFORM_ALIGNMENT;
	new_type.kind = derived;
	new_type.flags = cast(u32)flags;
	append(&ws.all_types, new_type);
	return new_type;
}

align_forward :: proc(val: uint, align: uint) -> uint {
	a := align;
	p := val;
	modulo := p & (a-1);
	if modulo != 0 do p += a - modulo;
	return p;
}

get_type :: proc(ws: ^Workspace, id: TypeID) -> ^Type {
	assert(id != 0);
	return ws.all_types[cast(int)id-1];
}

make_type_distinct :: proc(ws: ^Workspace, new_name: string, t: ^Type) -> ^Type {
	assert(t != nil);
	new_t := new_clone(t^);
	new_t.id = cast(TypeID)len(ws.all_types)+1;
	return new_t;
}

make_type_struct :: proc(ws: ^Workspace, name: string, fields: []Field) -> ^Type {
	size : uint = 0;
	aligned_size : uint = 0;
	// lotsa allocations here that probably dont need to be here
	names: [dynamic]string;
	offsets: [dynamic]u64;
	types: [dynamic]^Type;
	for var in fields {
		assert(var.inferred_type != nil);
		append(&names, var.name);
		append(&offsets, cast(u64)aligned_size);
		append(&types, var.inferred_type);

		size += var.inferred_type.packed_size;
		aligned_size += var.inferred_type.aligned_size;
	}

	assert(size != 0);
	new_type := make_type(ws, size, Type_Struct{name, names[:], types[:], offsets[:]}, {}, aligned_size);
	return new_type;
}

type_add_operator :: proc(type: ^Type, operator: Operator, result_type: ^Type, constant_eval_proc: proc(Constant_Value, Constant_Value) -> Constant_Value, constant_only := false, loc := #caller_location) {
	assert(type != nil, tprint(loc));
	assert(result_type != nil, tprint(loc));
	type.operators[operator] = Operator_Info{result_type, constant_eval_proc, constant_only};
}

type_get_operator :: proc(type: ^Type, operator: Operator) -> (Operator_Info, bool) {
	info, ok := type.operators[operator];
	return info, ok;
}

get_or_make_type_proc :: proc(using ws: ^Workspace, declaration: ^Ast_Proc) -> ^Type {
	if all_types != nil {
		type_loop:
		for canonical_type in all_types {
			other_type, ok := canonical_type.kind.(Type_Proc);
			if !ok do continue;
			if len(other_type.params) != len(declaration.params) do continue;
			if other_type.return_type != declaration.return_type do continue;

			for _, idx in other_type.params {
				this_param_type  := declaration.params[idx].base.expr_type;
				other_param_type := other_type.params[idx].inferred_type;
				if other_param_type != this_param_type {
					continue type_loop;
				}
			}

			return canonical_type;
		}
	}

	params: [dynamic]Field;
	for _, idx in declaration.params {
		param := declaration.params[idx];
		t := param.type;
		append(&params, Field{param.name, t});
	}

	new_type := make_type(ws, POINTER_SIZE, Type_Proc{params[:], declaration.return_type}, Type_Flags.Procedure);
	return new_type;
}

get_base_type :: proc(type: ^Type) -> ^Type {
	switch kind in type.kind {
		case Type_List: {
			return kind.list_of;
		}
		case Type_Array: {
			return kind.array_of;
		}
		case Type_Slice: {
			return kind.slice_of;
		}
		case Type_Ptr: {
			return kind.ptr_to;
		}
		case: {
			unhandledcase(kind);
		}
	}

	unreachable();
	return nil;
}

get_or_make_type_ptr_to :: proc(using ws: ^Workspace, ptr_to: ^Type) -> ^Type {
	if all_types != nil {
		for canonical_type in all_types {
			if ptr, ok := canonical_type.kind.(Type_Ptr); ok {
				if ptr.ptr_to == ptr_to {
					return canonical_type;
				}
			}
		}
	}

	type_ptr := make_type(ws, POINTER_SIZE, Type_Ptr{ptr_to}, Type_Flags.Pointer);
	return type_ptr;
}

get_or_make_type_array_of :: proc(using ws: ^Workspace, length: uint, array_of: ^Type) -> ^Type {
	assert(array_of.packed_size != 0);
	if all_types != nil {
		for other_type in all_types {
			if other_array, ok := other_type.kind.(Type_Array); ok {
				if other_length := other_array.length; other_length == length {
					if array_of == other_array.array_of {
						return other_type;
					}
				}
			}
		}
	}

	array_type := make_type(ws, array_of.aligned_size * length, Type_Array{length, array_of}, {});
	return array_type;
}

get_or_make_type_list_of :: proc(using ws: ^Workspace, list_of: ^Type) -> ^Type {
	if all_types != nil {
		for other_type in all_types {
			if other_list, ok := other_type.kind.(Type_List); ok {
				if list_of == other_list.list_of {
					return other_type;
				}
			}
		}
	}

	array_type := make_type(ws, POINTER_SIZE + INT_SIZE, Type_List{list_of}, {}); // ptr + length
	return array_type;
}

get_or_make_type_slice_of :: proc(using ws: ^Workspace, slice_of: ^Type) -> ^Type {
	if all_types != nil {
		for other_type in all_types {
			if other_slice, ok := other_type.kind.(Type_Slice); ok {
				if slice_of == other_slice.slice_of {
					return other_type;
				}
			}
		}
	}

	type_slice := make_type(ws, SLICE_SIZE, Type_Slice{slice_of}, {});
	return type_slice;
}



complete_typespec :: inline proc(typespec: ^Ast_Typespec, t: ^Type, loc := #caller_location) {
	assert(typespec != nil);
	assert(t != nil);
	typespec.completed_type = t;
	typespec.constant_value = t.id;
	complete_expr(typespec.base, type_type_id);
}

complete_expr :: inline proc(node: ^Ast_Node, t: ^Type, nil_type_is_ok := false, loc := #caller_location) {
	assert(node != nil, tprint("node was nil at ", loc));
	if !nil_type_is_ok {
		assert(t != nil, tprint("t was nil at ", loc));
	}

	node.expr_type = t;
	complete_node(node);
}

complete_node :: inline proc(node: ^Ast_Node) {
	node.check_state = Check_State.Checked;
}



error :: proc{node_error, site_error};
node_error :: proc(base: ^Ast_Node, args: ..any) {
	site_error(base.root_token.site, ..args);
}

site_error :: proc(site_var: Site, args: ..any) {
	print(site(site_var), " ");
	print(..args);
	print("\n");
}










type_to_string :: proc(canonical_type: ^Type) -> string {
	if canonical_type == nil do return "<nil>";

	#complete
	switch kind in canonical_type.kind {
		case Type_Primitive: {
			return kind.name;
		}
		case Type_Struct: {
			return kind.name;
		}
		case Type_List: {
			return aprint("[:]", type_to_string(kind.list_of));
		}
		case Type_Array: {
			return aprint("[", kind.length, "]", type_to_string(kind.array_of));
		}
		case Type_Slice: {
			return aprint("[]", type_to_string(kind.slice_of));
		}
		case Type_Ptr: {
			return aprint("^", type_to_string(kind.ptr_to));
		}
		case Type_Proc: {
			buf: strings.Builder;
			sbprint(&buf, "proc(");
			comma := "";
			for param in kind.params {
				assert(param.inferred_type != nil);
				sbprint(&buf, comma, type_to_string(param.inferred_type));
				comma = ", ";
			}
			sbprint(&buf, ")");
			if kind.return_type != nil {
				sbprint(&buf, " ", type_to_string(kind.return_type));
			}
			return strings.to_string(buf);
		}
		case Type_Untyped: {
			return kind.name;
		}

		// @UnionTypes
		// case Type_Union: {
		// 	buf: strings.Builder;
		// 	sbprint(&buf, "union {");
		// 	comma := "";
		// 	for canonical_type in kind.types {
		// 		sbprint(&buf, comma, type_to_string(canonical_type));
		// 		comma = ", ";
		// 	}

		// 	sbprint(&buf, "}");
		// 	return strings.to_string(buf);
		// }

		case: {
			unhandledcase(kind);
		}
	}

	unreachable();
	return "";
}

unhandledcase :: proc(value: $T, loc := #caller_location) -> ! {
	panic(tprint("Unhandled case at ", pretty_location(loc), ": ", value));
}














// value := evaluate_constant_value(kind.op, kind.lhs.constant_value, kind.rhs.constant_value);
				// assert(value != nil); // nil means the types didn't match we shouldn't have gotten this far if that is true
				// node.constant_value = value;

				// evaluate_constant_value :: proc(op: , a, b: Constant_Value) -> Constant_Value {
				// 	assert(a != nil);
				// 	assert(b != nil);

				// 	// we can assume the types of a and b match

				// 	value: Constant_Value;

				// 	#complete
				// 	switch lhs in a {
				// 	case TypeID: {
				// 		rhs := b.(TypeID);
				// 		switch op {
				// 			case .Equal:         value = lhs == rhs;
				// 			case .Not_Equal:     value = lhs != rhs;
				// 			case: unhandledcase(op);
				// 		}
				// 	}
				// 	case string: {
				// 		rhs := b.(string);
				// 		switch op {
				// 			case .Plus:          value = aprint(lhs, rhs);
				// 			case .Equal:         value = lhs == rhs;
				// 			case .Not_Equal:     value = lhs != rhs;

				// 			// todo(josh): these cases compile but I have no idea what they do
				// 			// case .Less:          value = lhs < rhs;
				// 			// case .Greater:       value = lhs > rhs;
				// 			// case .Less_Equal:    value = lhs <= rhs;
				// 			// case .Greater_Equal: value = lhs >= rhs;

				// 			case: unhandledcase(op);
				// 		}
				// 	}
				// 	case bool: {
				// 		rhs := b.(bool);
				// 		switch op {
				// 			case .Equal:         value = lhs == rhs;
				// 			case .Not_Equal:     value = lhs != rhs;
				// 			case .And_And:       value = lhs && rhs;
				// 			case .Or_Or:         value = lhs || rhs;
				// 			case: unhandledcase(op);
				// 		}
				// 	}
				// 	case f64: {
				// 		rhs := b.(f64);
				// 		switch op {
				// 			case .Plus:          value = lhs + rhs;
				// 			case .Minus:         value = lhs - rhs;
				// 			case .Multiply:      value = lhs * rhs;
				// 			case .Divide:        value = lhs / rhs;
				// 			case .Equal:         value = lhs == rhs;
				// 			case .Not_Equal:     value = lhs != rhs;
				// 			case .Less:          value = lhs < rhs;
				// 			case .Greater:       value = lhs > rhs;
				// 			case .Less_Equal:    value = lhs <= rhs;
				// 			case .Greater_Equal: value = lhs >= rhs;
				// 			case: unhandledcase(op);
				// 		}
				// 	}
				// 	case i64: {
				// 		rhs := b.(i64);
				// 		switch op {
				// 		case .Plus:     value = lhs + rhs;
				// 		case .Minus:    value = lhs - rhs;
				// 		case .Multiply: value = lhs * rhs;
				// 		case .Divide:   value = lhs / rhs;
				// 		case .Mod:      value = lhs % rhs;
				// 		case .Mod_Mod:  value = lhs %% rhs;
				// 		case .And:      value = lhs & rhs;
				// 		case .Or:       value = lhs | rhs;
				// 		case .Xor:      value = lhs ~ rhs;
				// 		case .LShift:   {
				// 			if rhs < 0 {
				// 				// todo(josh): error handling
				// 				logln("Shift amount must be an unsigned integer.");
				// 				return nil;
				// 			}
				// 			value = lhs << cast(u64)rhs;
				// 		}
				// 		case .RShift:   {
				// 			if rhs < 0 {
				// 				// todo(josh): error handling
				// 				logln("Shift amount must be an unsigned integer.");
				// 				return nil;
				// 			}
				// 			value = lhs >> cast(u64)rhs;
				// 		}
				// 		case .Equal:         value = lhs == rhs;
				// 		case .Not_Equal:     value = lhs != rhs;
				// 		case .Less:          value = lhs < rhs;
				// 		case .Greater:       value = lhs > rhs;
				// 		case .Less_Equal:    value = lhs <= rhs;
				// 		case .Greater_Equal: value = lhs >= rhs;
				// 		case: unhandledcase(op);
				// 		}
				// 	}
				// 	}

				// 	return value;
				// }