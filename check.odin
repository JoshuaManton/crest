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
// type_rawptr: ^Type;
// type_list: ^Type;
// type_slice: ^Type;

type_untyped_int:   ^Type;
type_untyped_uint:  ^Type;
type_untyped_float: ^Type;

type_type_id: ^Type;

constant_precedence_table: map[^Type]int;

init_builtin_types :: proc(using ws: ^Workspace) {
	if type_i8 == nil {
		type_i8   = make_type(ws, "i8",   1, Type_Integer{true},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_i16  = make_type(ws, "i16",  2, Type_Integer{true},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_i32  = make_type(ws, "i32",  4, Type_Integer{true},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_i64  = make_type(ws, "i64",  8, Type_Integer{true},  Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_u8   = make_type(ws, "u8",   1, Type_Integer{false}, Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_u16  = make_type(ws, "u16",  2, Type_Integer{false}, Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_u32  = make_type(ws, "u32",  4, Type_Integer{false}, Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_u64  = make_type(ws, "u64",  8, Type_Integer{false}, Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_f32  = make_type(ws, "f32",  4, Type_Float{},        Type_Flags.Number | Type_Flags.Float   | Type_Flags.Signed);
		type_f64  = make_type(ws, "f64",  8, Type_Float{},        Type_Flags.Number | Type_Flags.Float   | Type_Flags.Signed);
		type_bool = make_type(ws, "bool", 1, Type_Bool{},         {});

		type_int   = type_i32;
		type_uint  = type_u32;
		type_float = type_f32;

		// todo(josh)
		// type_rawptr = make_type(ws, POINTER_SIZE, Type_Primitive{"rawptr"}, Type_Flags.Pointer);

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

		// todo(josh)
		// create_declaration(ws.global_scope, "rawptr",  type_rawptr);



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
		type_type_id = make_type_distinct(ws, "type_id", type_int); create_declaration(ws.global_scope, "type_id", type_type_id);

		type_untyped_int   = make_type(ws, "untyped_int",   0, Type_Untyped{type_int},   Type_Flags.Untyped | Type_Flags.Number | Type_Flags.Integer | Type_Flags.Signed);
		type_untyped_uint  = make_type(ws, "untyped_uint",  0, Type_Untyped{type_uint},  Type_Flags.Untyped | Type_Flags.Number | Type_Flags.Integer | Type_Flags.Unsigned);
		type_untyped_float = make_type(ws, "untyped_float", 0, Type_Untyped{type_float}, Type_Flags.Untyped | Type_Flags.Number | Type_Flags.Float   | Type_Flags.Signed);

		constant_precedence_table[type_untyped_float] = 3;
		constant_precedence_table[type_untyped_int]   = 2;
		constant_precedence_table[type_untyped_uint]  = 1;



		// ints
		for t in ([?]^Type{type_i8, type_i16, type_i32, type_i64, type_untyped_int}) {
			add_equality_operators(t, i64);
			add_less_and_greater_than_operators(t, i64);
			add_math_operators(t, i64);
			add_mod_and_mod_mod(t, i64);
			type_add_unary_operator(t, .Minus, t, proc(a: Constant_Value) -> Constant_Value { return -a.(i64); });
		}
		// uints
		for t in ([?]^Type{type_u8, type_u16, type_u32, type_u64, type_untyped_uint}) {
			add_equality_operators(t, u64);
			add_less_and_greater_than_operators(t, u64);
			add_math_operators(t, u64);
		}
		type_add_unary_operator(type_untyped_uint, .Minus, type_untyped_int, proc(a: Constant_Value) -> Constant_Value { return -(cast(i64)a.(u64)); });
		// floats
		for t in ([?]^Type{type_f32, type_f64, type_untyped_float}) {
			add_equality_operators(t, f64);
			add_less_and_greater_than_operators(t, f64);
			add_math_operators(t, f64);
			type_add_unary_operator(t, .Minus, t, proc(a: Constant_Value) -> Constant_Value { return -a.(f64); });
		}

		// string
		add_equality_operators(type_string, string);
		type_add_binary_operator(type_string, .Plus, type_string, proc(a, b: Constant_Value) -> Constant_Value { return aprint(a.(string), b.(string)); }, true);

		// bool
		add_equality_operators(type_bool, bool);
		type_add_unary_operator(type_bool, .Boolean_Not, type_bool, proc(a: Constant_Value) -> Constant_Value { return !a.(bool); });

		// type_id
		add_equality_operators(type_type_id, TypeID);



		add_math_operators :: proc(type: ^Type, $Constant_Type: typeid) {
			assert(type != nil);
			type_add_binary_operator(type, .Plus,     type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) + b.(Constant_Type); });
			type_add_binary_operator(type, .Minus,    type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) - b.(Constant_Type); });
			type_add_binary_operator(type, .Multiply, type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) * b.(Constant_Type); });
			type_add_binary_operator(type, .Divide,   type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) / b.(Constant_Type); });
		}

		add_mod_and_mod_mod :: proc(type: ^Type, $Constant_Type: typeid) {
			assert(type != nil);
			type_add_binary_operator(type, .Mod,     type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) %  b.(Constant_Type); });
			type_add_binary_operator(type, .Mod_Mod, type, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) %% b.(Constant_Type); });
		}

		add_equality_operators :: proc(type: ^Type, $Constant_Type: typeid) {
			assert(type != nil);
			type_add_binary_operator(type, .Boolean_Equal,     type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) == b.(Constant_Type); });
			type_add_binary_operator(type, .Boolean_Not_Equal, type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) != b.(Constant_Type); });
		}

		add_less_and_greater_than_operators :: proc(type: ^Type, $Constant_Type: typeid) {
			assert(type != nil);
			type_add_binary_operator(type, .Boolean_Less_Than,             type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) <  b.(Constant_Type); });
			type_add_binary_operator(type, .Boolean_Greater_Than,          type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) >  b.(Constant_Type); });
			type_add_binary_operator(type, .Boolean_Less_Than_Or_Equal,    type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) <= b.(Constant_Type); });
			type_add_binary_operator(type, .Boolean_Greater_Than_Or_Equal, type_bool, proc(a, b: Constant_Value) -> Constant_Value { return a.(Constant_Type) >= b.(Constant_Type); });
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
			cv: Constant_Value;
			if kind.has_a_dot {
				t = type_untyped_float;
				cv = kind.float_value;
			}
			else {
				t = type_untyped_uint;
				cv = kind.uint_value;
			}
			complete_expr(node, Expr_Data{t, .Constant, cv});
			return .Ok;
		}

		case Ast_String: {
			complete_expr(node, Expr_Data{type_string, .Constant, kind.text});
			return .Ok;
		}

		case Ast_Unary: {
			rhs_data := kind.rhs.expr_data;
			switch kind.op {
				case .Dereference: {
					ptr, ok := rhs_data.type.kind.(Type_Ptr);
					if !ok {
						error(node, "Cannot dereference a non-pointer type ", type_to_string(rhs_data.type));
						return .Error;
					}
					complete_expr(node, Expr_Data{ptr.ptr_to, .LValue, {}});
					return .Ok;
				}
				case .Address: {
					complete_expr(node, Expr_Data{get_or_make_type_ptr_to(ws, rhs_data.type), .RValue, {}});
					return .Ok;
				}
				case: {
					info, ok := type_get_unary_operator(rhs_data.type, kind.op);
					if !ok {
						error(node, "Type ", type_to_string(rhs_data.type), " doesn't have unary operator ", kind.op);
						return .Error;
					}

					if info.constant_evaluation_procedure != nil && rhs_data.constant_value != nil {
						cv := info.constant_evaluation_procedure(rhs_data.constant_value);
						complete_expr(node, Expr_Data{info.result_type, .Constant, cv});
						return .Ok;
					}

					complete_expr(node, Expr_Data{info.result_type, .RValue, {}});
					return .Ok;
				}
			}
			unreachable();
			return .Error;
		}

		case Ast_Binary: {
			is_valid_rvalue :: proc(node: ^Ast_Node) -> bool {
				switch node.expr_data.mode {
					case .LValue: return true;
					case .RValue: return true;
					case .Constant: return true;
					case: return false;
				}
			}

			if !is_valid_rvalue(kind.lhs) {
				error(kind.lhs, "Not a valid rvalue.");
				println(kind.lhs);
				return .Error;
			}
			if !is_valid_rvalue(kind.rhs) {
				error(kind.rhs, "Not a valid rvalue.");
				println(kind.rhs);
				return .Error;
			}

			common_type: ^Type;
			{
				ltype := kind.lhs.expr_data.type;
				rtype := kind.rhs.expr_data.type;

				if !is_assignable_to(rtype, ltype) {
					type_mismatch(ltype, kind.rhs);
					return .Error;
				}

				if is_untyped_type(ltype) || is_untyped_type(rtype) {
					if is_untyped_type(ltype) && is_untyped_type(rtype) {
						lrank, ok1 := constant_precedence_table[ltype]; assert(ok1);
						rrank, ok2 := constant_precedence_table[rtype]; assert(ok2);
						if lrank > rrank {
							common_type = ltype;
							convert_if_untyped(kind.rhs, common_type);
						}
						else if rrank > lrank {
							common_type = rtype;
							convert_if_untyped(kind.lhs, common_type);
						}
						else {
							assert(ltype == rtype);
							common_type = ltype;
						}
					}
					else if is_untyped_type(ltype) {
						common_type = rtype;
						convert_if_untyped(kind.lhs, common_type);
					}
					else {
						assert(is_untyped_type(rtype));
						common_type = ltype;
						convert_if_untyped(kind.rhs, common_type);
					}
				}
				else {
					assert(ltype == rtype);
					common_type = ltype;
				}
			}
			assert(common_type != nil);

			operator_info, ok := type_get_binary_operator(common_type, kind.op);
			if !ok {
				error(kind.lhs, "Binary operator ", kind.op, " does not exist for type ", type_to_string(common_type));
				return .Error;
			}

			cv: Constant_Value;
			if kind.lhs.expr_data.mode == .Constant && kind.rhs.expr_data.mode == .Constant {
				assert(operator_info.constant_evaluation_procedure != nil);
				assert(kind.lhs.expr_data.constant_value != nil);
				assert(kind.rhs.expr_data.constant_value != nil);
				result := operator_info.constant_evaluation_procedure(kind.lhs.expr_data.constant_value, kind.rhs.expr_data.constant_value);
				assert(result != nil);
				cv = result;
			}
			else {
				if operator_info.constant_only {
					error(kind.lhs, "Binary operator ", kind.op, " for type ", type_to_string(common_type), " requires two constant values.");
				}
			}

			complete_expr(node, Expr_Data{operator_info.result_type, cv == nil ? .RValue : .Constant, cv});
			return .Ok;
		}

		case Ast_Selector: {
			assert(kind.lhs.expr_data.type != nil);
			left := kind.lhs.expr_data.type;
			left_struct, ok := left.kind.(Type_Struct);
			if !ok {
				error(node, "Selector only works for struct types right now.");
				return .Error;
			}
			for field, field_idx in left_struct.fields {
				if field == kind.field {
					complete_expr(node, Expr_Data{left_struct.types[field_idx], .LValue, {}});
					return .Ok;
				}
			}

			error(node, "Type ", type_to_string(left), " doesn't have a field '", kind.field, "'");
			return .Error;
		}

		case Ast_Subscript: {
			// todo(josh): Constant_Values for subscripts?

			assert(kind.lhs.expr_data.type != nil);
			assert(kind.index.expr_data.type != nil);
			if !is_assignable_to(kind.index.expr_data.type, type_int) {
				type_mismatch(type_int, kind.index);
				return .Error;
			}
			convert_if_untyped(kind.index, type_int);

			left := kind.lhs.expr_data.type;
			t := (&left.kind.(Type_Array)).array_of; // todo(josh): @ErrorHandling
			complete_expr(node, Expr_Data{t, .LValue, {}});
			return .Ok;
		}

		case Ast_Cast: {
			target_type := get_type(ws, kind.typespec.base.expr_data.constant_value.(TypeID));
			// todo: check if valid cast (can't cast string to int, for example)
			complete_expr(node, Expr_Data{target_type, .RValue, target_type.id});
			return .Ok;
		}

		case Ast_Identifier: {
			assert(kind.declaration != nil);
			// Symbols aren't AST nodes, so the Ast_Identifier can't depend() on it like normal. Oh well.
			if kind.declaration.kind == nil {
				return Check_Result.Not_Checked;
			}

			#complete
			switch decl in kind.declaration.kind {
				case Type_Decl: {
					assert(decl.type.id != 0);
					complete_expr(node, Expr_Data{type_type_id, .Constant, decl.type.id});
				}
				case Proc_Decl: {
					assert(decl.type != nil);
					complete_expr(node, Expr_Data{decl.type, .RValue, {}});
				}
				case Var_Decl: {
					assert(decl.type != nil);
					complete_expr(node, Expr_Data{decl.type, .LValue, decl.var.expr_data.constant_value});
				}
				case: {
					unhandledcase(decl);
				}
			}

			// there are the same thing right? why is this constant propagation happening?
			assert(&kind.expr_data.constant_value == &node.expr_data.constant_value);
			if kind.expr_data.constant_value != nil {
				node.expr_data.constant_value = kind.expr_data.constant_value;
			}
			return .Ok;
		}

		case Ast_Directive: {
			// todo(josh): delet this

			if kind.directive == "#odin" {
				complete_node(node);
				return .Ok;
			}
		}

		case Ast_Directive_Assert: {
			value, ok := kind.condition.expr_data.constant_value.(bool);
			if kind.condition.expr_data.type != type_bool || !ok {
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
				declared_type = get_type(ws, kind.typespec.base.expr_data.constant_value.(TypeID));
			}

			true_type: ^Type;
			if declared_type != nil {
				if kind.expr != nil {
					if !is_assignable_to(kind.expr.expr_data.type, declared_type) {
						type_mismatch(declared_type, kind.expr);
						return .Error;
					}
					convert_if_untyped(kind.expr, declared_type);
				}
				true_type = declared_type;
			}
			else if kind.expr != nil {
				if kind.expr.expr_data.mode == .No_Value {
					error(kind.expr, "Rhs of assignment doesn't have a value.");
					return .Error;
				}

				if is_untyped_type(kind.expr.expr_data.type) {
					solidify_untyped_type(kind.expr);
				}
				true_type = kind.expr.expr_data.type;
			}
			else {
				error(node, "Either a type or value is required for a variable declaration.");
				return .Error;
			}

			assert(true_type != nil);
			assert(!is_untyped_type(true_type));

			kind.type = true_type;

			if kind.is_constant && true_type == type_type_id {
				assert(kind.expr != nil);
				node.do_not_print = true; // don't print type aliases
				complete_declaration(kind.declaration, Type_Decl{get_type(ws, kind.expr.expr_data.constant_value.(TypeID))});
			}
			else {
				complete_declaration(kind.declaration, Var_Decl{true_type, kind});
			}

			if kind.is_constant {
				assert(kind.expr != nil);
				if kind.expr.expr_data.constant_value == nil {
					error(node, "Constants require a constant value expression.");
					return .Error;
				}
				node.expr_data.constant_value = kind.expr.expr_data.constant_value;
				kind.declaration.constant_value = node.expr_data.constant_value;
			}

			complete_node(node);

			if ws.current_scope == ws.global_scope {
				append(&ws.all_global_variables, kind);
			}

			return .Ok;
		}

		case Ast_Proc: {
			if kind.return_typespec != nil {
				kind.return_type = get_type(ws, kind.return_typespec.base.expr_data.constant_value.(TypeID));
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
			t := make_type_distinct(ws, kind.name, get_type(ws, kind.other.base.expr_data.constant_value.(TypeID)));
			complete_node(node);
			complete_declaration(kind.declaration, Type_Decl{t});
			kind.declaration.constant_value = t.id;
			return .Ok;
		}

		case Ast_Call: {
			target_type := kind.procedure.expr_data.type;
			proc_type, ok := target_type.kind.(Type_Proc);
			assert(ok);

			if len(kind.args) != len(proc_type.params) {
				error(node, "Expected ", len(proc_type.params), " parameters, got ", len(kind.args));
				return .Error;
			}

			for _, idx in kind.args {
				arg := kind.args[idx];
				param := proc_type.params[idx];
				if !is_assignable_to(arg.expr_data.type, param.inferred_type) {
					type_mismatch(param.inferred_type, arg);
					return .Error;
				}
				convert_if_untyped(arg, param.inferred_type);
			}

			mode: Addressing_Mode;
			if proc_type.return_type == nil {
				mode = .No_Value;
			}
			else {
				mode = .RValue;
			}
			assert(mode != .Invalid);

			complete_expr(node, Expr_Data{proc_type.return_type, mode, {}});
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
			assert(kind.nested_expr.expr_data.type != nil);
			node.expr_data.constant_value = kind.nested_expr.expr_data.constant_value;
			complete_expr(node, kind.nested_expr.expr_data);
			complete_node(node);
			return .Ok;
		}

		case Ast_If: {
			if !is_assignable_to(kind.condition.expr_data.type, type_bool) {
				type_mismatch(type_bool, kind.condition);
				return .Error;
			}
			convert_if_untyped(kind.condition, type_bool);
			complete_node(node);
			return .Ok;
		}

		case Ast_Else_If: {
			if !is_assignable_to(kind.condition.expr_data.type, type_bool) {
				type_mismatch(type_bool, kind.condition);
				return .Error;
			}
			convert_if_untyped(kind.condition, type_bool);
			complete_node(node);
			return .Ok;
		}

		case Ast_While: {
			if !is_assignable_to(kind.condition.expr_data.type, type_bool) {
				type_mismatch(type_bool, kind.condition);
				return .Error;
			}
			convert_if_untyped(kind.condition, type_bool);
			complete_node(node);
			return .Ok;
		}

		case Ast_Assign: {
			if kind.lhs.expr_data.mode != .LValue {
				error(kind.lhs, "Cannot assign to non lvalue.");
				return .Error;
			}

			if kind.rhs.expr_data.mode == .No_Value {
				error(kind.lhs, "Rhs of assignment doesn't have a value.");
				return .Error;
			}
			ltype := kind.lhs.expr_data.type;
			rtype := kind.rhs.expr_data.type;
			assert(ltype != nil);
			assert(rtype != nil);

			if !is_assignable_to(rtype, ltype) {
				type_mismatch(ltype, kind.rhs);
				return .Error;
			}
			convert_if_untyped(kind.rhs, ltype);
			complete_node(node);
			return .Ok;
		}

		case Ast_Return: {
			assert(kind.procedure.signature_type != nil);
			assert(kind.expr != nil);
			return_type := kind.procedure.signature_type.kind.(Type_Proc).return_type;
			expr_type := kind.expr.expr_data.type;
			if !is_assignable_to(expr_type, return_type) {
				type_mismatch(return_type, kind.expr);
				return .Error;
			}
			convert_if_untyped(kind.expr, return_type);
			complete_node(node);
			return .Ok;
		}

		case Ast_Type_Expression: {
			unimplemented();
			// todo(josh): expr shouldn't depend on typespec. the whole point of #type is that we don't _know_ that the following expression is necessarily a type
			// node.constant_value = kind.typespec.base.constant_value.(TypeID);
			// complete_expr(node, type_type_id);
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
					list_of := get_type(ws, typespec_kind.typespec.base.expr_data.constant_value.(TypeID));
					t := get_or_make_type_list_of(ws, list_of);
					complete_typespec(kind, t);
					return .Ok;
				}

				case Typespec_Array: {
					if typespec_kind.length_expr.expr_data.constant_value == nil {
						error(typespec_kind.length_expr, "Array sizes require a constant integer value.");
						return .Error;
					}
					array_length, ok := typespec_kind.length_expr.expr_data.constant_value.(i64);
					if !ok {
						error(typespec_kind.length_expr, "Array sizes require a constant integer value.");
						return .Error;
					}
					array_of := get_type(ws, typespec_kind.typespec.base.expr_data.constant_value.(TypeID));
					t := get_or_make_type_array_of(ws, cast(u64)array_length, array_of);
					complete_typespec(kind, t);
					return .Ok;
				}

				case Typespec_Slice: {
					slice_of := get_type(ws, typespec_kind.typespec.base.expr_data.constant_value.(TypeID));
					t := get_or_make_type_slice_of(ws, slice_of);
					complete_typespec(kind, t);
					return .Ok;
				}

				case Typespec_Ptr: {
					ptr_to := get_type(ws, typespec_kind.typespec.base.expr_data.constant_value.(TypeID));
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

convert_if_untyped :: proc(node: ^Ast_Node, target_type: ^Type) {
	if !is_untyped_type(node.expr_data.type) {
		return;
	}

	switch kind in target_type.kind {
		case Type_Untyped: {
			//
			convert_if_untyped(node, kind.base);
			return;
		}
	}

	assert(node.expr_data.mode == .Constant);

	switch kind in node.expr_data.constant_value {
		case f64: {
			switch target_kind in target_type.kind {
				case Type_Float: {
					// do nothing :D
				}
				case: panic(tprint(target_kind));
			}
		}
		case i64: {
			switch target_kind in target_type.kind {
				case Type_Float:   node.expr_data.constant_value = cast(f64)kind;
				case Type_Integer: assert(target_kind.signed == true, tprint("Should never lower a signed int to an unsigned int"));
				case: panic(tprint(target_kind));
			}
		}
		case u64: {
			switch target_kind in target_type.kind {
				case Type_Float:   node.expr_data.constant_value = cast(f64)kind;
				case Type_Integer: if target_kind.signed do node.expr_data.constant_value = cast(i64)kind;
				case: panic(tprint(target_kind));
			}
		}
		case: panic(tprint(kind));
	}
	node.expr_data.type = target_type;
}

solidify_untyped_type :: proc(node: ^Ast_Node) {
	switch kind in node.expr_data.type.kind {
		case Type_Untyped: {
			switch untyped_kind in kind.base.kind {
				case Type_Integer: {
					node.expr_data.type = type_int;
				}
				case Type_Float: {
					node.expr_data.type = type_int;
				}
				case: panic(tprint(untyped_kind));
			}
		}
		case: panic(tprint("Expected untyped type, got:", node.expr_data.type));
	}
}



type_mismatch :: proc(wanted: ^Type, given: ^Ast_Node, loc := #caller_location) {
	error(given, "Type mismatch: wanted ", type_to_string(wanted), " given ", type_to_string(given.expr_data.type));
}

is_assignable_to :: inline proc(rhs: ^Type, lhs: ^Type, loc := #caller_location) -> bool {
	//
	if lhs == nil && rhs != nil do return false;
	if lhs != nil && rhs == nil do return false;

	//
	if lhs == rhs do return true;

	if is_untyped_type(lhs) || is_untyped_type(rhs) {
		if is_integer_type(rhs) && is_integer_type(lhs) {
			return true;
		}
		if is_float_type(rhs)   && is_float_type(lhs) {
			return true;
		}

		// any untyped number can be assigned to floats
		if is_numeric_type(rhs) && is_float_type(lhs) {
			return true;
		}
	}

	return false;
}



is_numeric_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	if t.flags & cast(u32)Type_Flags.Number > 0 {
		return true;
	}
	return false;
}

is_integer_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	if t.flags & cast(u32)Type_Flags.Integer > 0 {
		return true;
	}
	return false;
}

is_float_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	if t.flags & cast(u32)Type_Flags.Float > 0 {
		return true;
	}
	return false;
}

is_signed_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	if t.flags & cast(u32)Type_Flags.Signed > 0 {
		return true;
	}
	return false;
}

is_unsigned_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	if t.flags & cast(u32)Type_Flags.Unsigned > 0 {
		return true;
	}
	return false;
}

is_untyped_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	if t.flags & cast(u32)Type_Flags.Untyped > 0 {
		return true;
	}
	return false;
}

is_pointer_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	if t.flags & cast(u32)Type_Flags.Pointer > 0 {
		return true;
	}
	return false;
}

is_array_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	switch kind in t.kind {
		case Type_Array: return true;
	}
	return false;
}
is_slice_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	switch kind in t.kind {
		case Type_Slice: return true;
	}
	return false;
}
is_list_type :: proc(t: ^Type, loc := #caller_location) -> bool {
	assert(t != nil, tprint(loc));
	switch kind in t.kind {
		case Type_List: return true;
	}
	return false;
}



// note(josh): need some kind of system for this, 8 is just what the VM's alignment is so we'll use that for now
TARGET_PLATFORM_ALIGNMENT :: 8;

make_type :: proc(ws: ^Workspace, name: string, size: u64, derived: $T, flags: Type_Flags, loc := #caller_location) -> ^Type {
	new_type := new(Type);
	new_type.id = cast(TypeID)len(ws.all_types)+1;
	new_type.name = name;
	new_type.size = size;
	new_type.kind = derived;
	new_type.flags = cast(u32)flags;
	append(&ws.all_types, new_type);
	return new_type;
}

align_forward :: proc(val: u64, align: u64) -> u64 {
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
	size : u64 = 0;
	// lotsa allocations here that probably dont need to be here
	names: [dynamic]string;
	offsets: [dynamic]u64;
	types: [dynamic]^Type;
	for var in fields {
		assert(var.inferred_type != nil);
		append(&names, var.name);
		append(&offsets, cast(u64)size);
		append(&types, var.inferred_type);

		size += var.inferred_type.size;
	}

	assert(size != 0);
	new_type := make_type(ws, name, size, Type_Struct{names[:], types[:], offsets[:]}, {});
	return new_type;
}

type_add_binary_operator :: proc(type: ^Type, operator: Operator, result_type: ^Type, constant_eval_proc: proc(Constant_Value, Constant_Value) -> Constant_Value, constant_only := false, loc := #caller_location) {
	assert(type != nil, tprint(loc));
	assert(result_type != nil, tprint(loc));
	type.binary_operators[operator] = Binary_Operator_Info{result_type, constant_eval_proc, constant_only};
}
type_add_unary_operator :: proc(type: ^Type, operator: Operator, result_type: ^Type, constant_eval_proc: proc(Constant_Value) -> Constant_Value, constant_only := false, loc := #caller_location) {
	assert(type != nil, tprint(loc));
	assert(result_type != nil, tprint(loc));
	type.unary_operators[operator] = Unary_Operator_Info{result_type, constant_eval_proc};
}
type_get_binary_operator :: proc(type: ^Type, operator: Operator) -> (Binary_Operator_Info, bool) {
	info, ok := type.binary_operators[operator];
	return info, ok;
}
type_get_unary_operator :: proc(type: ^Type, operator: Operator) -> (Unary_Operator_Info, bool) {
	info, ok := type.unary_operators[operator];
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
				this_param_type  := declaration.params[idx].base.expr_data.type;
				other_param_type := other_type.params[idx].inferred_type;
				if other_param_type != this_param_type {
					continue type_loop;
				}
			}

			return canonical_type;
		}
	}

	name: strings.Builder;
	sbprint(&name, "proc(");
	params: [dynamic]Field;
	for _, idx in declaration.params {
		param := declaration.params[idx];
		t := param.type;
		sbprint(&name, t.name);
		append(&params, Field{param.name, t});
	}
	sbprint(&name, ")");
	if declaration.return_type != nil {
		sbprint(&name, " ", declaration.return_type.name);;
	}

	new_type := make_type(ws, strings.to_string(name), POINTER_SIZE, Type_Proc{params[:], declaration.return_type}, Type_Flags.Procedure);
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

	name: strings.Builder;
	sbprint(&name, "^", ptr_to.name);
	type_ptr := make_type(ws, strings.to_string(name), POINTER_SIZE, Type_Ptr{ptr_to}, Type_Flags.Pointer);
	return type_ptr;
}

get_or_make_type_array_of :: proc(using ws: ^Workspace, length: u64, array_of: ^Type) -> ^Type {
	assert(array_of.size != 0);
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

	name: strings.Builder;
	sbprint(&name, "[", length, "]", array_of.name);
	array_type := make_type(ws, strings.to_string(name), array_of.size * length, Type_Array{length, array_of}, {});
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

	name: strings.Builder;
	sbprint(&name, "[:]", list_of.name);
	array_type := make_type(ws, strings.to_string(name), POINTER_SIZE + INT_SIZE, Type_List{list_of}, {}); // ptr + length
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

	name: strings.Builder;
	sbprint(&name, "[]", slice_of.name);
	type_slice := make_type(ws, strings.to_string(name), SLICE_SIZE, Type_Slice{slice_of}, {});
	return type_slice;
}



complete_typespec :: inline proc(typespec: ^Ast_Typespec, t: ^Type, loc := #caller_location) {
	assert(typespec != nil);
	assert(t != nil);
	typespec.completed_type = t;
	complete_expr(typespec.base, Expr_Data{type_type_id, .Constant, t.id});
}

complete_expr :: inline proc(node: ^Ast_Node, expr_data: Expr_Data, loc := #caller_location) {
	assert(node != nil, tprint("node was nil at ", loc));
	node.expr_data = expr_data;
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
	return canonical_type.name;
}

unhandledcase :: proc(value: $T, loc := #caller_location) -> ! {
	panic(tprint("Unhandled case at ", pretty_location(loc), ": ", value));
}
