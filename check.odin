package crest

using import "core:fmt"
	  import "core:os"
	  import "core:strings"

using import "shared:workbench/reflection"
using import "shared:workbench/logging"

POINTER_SIZE :: 8;
INT_SIZE :: 8;
DYNAMIC_ARRAY_SIZE :: POINTER_SIZE + INT_SIZE + INT_SIZE;
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

type_untyped_int:   ^Type = new_clone(Type{Type_Struct{"untyped_int",   nil}, 0});
type_untyped_float: ^Type = new_clone(Type{Type_Struct{"untyped_float", nil}, 0});

init_builtin_types :: proc(ws: ^Workspace) {
	if type_i8 == nil {
		type_i8  = make_type(ws, 1, Type_Primitive{"i8"});
		type_i16 = make_type(ws, 2, Type_Primitive{"i16"});
		type_i32 = make_type(ws, 4, Type_Primitive{"i32"});
		type_i64 = make_type(ws, 8, Type_Primitive{"i64"});
		type_int = type_i32;

		type_u8  = make_type(ws, 1, Type_Primitive{"u8"});
		type_u16 = make_type(ws, 2, Type_Primitive{"u16"});
		type_u32 = make_type(ws, 4, Type_Primitive{"u32"});
		type_u64 = make_type(ws, 8, Type_Primitive{"u64"});
		type_int = type_u32;

		type_f32 = make_type(ws, 4, Type_Primitive{"f32"});
		type_f64 = make_type(ws, 8, Type_Primitive{"f64"});
		type_float = type_f32;

		type_bool = make_type(ws, 1, Type_Primitive{"bool"});

		fields := [?]Field{
			{"data",   get_or_make_type_ptr_to(ws, type_u8)},
			{"length", get_or_make_type_ptr_to(ws, type_int)},
		};
		type_string = make_type_struct(ws, "string", fields[:]);
	}

	create_symbol(ws.global_scope, "i8",    type_i8);
	create_symbol(ws.global_scope, "i16",   type_i16);
	create_symbol(ws.global_scope, "i32",   type_i32);
	create_symbol(ws.global_scope, "i64",   type_i64);
	create_symbol(ws.global_scope, "int",   type_int);

	create_symbol(ws.global_scope, "u8",    type_u8);
	create_symbol(ws.global_scope, "u16",   type_u16);
	create_symbol(ws.global_scope, "u32",   type_u32);
	create_symbol(ws.global_scope, "u64",   type_u64);
	create_symbol(ws.global_scope, "uint",  type_uint);

	create_symbol(ws.global_scope, "f32",   type_f32);
	create_symbol(ws.global_scope, "f64",   type_f64);
	create_symbol(ws.global_scope, "float", type_float);

	create_symbol(ws.global_scope, "bool",   type_bool);

	create_symbol(ws.global_scope, "string", type_string);
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

			assert(node.inferred_type == nil);
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

					sbprint(&sb, "\"", get_union_type_info(node.derived),   " ", node.check_state,   " ", site(node.site), "\"");
					sbprint(&sb, " -> ", );
					sbprint(&sb, "\"", get_union_type_info(depend.derived), " ", depend.check_state, " ", site(depend.site), "\"", "\n");
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
	switch kind in &node.derived {
		case Ast_Number: {
			t: ^Type;
			if kind.is_float {
				t = type_untyped_float;
			}
			else {
				t = type_untyped_int;
			}

			complete_node(node, t);
			return Check_Result.Ok;
		}

		case Ast_Unary: {
			using Token_Type;

			assert(kind.rhs.inferred_type != nil);
			t := kind.rhs.inferred_type;
			switch kind.op.kind {
				case Not: {
					assert(kind.rhs.inferred_type == type_bool);
					t = type_bool;
				}
				case Plus, Minus: {
					// all good
				}
				case Xor: { // deref
					ptr, ok := t.kind.(Type_Ptr);
					if !ok {
						error(node, "Cannot dereference a non-pointer type, given ", type_to_string(t));
						return Check_Result.Error;
					}

					t = ptr.ptr_to;
				}
				case And: { // take address
					t = get_or_make_type_ptr_to(ws, t);
				}
				case: {
					unhandledcase(kind.op.kind);
				}
			}

			complete_node(node, t);
			return Check_Result.Ok;
		}

		case Ast_Binary: {
			using Token_Type;

			if !ensure_is_assignable_to(kind.lhs.inferred_type, kind.rhs) do return Check_Result.Error;

			t: ^Type;
			switch kind.op.kind {
				case NUMBER_OPS_BEGIN..NUMBER_OPS_END: {
					t = kind.lhs.inferred_type;
				}
				case BOOL_OPS_BEGIN..BOOL_OPS_END: {
					t = type_bool;
				}
				case: {
					unhandledcase(kind.op.kind);
				}
			}
			complete_node(node, t);
			return Check_Result.Ok;
		}

		case Ast_Selector: {
			assert(kind.left.inferred_type != nil);
			left := kind.left.inferred_type;
			left_struct := left.kind.(Type_Struct);
			for field in left_struct.fields {
				if field.name == kind.field {
					complete_node(node, field.inferred_type);
					return Check_Result.Ok;
				}
			}

			error(node, "Type ", type_to_string(left), " doesn't have a field '", kind.field, "'");
			return Check_Result.Error;
		}

		case Ast_Subscript: {
			assert(kind.left.inferred_type != nil);
			left := kind.left.inferred_type;

			if !ensure_is_assignable_to(type_int, kind.index) do return Check_Result.Error;
			t := (&left.kind.(Type_Array)).array_of;
			complete_node(node, t);
			return Check_Result.Ok;
		}

		case Ast_Cast: {
			assert(kind.typespec.inferred_type != nil);
			target := kind.typespec.inferred_type;
			// todo: check if valid cast (can't cast string to Vector2, for example)
			complete_node(node, target);
			return Check_Result.Ok;
		}

		case Ast_String: {
			complete_node(node, type_string);
			return Check_Result.Ok;
		}

		case Ast_Identifier: {
			// Symbols aren't AST nodes, so the Ast_Identifier can't depend() on it like normal. Oh well.
			sym := kind.sym;
			assert(sym != nil);
			if sym.inferred_type == nil {
				return Check_Result.Not_Checked;
			}

			complete_node(node, sym.inferred_type);
			return Check_Result.Ok;
		}

		case Ast_Directive: {
			if kind.directive == "#odin" {
				node.check_state = Check_State.Checked;
			return Check_Result.Ok;
			}
		}

		case Ast_Block: {
			node.check_state = Check_State.Checked;
			return Check_Result.Ok;
		}

		case Ast_Var: {
			declared_type: ^Type;
			if kind.typespec != nil {
				assert(kind.typespec.inferred_type != nil);
				declared_type = kind.typespec.inferred_type;
			}
			expr_type: ^Type;
			if kind.expr != nil {
				assert(kind.expr.inferred_type != nil);
				expr_type = kind.expr.inferred_type;
			}

			true_type: ^Type;
			if declared_type != nil && expr_type != nil {
				if !ensure_is_assignable_to(declared_type, kind.expr) do return Check_Result.Error;
				true_type = declared_type;
			}

			if declared_type != nil {
				true_type = declared_type;
			}
			else if expr_type != nil {
				true_type = expr_type;
			}
			else {
				assert(false, "no declared type or expr to infer type from");
			}

			if true_type == type_untyped_int   do true_type = type_int;
			if true_type == type_untyped_float do true_type = type_float;

			assert(true_type != nil);
			complete_node(node, true_type);
			complete_sym(kind.sym, true_type);
			return Check_Result.Ok;
		}

		case Ast_Proc: {
			if kind.return_typespec != nil {
				assert(kind.return_typespec.inferred_type != nil);
				kind.return_type = kind.return_typespec.inferred_type;
			}

			t := get_or_make_type_proc(ws, kind);
			complete_node(node, t);
			complete_sym(kind.sym, t);
			return Check_Result.Ok;
		}

		case Ast_Struct: {
			fields: [dynamic]Field;
			for field in kind.fields {
				append(&fields, Field{field.name, field.inferred_type});
			}
			t := make_type_struct(ws, kind.name, fields[:]);
			complete_node(node, t);
			complete_sym(kind.sym, t);
			return Check_Result.Ok;
		}

		case Ast_Call: {
			target_type := kind.procedure.inferred_type;
			proc_type, ok := target_type.kind.(Type_Proc);
			assert(ok);

			if len(kind.params) != len(proc_type.params) {
				error(node, "Expected ", len(proc_type.params), " parameters, got ", len(kind.params));
			}

			for _, idx in kind.params {
				param := kind.params[idx];
				ensure_is_assignable_to(proc_type.params[idx].inferred_type, param);
			}

			complete_node(node, proc_type.return_type, true);
			return Check_Result.Ok;
		}

		// case Ast_Switch: {
		// 	node.check_state = Checked;
		// 	return Check_Result.Ok;
		// }

		case Ast_Range: {
			min_type: ^Type;
			max_type: ^Type;

			if kind.min == nil {
				min_type = type_int;
			}
			else {
				assert(kind.min.inferred_type != nil);
				min_type = kind.min.inferred_type;
				ensure_is_assignable_to(type_int, kind.min);
			}

			if kind.max == nil {
				max_type = type_int;
			}
			else {
				assert(kind.max.inferred_type != nil);
				max_type = kind.max.inferred_type;
				ensure_is_assignable_to(type_int, kind.max);
			}

			t: ^Type;
			if kind.lhs != nil {
				assert(kind.lhs.inferred_type != nil);
				t = kind.lhs.inferred_type;
			}
			else {
				t = type_int;
			}
			complete_node(node, t);
			return Check_Result.Ok;
		}

		case Ast_Slice: {
			assert(kind.array.inferred_type != nil);
			array_type := kind.array.inferred_type;
			array_of: ^Type;
			switch array_kind in array_type.kind {
				case Type_Array: {
					array_of = array_kind.array_of;
				}
				case Type_Dynamic_Array: {
					array_of = array_kind.array_of;
				}
				case Type_Slice: {
					array_of = array_kind.slice_of;
				}
				case: {
					error(kind.array, "Cannot slice type: ", type_to_string(array_type));
					assert(false);
				}
			}
			slice_type := get_or_make_type_slice_of(ws, array_of);
			complete_node(node, slice_type);
			return Check_Result.Ok;
		}

		case Ast_If: {
			ensure_is_assignable_to(type_bool, kind.condition);
			node.check_state = Check_State.Checked;
			return Check_Result.Ok;
		}

		case Ast_Else_If: {
			ensure_is_assignable_to(type_bool, kind.condition);
			node.check_state = Check_State.Checked;
			return Check_Result.Ok;
		}

		case Ast_While: {
			ensure_is_assignable_to(type_bool, kind.condition);
			node.check_state = Check_State.Checked;
			return Check_Result.Ok;
		}

		case Ast_For_Each: {
			assert(kind.array.inferred_type != nil);
			t := get_base_type(kind.array.inferred_type);
			complete_node(kind.var, t);
			return Check_Result.Ok;
		}

		case Ast_Assign: {
			assert(kind.left.inferred_type != nil);
			left := kind.left.inferred_type;
			ensure_is_assignable_to(left, kind.right);
			node.check_state = Check_State.Checked;
			return Check_Result.Ok;
		}

		case Ast_Return: {
			assert(kind.procedure.inferred_type != nil);
			ensure_is_assignable_to(kind.procedure.inferred_type.kind.(Type_Proc).return_type, kind.expr);
			node.check_state = Check_State.Checked;
			return Check_Result.Ok;
		}

		case Ast_Typespec_Dynamic_Array: {
			assert(kind.typespec.inferred_type != nil);
			array_of := kind.typespec.inferred_type;
			t := get_or_make_type_dynamic_array_of(ws, array_of);
			complete_node(node, t);
			return Check_Result.Ok;
		}

		case Ast_Typespec_Array: {
			assert(kind.typespec.inferred_type != nil);
			array_of := kind.typespec.inferred_type;
			// todo(josh): support more than just number literals for array lengths, need a good constants system
			length, ok := kind.size_expr.derived.(Ast_Number);
			if !ok {
				error(kind.size_expr, "We only handle number literals for array lengths right now.");
				return Check_Result.Error;
			}
			t := get_or_make_type_array_of(ws, cast(uint)length.int_number, array_of);
			complete_node(node, t);
			return Check_Result.Ok;
		}

		case Ast_Typespec_Slice: {
			assert(kind.typespec.inferred_type != nil);
			slice_of := kind.typespec.inferred_type;
			t := get_or_make_type_slice_of(ws, slice_of);
			complete_node(node, t);
			return Check_Result.Ok;
		}

		case Ast_Typespec_Ptr: {
			assert(kind.typespec.inferred_type != nil);
			ptr_to := kind.typespec.inferred_type;
			t := get_or_make_type_ptr_to(ws, ptr_to);
			complete_node(node, t);
			return Check_Result.Ok;
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
		// 	complete_node(node, union_type);
		// 	return Check_Result.Ok;
		// }

		case Ast_Comment: {
			node.check_state = Check_State.Checked;
			return Check_Result.Ok;
		}

		case Ast_Directive_Include: {
			node.check_state = Check_State.Checked;
			return Check_Result.Ok;
		}

		case: {
			unhandledcase(kind^);
		}
	}

	unreachable();
	return Check_Result.Error;
}



ensure_is_assignable_to :: proc(wanted: ^Type, given: ^Ast_Node, loc := #caller_location) -> bool {
	if !is_assignable_to(wanted, given.inferred_type, loc) {
		error(given, "Type mismatch: wanted ", type_to_string(wanted), " given ", type_to_string(given.inferred_type));
		return false;
	}
	return true;
}

is_assignable_to :: inline proc(wanted: ^Type, given: ^Type, loc := #caller_location) -> bool {
	//
	if wanted == nil && given != nil do return false;
	if wanted != nil && given == nil do return false;

	//
	if wanted == given do return true;

	//
	if wanted == type_int   && given == type_untyped_int   do return true;
	if wanted == type_float && given == type_untyped_int   do return true;
	if wanted == type_float && given == type_untyped_float do return true;

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



make_type ::  proc(ws: ^Workspace, size: uint, derived: $T, loc := #caller_location) -> ^Type {
	new_type := new(Type);
	new_type.size = size;
	new_type.kind = derived;
	append(&ws.all_types, new_type);
	return new_type;
}

make_type_struct :: proc(ws: ^Workspace, name: string, fields: []Field) -> ^Type {
	size : uint = 0;
	for var in fields {
		assert(var.inferred_type != nil);
		size += var.inferred_type.size;
	}

	assert(size != 0);
	new_type := make_type(ws, size, Type_Struct{name, fields});
	return new_type;
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
				this_param_type  := declaration.params[idx].base.inferred_type;
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
		t := (param).inferred_type;
		append(&params, Field{param.name, t});
	}

	new_type := make_type(ws, POINTER_SIZE, Type_Proc{params[:], declaration.return_type});
	return new_type;
}

get_base_type :: proc(type: ^Type) -> ^Type {
	switch kind in type.kind {
		case Type_Dynamic_Array: {
			return kind.array_of;
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

	type_ptr := make_type(ws, POINTER_SIZE, Type_Ptr{ptr_to});
	return type_ptr;
}

get_or_make_type_dynamic_array_of :: proc(using ws: ^Workspace, array_of: ^Type) -> ^Type {
	if all_types != nil {
		for other_type in all_types {
			if other_array, ok := other_type.kind.(Type_Dynamic_Array); ok {
				if array_of == other_array.array_of {
					return other_type;
				}
			}
		}
	}

	array_type := make_type(ws, DYNAMIC_ARRAY_SIZE, Type_Dynamic_Array{array_of});
	return array_type;
}

get_or_make_type_array_of :: proc(using ws: ^Workspace, length: uint, array_of: ^Type) -> ^Type {
	assert(array_of.size != 0);
	length *= array_of.size;
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

	array_type := make_type(ws, length, Type_Array{length, array_of});
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

	type_slice := make_type(ws, SLICE_SIZE, Type_Slice{slice_of});
	return type_slice;
}



complete_node :: inline proc(node: ^Ast_Node, t: ^Type, nil_type_is_ok := false, loc := #caller_location) {
	if !nil_type_is_ok {
		assert(node != nil, tprint("node was nil at ", loc));
		assert(t != nil, tprint("t was nil at ", loc));
	}

	node.inferred_type = t;
	node.check_state = Check_State.Checked;
}

complete_sym :: inline proc(sym: ^Symbol, t: ^Type) {
	sym.inferred_type = t;
}



error :: inline proc(base: ^Ast_Node, args: ..any) {
	print(site(base.site), " ");
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
		case Type_Dynamic_Array: {
			return aprint("[:]", type_to_string(kind.array_of));
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

	assert(false);
	return "";
}

unhandledcase :: proc(value: $T, loc := #caller_location) -> ! {
	panic(tprint("Unhandled case: ", value, " at ", pretty_location(loc)));
}