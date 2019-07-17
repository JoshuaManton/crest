package crest

      import "core:strings"

using import "core:fmt"
using import "core:mem"
using import "shared:workbench/logging"
using import "../storm"

VM_ALIGNMENT :: 8;

generate_and_execute_workspace :: proc(ws: ^Workspace) {
	// emit main
	main_decl := try_find_declaration_in_block(ws.global_scope, "main");
	assert(main_decl != nil);
	main_proc := main_decl.kind.(Proc_Decl).procedure;
	emit_procedure(ws, main_proc, true);
	output("quit");

	for procedure in ws.all_procedures {
		if procedure == main_proc do continue;
		emit_procedure(ws, procedure);
	}

	println(strings.to_string(vm_code));

	parse_and_execute(strings.to_string(vm_code));

	// foo_decl := try_find_declaration_in_block(ws.global_scope, "foo");
	// assert(foo_decl != nil);
	// foo_proc := foo_decl.kind.(Proc_Decl).procedure;

	// vec_decl := try_find_declaration_in_block(main_proc.block, "vec");
	// assert(vec_decl != nil);
	// println("vec.x is:", ws.vm.register_memory[vec_decl.kind.(Var_Decl).var.register.start]);
	// println("vec.y is:", ws.vm.register_memory[vec_decl.kind.(Var_Decl).var.register.start+1]);
}

vm_code: strings.Builder;

output :: proc(pieces: ..any) {
	sbprintln(&vm_code, ..pieces);
}

emit_procedure :: proc(ws: ^Workspace, procedure: ^Ast_Proc, is_main := false) {
	// frame_pointer := procedure.block.current_register;
	// defer procedure.block.current_register = frame_pointer;
	output(":", procedure.name);

	if !is_main {
		procedure.return_address_reg = alloc_register(procedure, type_u64);
		output("# return address");
		output("pop", procedure.return_address_reg.reg, procedure.return_address_reg.type.size);
	}

	if len(procedure.params) > 0 {
		for param in procedure.params {
			param.offset_in_stack_frame = procedure.stack_frame_size;
			procedure.stack_frame_size += param.type.size;
		}

		output("movuim rim", procedure.stack_frame_size);
		output("subu rsp rsp rim");
	}

	procedure.stack_pointer_reg = alloc_register(procedure, type_u64);
	output("# frame pointer");
	output("mov", procedure.stack_pointer_reg.reg, "rsp");

	for var in procedure.variables {
		var.offset_in_stack_frame = procedure.stack_frame_size;
		procedure.stack_frame_size += var.type.size;
	}

	output("# stack frame size");
	output("adduim rsp rsp", procedure.stack_frame_size);
	emit_block(ws, procedure.block, procedure);
	output("# restore frame pointer");
	output("mov rsp", procedure.stack_pointer_reg.reg);
	if !is_main {
		return_reg := alloc_register(procedure, type_u64);
		output("# jump to return address");
		output("adduim", return_reg.reg, procedure.return_address_reg.reg, 1);
		output("mov rip", return_reg.reg);
	}
}

emit_block :: proc(ws: ^Workspace, block: ^Ast_Block, procedure: ^Ast_Proc) {
	for stmt in block.stmts {
		if c, ok := stmt.derived.(Ast_Comment); ok {
			output("#", c.text);
			continue;
		}

		output("#", stmt.root_token.site);
		switch kind in &stmt.derived {
			case Ast_Var: {
				if kind.expr != nil {
					result := emit_expr(kind.expr, procedure);
					mov_reg_to_var(result, kind, procedure);
					free_register(procedure, result);
				}
			}
			case Ast_If: {
				result := emit_expr(kind.condition, procedure);
				jump_label := aprint("if_", stmt.serial);
				defer delete(jump_label);
				output("jeq", jump_label, result.reg, "rz");
				free_register(procedure, result);
				emit_block(ws, kind.block, procedure);
				output(":", jump_label);
			}
			case Ast_Return: {
				unimplemented();
				assert(procedure != nil);
				// emit_return(ws, procedure, kind.expr);
			}
			case Ast_Call: {
				emit_call(ws, kind, procedure);
			}
			case Ast_Assign: {
				emit_assign(ws, kind, procedure);
			}
			case Ast_Comment: {
				// nothing
			}
			case: assert(false, tprint(kind));
		}
	}
}

emit_call :: proc(ws: ^Workspace, ast_call: ^Ast_Call, procedure: ^Ast_Proc) {
	output("# save registers");
	for reg in procedure.registers_in_use {
		output("push", reg.reg, reg.type.size);
	}

	if len(ast_call.args) > 0 {
		output("# push parameters");
		for arg in ast_call.args {
			assert(arg.expr_type != nil);
			reg := emit_expr(arg, procedure);
			output("push", reg.reg, reg.type.size);
			free_register(procedure, reg);
		}
	}

	// do the call
	proc_name := ast_call.procedure.derived.(Ast_Identifier).declaration.kind.(Proc_Decl).procedure.name;
	output("# push return address");
	output("push rip", 8);
	output("# call", proc_name);
	output("goto", proc_name);

	// pop return values


	output("# pop saved registers");
	for i := len(procedure.registers_in_use)-1; i >= 0; i -= 1 {
		reg := procedure.registers_in_use[i];
		output("pop", reg.reg, reg.type.size);
	}
}

emit_assign :: proc(ws: ^Workspace, assign: ^Ast_Assign, procedure: ^Ast_Proc) {
	switch left_kind in &assign.left.derived {
		case Ast_Var: {
			panic("");
			result := emit_expr(left_kind.expr, procedure);
			mov_reg_to_var(result, left_kind, procedure);
			free_register(procedure, result);
		}
		case: panic(tprint(left_kind));
	}
	// storage := find_storage(assign.left);

	// switch assign.op {
	// 	case .Assign: {
	// 		emit_expr(assign.right, storage);
	// 	}
	// 	case .Plus_Assign: {
	// 		expr_reg := alloc_register(assign.parent_procedure, assign.right.expr_type);
	// 		assert(storage.size == 1); // todo(josh): array programming?
	// 		assert(expr_reg.size == storage.size);
	// 		emit_expr(assign.right, expr_reg);
	// 		add(&ws.vm, storage.start, storage.start, expr_reg.start);
	// 	}
	// 	case: assert(false, tprint(assign.op));
	// }
}

// find_storage :: proc(node: ^Ast_Node) -> Register_Allocation {
// 	switch kind in node.derived {
// 		case Ast_Identifier: {
// 			switch decl_kind in kind.declaration.kind {
// 				case Var_Decl: {
// 					assert(decl_kind.var.register.size > 0);
// 					return decl_kind.var.register;
// 				}
// 				case: assert(false, tprint(decl_kind));
// 			}
// 		}
// 		case Ast_Selector: {
// 			var_storage := find_storage(kind.left);
// 			ss := &kind.left.expr_type.kind.(Type_Struct);
// 			idx := get_field_idx(ss, kind.field);
// 			storage := Register_Allocation{var_storage.start+(ss.offsets[idx] / TARGET_PLATFORM_ALIGNMENT), ss.types[idx].register_size};
// 			return storage;
// 		}
// 		case: assert(false, tprint(kind));
// 	}
// 	unreachable();
// 	return {};
// }

get_field_idx :: proc(type: ^Type_Struct, name: string) -> int {
	for field, idx in type.fields {
		if field == name {
			return idx;
		}
	}
	unreachable();
	return {};
}

emit_expr :: proc(expr: ^Ast_Node, procedure: ^Ast_Proc) -> Register_Allocation {
	if expr.constant_value != nil {
		reg: Register_Allocation;
		switch kind in expr.constant_value {
			case i64: {
				reg = alloc_register(procedure, type_i64);
				output("movsim", reg.reg, kind);
			}
			case u64: {
				reg = alloc_register(procedure, type_u64);
				output("movuim", reg.reg, kind);
			}
			case f64: {
				reg = alloc_register(procedure, type_f64);
				output("movfim", reg.reg, transmute(u64)kind);
			}
			case: panic(tprint(kind));
		}
		return reg;
	}

	assert(!is_untyped_type(expr.expr_type), tprint(expr.derived));
	switch kind in &expr.derived {
		case Ast_Number: {
			switch number_kind in kind.base.constant_value {
				case i64: {
					reg := alloc_register(procedure, type_i64);
					output("movsim", reg.reg, number_kind);
					return reg;
				}
				case f64: {
					reg := alloc_register(procedure, type_f64);
					output("movfim", reg.reg, transmute(u64)number_kind);
					return reg;
				}
				case u64: {
					reg := alloc_register(procedure, type_u64);
					output("movuim", reg.reg, number_kind);
					return reg;
				}
				case: assert(false, tprint(number_kind));
			}
		}

		case Ast_Identifier: {
			switch decl_type in kind.declaration.kind {
				case Var_Decl: {
					reg := alloc_register(procedure, decl_type.var.type);
					mov_var_to_reg(decl_type.var, reg, procedure);
					return reg;
				}
				case: assert(false, tprint(decl_type));
			}
		}

		case Ast_Binary: {
			assert(kind.lhs.expr_type != nil);
			assert(kind.rhs.expr_type != nil);
			lhs_reg := emit_expr(kind.lhs, procedure);
			rhs_reg := emit_expr(kind.rhs, procedure);
			defer {
				free_register(procedure, lhs_reg);
				free_register(procedure, rhs_reg);
			}
			switch kind.op {
				case .Plus: {
					t := kind.lhs.expr_type;
					reg := alloc_register(procedure, t);
					if is_integer_type(t) {
						if is_signed_type(t) {
							output("adds", reg.reg, lhs_reg.reg, rhs_reg.reg);
						}
						else {
							output("addu", reg.reg, lhs_reg.reg, rhs_reg.reg);
						}
					}
					else if is_float_type(t) {
						output("addf", reg.reg, lhs_reg.reg, rhs_reg.reg);
					}
					else {
						panic(tprint(t));
					}
					return reg;
				}
				case .Minus: {
					t := kind.lhs.expr_type;
					reg := alloc_register(procedure, t);
					if is_integer_type(t) {
						if is_signed_type(t) {
							output("subs", reg.reg, lhs_reg.reg, rhs_reg.reg);
						}
						else {
							output("subu", reg.reg, lhs_reg.reg, rhs_reg.reg);
						}
					}
					else if is_float_type(t) {
						output("subf", reg.reg, lhs_reg.reg, rhs_reg.reg);
					}
					else {
						panic(tprint(t));
					}
					return reg;
				}
				case .Multiply: {
					t := kind.lhs.expr_type;
					reg := alloc_register(procedure, t);
					if is_integer_type(t) {
						if is_signed_type(t) {
							output("muls", reg.reg, lhs_reg.reg, rhs_reg.reg);
						}
						else {
							output("mulu", reg.reg, lhs_reg.reg, rhs_reg.reg);
						}
					}
					else if is_float_type(t) {
						output("mulf", reg.reg, lhs_reg.reg, rhs_reg.reg);
					}
					else {
						panic(tprint(t));
					}
					return reg;
				}
				case .Divide: {
					t := kind.lhs.expr_type;
					reg := alloc_register(procedure, t);
					if is_integer_type(t) {
						if is_signed_type(t) {
							output("divs", reg.reg, lhs_reg.reg, rhs_reg.reg);
						}
						else {
							output("divu", reg.reg, lhs_reg.reg, rhs_reg.reg);
						}
					}
					else if is_float_type(t) {
						output("divf", reg.reg, lhs_reg.reg, rhs_reg.reg);
					}
					else {
						panic(tprint(t));
					}
					return reg;
				}
				case .Boolean_Equal: {
					reg := alloc_register(procedure, kind.lhs.expr_type);
					output("eq", reg.reg, lhs_reg.reg, rhs_reg.reg);
					return reg;
				}
				case: assert(false, tprint(kind.op));
			}
		}

		case Ast_Unary: {
			switch kind.op {
				case .Minus: {
					assert(is_signed_type(kind.rhs.expr_type));
					reg := emit_expr(kind.rhs, procedure);
					output("neg", reg.reg);
					return reg;
				}
			}
		}

		case Ast_Call: {
			unimplemented();
			// emit_call(kind, &result_register);
		}

		case Ast_Selector: {
			unimplemented();
			// storage := find_storage(kind);
			// emit_copy(result_register, storage);
		}

		case: assert(false, tprint(kind));
	}
	unreachable();
	return {};
}

mov_var_to_reg :: proc(var: ^Ast_Var, reg: Register_Allocation, procedure: ^Ast_Proc) {
	if var.is_local	{
		address_reg := alloc_register(procedure, type_u64);
		output("adduim", address_reg.reg, procedure.stack_pointer_reg.reg, var.offset_in_stack_frame);
		defer free_register(procedure, address_reg);

		switch {
			case is_float_type(var.type): {
				switch var.type.size {
					case 4: output("ld32f", reg.reg, address_reg.reg);
					case 8: output("ld64f", reg.reg, address_reg.reg);
					case: panic(tprint(var.type.size));
				}
			}
			case is_integer_type(var.type) && is_signed_type(var.type): {
				switch var.type.size {
					case 1: output("ld8s",  reg.reg, address_reg.reg);
					case 2: output("ld16s", reg.reg, address_reg.reg);
					case 4: output("ld32s", reg.reg, address_reg.reg);
					case 8: output("ld64s", reg.reg, address_reg.reg);
					case: panic(tprint(var.type.size));
				}
			}
			case is_integer_type(var.type) && is_unsigned_type(var.type): {
				switch var.type.size {
					case 1: output("ld8u",  reg.reg, address_reg.reg);
					case 2: output("ld16u", reg.reg, address_reg.reg);
					case 4: output("ld32u", reg.reg, address_reg.reg);
					case 8: output("ld64u", reg.reg, address_reg.reg);
					case: panic(tprint(var.type.size));
				}
			}
			case: {
				panic(tprint(var.type));
			}
		}
	}
	else {
		unimplemented();
	}
}

mov_reg_to_var :: proc(reg: Register_Allocation, var: ^Ast_Var, procedure: ^Ast_Proc) {
	if var.is_local	{
		address_reg := alloc_register(procedure, type_u64);
		output("adduim", address_reg.reg, procedure.stack_pointer_reg.reg, var.offset_in_stack_frame);
		defer free_register(procedure, address_reg);

		switch {
			case is_float_type(var.type): {
				switch var.type.size {
					case 4: output("sv32f", address_reg.reg, reg.reg);
					case 8: output("sv64f", address_reg.reg, reg.reg);
					case: panic(tprint(var.type.size));
				}
			}
			case is_integer_type(var.type) && is_signed_type(var.type): {
				switch var.type.size {
					case 1: output("sv8s",  address_reg.reg, reg.reg);
					case 2: output("sv16s", address_reg.reg, reg.reg);
					case 4: output("sv32s", address_reg.reg, reg.reg);
					case 8: output("sv64s", address_reg.reg, reg.reg);
					case: panic(tprint(var.type.size));
				}
			}
			case is_integer_type(var.type) && is_unsigned_type(var.type): {
				switch var.type.size {
					case 1: output("sv8u",  address_reg.reg, reg.reg);
					case 2: output("sv16u", address_reg.reg, reg.reg);
					case 4: output("sv32u", address_reg.reg, reg.reg);
					case 8: output("sv64u", address_reg.reg, reg.reg);
					case: panic(tprint(var.type.size));
				}
			}
			case: {
				panic(tprint(var.type));
			}
		}
	}
	else {
		unimplemented();
	}
}

// set_len :: proc(arr: ^[dynamic]$T, length: int) {
// 	assert(len(arr) >= length);
// 	(cast(^mem.Raw_Dynamic_Array)arr).len = length;
// }

// emit_call :: proc(ws: ^Workspace, ast_call: ^Ast_Call, procedure: ^Ast_Proc, result_register: Maybe(Register)) {
	// regs_before_args := len(ast_call.parent_procedure.block.register_allocations);
	// defer set_len(&ast_call.parent_procedure.block.register_allocations, regs_before_args);

	// call_result: Register_Allocation;
	// if result_register != nil {
	// 	assert(ast_call.expr_type != nil);
	// 	call_result = alloc_register(ast_call.parent_procedure, ast_call.expr_type);
	// }

	// args: [dynamic]Register_Allocation;
	// defer delete(args); // bleh
	// for arg in ast_call.args {
	// 	ra := alloc_register(arg.parent_procedure, arg.expr_type);
	// 	emit_expr(arg, ra);
	// 	append(&args, ra);
	// }
	// for reg_idx in 0..<regs_before_args {
	// 	reg := ast_call.parent_procedure.block.register_allocations[reg_idx];
	// 	if result_register != nil && reg.start == result_register.start {
	// 		continue;
	// 	}
	// 	copy_to_stack(&ws.vm, reg);
	// }
	// for arg in args {
	// 	copy_to_stack(&ws.vm, arg);
	// }

	// call(&ws.vm, ast_call.procedure.derived.(Ast_Identifier).name);

	// if call_result.size > 0 {
	// 	if result_register != nil {
	// 		copy_from_stack(&ws.vm, result_register^);
	// 	}
	// }

	// for reg_idx := regs_before_args-1; reg_idx	>= 0; reg_idx -= 1 {
	// 	reg := ast_call.parent_procedure.block.register_allocations[reg_idx];
	// 	if result_register != nil && reg.start == result_register.start {
	// 		continue;
	// 	}
	// 	copy_from_stack(&ws.vm, reg);
	// }
// }

// emit_return :: proc(ws: ^Workspace, procedure: ^Ast_Proc, expr: ^Ast_Node) {
// 	assert(procedure.return_address_register.size > 0);
// 	if expr != nil {
// 		return_value_reg := alloc_register(expr.parent_procedure, expr.expr_type);
// 		emit_expr(expr, return_value_reg);
// 		copy_to_stack(&ws.vm, return_value_reg);
// 	}
// 	addi(&ws.vm, procedure.return_address_register.start, procedure.return_address_register.start, 1);
// 	mov(&ws.vm, rip, procedure.return_address_register.start);
// }

Register_Allocation :: struct {
	reg: Register,
	type: ^Type,
}

alloc_register :: proc(procedure: ^Ast_Proc, type: ^Type, loc := #caller_location) -> Register_Allocation {
	assert(procedure != nil);
	for r in Register.r1..Register.r10 {
		if reg, ok := try_alloc_register(procedure, r, type); ok {
			return reg;
		}
	}
	panic("Too many register allocations!!!");
	return {};
}

try_alloc_register :: proc(procedure: ^Ast_Proc, requested_register: Register, type: ^Type) -> (Register_Allocation, bool) {
	found := false;
	for active in procedure.registers_in_use {
		if active.reg == requested_register {
			found = true;
			break;
		}
	}
	if !found {
		allocation := Register_Allocation{requested_register, type};
		append(&procedure.registers_in_use, allocation);
		return allocation, true;
	}
	return {}, false;
}

free_register :: proc(procedure: ^Ast_Proc, reg: Register_Allocation) {
	for r, idx in procedure.registers_in_use {
		if r.reg == reg.reg {
			unordered_remove(&procedure.registers_in_use, idx);
			return;
		}
	}
	panic("Didn't allocate this register");
}

array_contains :: proc(array: $T/[]$E, val: E) -> bool {
	for elem in array {
		if elem == val {
			return true;
		}
	}
	return false;
}
