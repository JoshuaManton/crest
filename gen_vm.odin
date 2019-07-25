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
	main_proc.is_main = true;
	emit_procedure(ws, main_proc);

	for procedure in ws.all_procedures {
		if procedure == main_proc do continue;
		emit_procedure(ws, procedure);
	}

	println(strings.to_string(vm_code));

	vm: VM;
	parse_and_execute(strings.to_string(vm_code), &vm);

	if main_proc.return_type != nil {
		switch kind in main_proc.return_type.kind {
			case Type_Integer: {
				if kind.signed {
					switch main_proc.return_type.size {
						case 1: println((cast(^i8 )&vm.memory[vm.registers[reg(.rsp)]-1])^);
						case 2: println((cast(^i16)&vm.memory[vm.registers[reg(.rsp)]-2])^);
						case 4: println((cast(^i32)&vm.memory[vm.registers[reg(.rsp)]-4])^);
						case 8: println((cast(^i64)&vm.memory[vm.registers[reg(.rsp)]-8])^);
					}
				}
			}
			case: panic(tprint(kind));
		}
	}

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

emit_procedure :: proc(ws: ^Workspace, procedure: ^Ast_Proc) {
	// frame_pointer := procedure.block.current_register;
	// defer procedure.block.current_register = frame_pointer;
	output(":", procedure.name);

	if !procedure.is_main {
		procedure.return_address_reg = alloc_register(procedure, type_u64);
		output("# save return address");
		output("pop64u", procedure.return_address_reg.reg);
	}

	// calculate stack frame size
	local_vars_stack_size: u64;
	{
		for var in procedure.variables {
			procedure.stack_frame_size += var.type.size;
			var.offset_in_stack_frame = procedure.stack_frame_size;
			local_vars_stack_size += var.type.size;
		}
		if len(procedure.params) > 0 {
			for param in procedure.params {
				procedure.stack_frame_size += param.type.size;
				param.offset_in_stack_frame = procedure.stack_frame_size;
			}
		}
	}

	output("# allocate our stack frame");
	output("adduim rsp rsp", local_vars_stack_size); // the params are already on the stack, so only add space for the locals
	output("mov rfp rsp");

	emit_block(ws, procedure.block, procedure);
	emit_return(procedure, nil);
}

emit_block :: proc(ws: ^Workspace, block: ^Ast_Block, procedure: ^Ast_Proc) {
	for stmt in block.stmts {
		if c, ok := stmt.derived.(Ast_Comment); ok {
			output("#", c.text);
			continue;
		}

		output("\n#", stmt.root_token.site);
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
				emit_return(procedure, kind.expr);
			}
			case Ast_Call: {
				emit_call(kind, procedure, false);
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

emit_return :: proc(procedure: ^Ast_Proc, return_expression: ^Ast_Node) {
	result_reg: Register_Allocation;
	if return_expression != nil {
		output("# return expression");
		result_reg = emit_expr(return_expression, procedure);
	}

	output("# pop our stack frame");
	output("movuim rim", procedure.stack_frame_size);
	output("subu rsp rsp rim");

	if return_expression != nil {
		output("# push return value");
		push_register(result_reg);
	}

	if procedure.is_main {
		output("quit");
	}
	else {
		output("# jump to return address");
		output("adduim", procedure.return_address_reg.reg, procedure.return_address_reg.reg, 1);
		output("mov rip", procedure.return_address_reg.reg);
	}
}

dynamic_array_from_mem :: proc(slice: $T/[]$E) -> [dynamic]E {
	return transmute([dynamic]E)mem.Raw_Dynamic_Array{&slice[0], 0, len(slice), nil_allocator()};
}

emit_call :: proc(ast_call: ^Ast_Call, procedure: ^Ast_Proc, give_result: bool) -> (Register_Allocation, bool) {
	saved_register_memory: [12]Register_Allocation;
	saved_registers := dynamic_array_from_mem(saved_register_memory[:]);
	output("# save registers");
	for reg in procedure.registers_in_use {
		push_register(reg);
		append(&saved_registers, reg);
	}
	assert(len(saved_registers) < cap(saved_registers));

	//
	output("# save our stack frame");
	output("push64u rfp");

	//
	if len(ast_call.args) > 0 {
		output("# push parameters");
		for arg in ast_call.args {
			assert(arg.expr_data.type != nil);
			reg := emit_expr(arg, procedure);
			push_register(reg);
			free_register(procedure, reg);
		}
	}

	//
	output("# save return address");
	output("push64u rip");

	//
	proc_name := ast_call.procedure.derived.(Ast_Identifier).declaration.kind.(Proc_Decl).procedure.name;
	output("# call", proc_name);
	output("goto", proc_name);

	//
	result_reg: Register_Allocation;
	has_result := false;
	proc_type := ast_call.procedure.expr_data.type.kind.(Type_Proc);
	if proc_type.return_type != nil {
		if give_result {
			has_result = true;
			result_reg = alloc_register(procedure, proc_type.return_type);
			output("# pop return value");
			pop_register(result_reg);
		}
		else {
			output("# discard return value");
			pop_register(Register_Allocation{.rz, proc_type.return_type});
		}
	}

	//
	output("# restore our stack frame");
	output("pop64u rfp");

	output("# restore saved registers");
	for i := len(saved_registers)-1; i >= 0; i -= 1 {
		reg := saved_registers[i];
		pop_register(reg);
	}

	return result_reg, has_result;
}

push_register :: proc(reg: Register_Allocation) {
	assert(!is_untyped_type(reg.type));
	switch kind in reg.type.kind {
		case Type_Integer: {
			if kind.signed {
				switch reg.type.size {
					case 1: output("push8s",  reg.reg);
					case 2: output("push16s", reg.reg);
					case 4: output("push32s", reg.reg);
					case 8: output("push64s", reg.reg);
					case: panic(tprint(reg.type.size));
				}
			}
			else {
				switch reg.type.size {
					case 1: output("push8u",  reg.reg);
					case 2: output("push16u", reg.reg);
					case 4: output("push32u", reg.reg);
					case 8: output("push64u", reg.reg);
					case: panic(tprint(reg.type.size));
				}
			}
		}
		case Type_Float: {
			switch reg.type.size {
				case 4: output("push32f", reg.reg);
				case 8: output("push64f", reg.reg);
				case: panic(tprint(reg.type.size));
			}
		}
		case: panic(tprint(reg.type.kind));
	}
}

pop_register :: proc(reg: Register_Allocation) {
	assert(!is_untyped_type(reg.type));
	switch kind in reg.type.kind {
		case Type_Integer: {
			if kind.signed {
				switch reg.type.size {
					case 1: output("pop8s",  reg.reg);
					case 2: output("pop16s", reg.reg);
					case 4: output("pop32s", reg.reg);
					case 8: output("pop64s", reg.reg);
					case: panic(tprint(reg.type.size));
				}
			}
			else {
				switch reg.type.size {
					case 1: output("pop8u",  reg.reg);
					case 2: output("pop16u", reg.reg);
					case 4: output("pop32u", reg.reg);
					case 8: output("pop64u", reg.reg);
					case: panic(tprint(reg.type.size));
				}
			}
		}
		case Type_Float: {
			switch reg.type.size {
				case 4: output("pop32f", reg.reg);
				case 8: output("pop64f", reg.reg);
				case: panic(tprint(reg.type.size));
			}
		}
		case: panic(tprint(reg.type.kind));
	}
}

emit_assign :: proc(ws: ^Workspace, assign: ^Ast_Assign, procedure: ^Ast_Proc) {
	switch left_kind in &assign.lhs.derived {
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
	assert(!is_untyped_type(expr.expr_data.type), tprint(expr.derived));
	if expr.expr_data.constant_value != nil {
		reg: Register_Allocation;
		switch kind in expr.expr_data.constant_value {
			case i64: {
				reg = alloc_register(procedure, expr.expr_data.type);
				output("movsim", reg.reg, kind);
			}
			case u64: {
				reg = alloc_register(procedure, expr.expr_data.type);
				output("movuim", reg.reg, kind);
			}
			case f64: {
				reg = alloc_register(procedure, expr.expr_data.type);
				output("movfim", reg.reg, transmute(u64)kind);
			}
			case: panic(tprint(kind));
		}
		return reg;
	}

	switch kind in &expr.derived {
		case Ast_Number: {
			switch number_kind in kind.base.expr_data.constant_value {
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
			assert(kind.lhs.expr_data.type != nil);
			assert(kind.rhs.expr_data.type != nil);
			lhs_reg := emit_expr(kind.lhs, procedure);
			rhs_reg := emit_expr(kind.rhs, procedure);
			defer {
				free_register(procedure, lhs_reg);
				free_register(procedure, rhs_reg);
			}
			switch kind.op {
				case .Plus: {
					t := kind.lhs.expr_data.type;
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
					t := kind.lhs.expr_data.type;
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
					t := kind.lhs.expr_data.type;
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
					t := kind.lhs.expr_data.type;
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
					reg := alloc_register(procedure, kind.lhs.expr_data.type);
					output("eq", reg.reg, lhs_reg.reg, rhs_reg.reg);
					return reg;
				}
				case: assert(false, tprint(kind.op));
			}
		}

		case Ast_Unary: {
			switch kind.op {
				case .Minus: {
					assert(is_signed_type(kind.rhs.expr_data.type));
					reg := emit_expr(kind.rhs, procedure);
					output("neg", reg.reg);
					return reg;
				}
			}
		}

		case Ast_Call: {
			proc_type, ok := kind.procedure.expr_data.type.kind.(Type_Proc);
			assert(ok);
			assert(proc_type.return_type != nil); // if we ended up in emit_expr() with a proc call then it should have a return value

			result, has_result := emit_call(kind, procedure, true);
			assert(has_result);
			return result;
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
		output("addsim", address_reg.reg, "rfp", -cast(i64)var.offset_in_stack_frame);
		defer free_register(procedure, address_reg);

		switch kind in var.type.kind {
			case Type_Integer: {
				if kind.signed {
					switch var.type.size {
						case 1: output("ld8s",  reg.reg, address_reg.reg);
						case 2: output("ld16s", reg.reg, address_reg.reg);
						case 4: output("ld32s", reg.reg, address_reg.reg);
						case 8: output("ld64s", reg.reg, address_reg.reg);
						case: panic(tprint(var.type.size));
					}
				}
				else {
					switch var.type.size {
						case 1: output("ld8u",  reg.reg, address_reg.reg);
						case 2: output("ld16u", reg.reg, address_reg.reg);
						case 4: output("ld32u", reg.reg, address_reg.reg);
						case 8: output("ld64u", reg.reg, address_reg.reg);
						case: panic(tprint(var.type.size));
					}
				}
			}
			case Type_Float: {
				switch var.type.size {
					case 4: output("ld32f", reg.reg, address_reg.reg);
					case 8: output("ld64f", reg.reg, address_reg.reg);
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
		output("addsim", address_reg.reg, "rfp", -cast(i64)var.offset_in_stack_frame);
		defer free_register(procedure, address_reg);

		switch kind in var.type.kind {
			case Type_Integer: {
				if kind.signed {
					switch var.type.size {
						case 1: output("sv8s",  address_reg.reg, reg.reg);
						case 2: output("sv16s", address_reg.reg, reg.reg);
						case 4: output("sv32s", address_reg.reg, reg.reg);
						case 8: output("sv64s", address_reg.reg, reg.reg);
						case: panic(tprint(var.type.size));
					}
				}
				else {
					switch var.type.size {
						case 1: output("sv8u",  address_reg.reg, reg.reg);
						case 2: output("sv16u", address_reg.reg, reg.reg);
						case 4: output("sv32u", address_reg.reg, reg.reg);
						case 8: output("sv64u", address_reg.reg, reg.reg);
						case: panic(tprint(var.type.size));
					}
				}
			}
			case Type_Float: {
				switch var.type.size {
					case 4: output("sv32f", address_reg.reg, reg.reg);
					case 8: output("sv64f", address_reg.reg, reg.reg);
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
