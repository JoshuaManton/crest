package crest

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
	quit(&ws.vm);

	for procedure in ws.all_procedures {
		if procedure == main_proc do continue;
		emit_procedure(ws, procedure);
	}

	execute(&ws.vm);

	// foo_decl := try_find_declaration_in_block(ws.global_scope, "foo");
	// assert(foo_decl != nil);
	// foo_proc := foo_decl.kind.(Proc_Decl).procedure;

	vec_decl := try_find_declaration_in_block(main_proc.block, "vec");
	assert(vec_decl != nil);
	println("vec.x is:", ws.vm.register_memory[vec_decl.kind.(Var_Decl).var.register.start]);
	println("vec.y is:", ws.vm.register_memory[vec_decl.kind.(Var_Decl).var.register.start+1]);
}

emit_procedure :: proc(ws: ^Workspace, procedure: ^Ast_Proc, is_main := false) {
	// frame_pointer := procedure.block.current_register;
	// defer procedure.block.current_register = frame_pointer;
	label(&ws.vm, procedure.name);

	if !is_main {
		procedure.return_address_register = alloc_register(procedure, type_u64);
		stack_pop(&ws.vm, procedure.return_address_register.start);
		for param in procedure.params {
			param.register = alloc_register(procedure, param.type);
		}
		for param_idx := len(procedure.params)-1; param_idx >= 0; param_idx -= 1 {
			param := procedure.params[param_idx];
			assert(param.register.size > 0);
			copy_from_stack(&ws.vm, param.register);
		}
	}

	for var in procedure.variables {
		var.register = alloc_register(procedure, var.type);
	}

	emit_block(ws, procedure.block, procedure);
	if !is_main {
		emit_return(ws, procedure, nil);
	}
}

emit_block :: proc(ws: ^Workspace, block: ^Ast_Block, procedure: ^Ast_Proc = nil) {
	for stmt in block.stmts {
		switch kind in &stmt.derived {
			case Ast_Var: {
				if kind.expr != nil {
					emit_expr(ws, kind.expr, kind.register);
				}
			}
			case Ast_If: {
				result_register := alloc_register(kind.condition.parent_procedure, kind.condition.expr_type);
				emit_expr(ws, kind.condition, result_register);
				jump_label := aprint("if_", stmt.serial);
				jez(&ws.vm, jump_label, result_register.start); // todo(josh): change this to jeq with rz
				emit_block(ws, kind.block);
				label(&ws.vm, jump_label);
			}
			case Ast_Return: {
				assert(procedure != nil);
				emit_return(ws, procedure, kind.expr);
			}
			case Ast_Call: {
				emit_call(ws, kind, nil);
			}
			case Ast_Assign: {
				emit_assign(ws, kind);
			}
			case Ast_Comment: {
				// nothing
			}
			case: assert(false, tprint(kind));
		}
	}
}

// todo(josh): implement a `copy` instruction in the VM
emit_copy :: proc(ws: ^Workspace, dst: Register_Allocation, src: Register_Allocation) {
	assert(dst.size == src.size, tprint(dst, src));
	for i in 0..<dst.size {
		mov(&ws.vm, dst.start+i, src.start+i);
	}
}

emit_assign :: proc(ws: ^Workspace, assign: ^Ast_Assign) {
	storage := find_storage(assign.left);

	switch assign.op {
		case .Assign: {
			emit_expr(ws, assign.right, storage);
		}
		case .Plus_Assign: {
			expr_reg := alloc_register(assign.parent_procedure, assign.right.expr_type);
			assert(storage.size == 1); // todo(josh): array programming?
			assert(expr_reg.size == storage.size);
			emit_expr(ws, assign.right, expr_reg);
			add(&ws.vm, storage.start, storage.start, expr_reg.start);
		}
		case: assert(false, tprint(assign.op));
	}
}

find_storage :: proc(node: ^Ast_Node) -> Register_Allocation {
	switch kind in node.derived {
		case Ast_Identifier: {
			switch decl_kind in kind.declaration.kind {
				case Var_Decl: {
					assert(decl_kind.var.register.size > 0);
					return decl_kind.var.register;
				}
				case: assert(false, tprint(decl_kind));
			}
		}
		case Ast_Selector: {
			var_storage := find_storage(kind.left);
			ss := &kind.left.expr_type.kind.(Type_Struct);
			idx := get_field_idx(ss, kind.field);
			storage := Register_Allocation{var_storage.start+(ss.offsets[idx] / TARGET_PLATFORM_ALIGNMENT), ss.types[idx].register_size};
			return storage;
		}
		case: assert(false, tprint(kind));
	}
	unreachable();
	return {};
}

get_field_idx :: proc(type: ^Type_Struct, name: string) -> int {
	for field, idx in type.fields {
		if field == name {
			return idx;
		}
	}
	unreachable();
	return {};
}

emit_expr :: proc(ws: ^Workspace, expr: ^Ast_Node, result_register: Register_Allocation) {
	assert(result_register.size > 0);
	regs := len(expr.parent.register_allocations);
	defer set_len(&expr.parent.register_allocations, regs);

	switch kind in &expr.derived {
		case Ast_Number: {
			switch number_kind in kind.base.constant_value {
				case i64: {
					movis(&ws.vm, result_register.start, number_kind);
				}
				case f64: {
					movif(&ws.vm, result_register.start, number_kind);
				}
				case u64: {
					movi(&ws.vm, result_register.start, number_kind);
				}
				case: assert(false, tprint(number_kind));
			}
		}

		case Ast_Identifier: {
			switch decl_type in kind.declaration.kind {
				case Var_Decl: {
					emit_copy(ws, result_register, decl_type.var.register);
				}
				case: assert(false, tprint(decl_type));
			}
		}

		case Ast_Binary: {
			lhs_reg := alloc_register(expr.parent_procedure, kind.lhs.expr_type);
			rhs_reg := alloc_register(expr.parent_procedure, kind.rhs.expr_type);

			emit_expr(ws, kind.lhs, lhs_reg);
			emit_expr(ws, kind.rhs, rhs_reg);
			assert(lhs_reg.size == 1); // todo(josh): array programming?
			assert(rhs_reg.size == lhs_reg.size);
			switch kind.op {
				case .Plus: {
					t := kind.lhs.expr_type;
					if is_integer_type(t) {
						if is_signed_type(t) {
							add(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
						}
						else {
							addu(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
						}
					}
					else if is_float_type(t) {
						addf(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
					}
				}
				case .Minus: {
					t := kind.lhs.expr_type;
					if is_integer_type(t) {
						if is_signed_type(t) {
							sub(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
						}
						else {
							subu(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
						}
					}
					else if is_float_type(t) {
						subf(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
					}
				}
				case .Multiply: {
					t := kind.lhs.expr_type;
					if is_integer_type(t) {
						if is_signed_type(t) {
							mul(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
						}
						else {
							mulu(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
						}
					}
					else if is_float_type(t) {
						mulf(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
					}
				}
				case .Boolean_Equal: {
					eq(&ws.vm, result_register.start, lhs_reg.start, rhs_reg.start);
				}
				case: assert(false, tprint(kind.op));
			}
		}

		case Ast_Call: {
			emit_call(ws, kind, &result_register);
		}

		case Ast_Selector: {
			storage := find_storage(kind);
			emit_copy(ws, result_register, storage);
		}

		case: assert(false, tprint(kind));
	}
}

set_len :: proc(arr: ^[dynamic]$T, length: int) {
	assert(len(arr) >= length);
	(cast(^mem.Raw_Dynamic_Array)arr).len = length;
}

emit_call :: proc(ws: ^Workspace, ast_call: ^Ast_Call, result_register: ^Register_Allocation) {
	regs_before_args := len(ast_call.parent_procedure.block.register_allocations);
	defer set_len(&ast_call.parent_procedure.block.register_allocations, regs_before_args);

	call_result: Register_Allocation;
	if result_register != nil {
		assert(ast_call.expr_type != nil);
		call_result = alloc_register(ast_call.parent_procedure, ast_call.expr_type);
	}

	args: [dynamic]Register_Allocation;
	defer delete(args); // bleh
	for arg in ast_call.args {
		ra := alloc_register(arg.parent_procedure, arg.expr_type);
		emit_expr(ws, arg, ra);
		append(&args, ra);
	}
	for reg_idx in 0..<regs_before_args {
		reg := ast_call.parent_procedure.block.register_allocations[reg_idx];
		if result_register != nil && reg.start == result_register.start {
			continue;
		}
		copy_to_stack(&ws.vm, reg);
	}
	for arg in args {
		copy_to_stack(&ws.vm, arg);
	}

	call(&ws.vm, ast_call.procedure.derived.(Ast_Identifier).name);

	if call_result.size > 0 {
		if result_register != nil {
			copy_from_stack(&ws.vm, result_register^);
		}
	}

	for reg_idx := regs_before_args-1; reg_idx	>= 0; reg_idx -= 1 {
		reg := ast_call.parent_procedure.block.register_allocations[reg_idx];
		if result_register != nil && reg.start == result_register.start {
			continue;
		}
		copy_from_stack(&ws.vm, reg);
	}
}

emit_return :: proc(ws: ^Workspace, procedure: ^Ast_Proc, expr: ^Ast_Node) {
	assert(procedure.return_address_register.size > 0);
	if expr != nil {
		return_value_reg := alloc_register(expr.parent_procedure, expr.expr_type);
		emit_expr(ws, expr, return_value_reg);
		copy_to_stack(&ws.vm, return_value_reg);
	}
	addi(&ws.vm, procedure.return_address_register.start, procedure.return_address_register.start, 1);
	mov(&ws.vm, rip, procedure.return_address_register.start);
}

copy_to_stack :: proc(vm: ^VM, ra: Register_Allocation, loc := #caller_location) {
	for r in ra.start..<ra.start+ra.size {
		stack_push(vm, r);
	}
}

copy_from_stack :: proc(vm: ^VM, ra: Register_Allocation, to_zero: bool = false) {
	for r := ra.start + ra.size-1; r >= ra.start; r -= 1 {
		if to_zero {
			stack_pop(vm, rz);
		}
		else {
			stack_pop(vm, r);
		}

		if r == 0 {
			break;
		}
	}
}

alloc_register :: proc(procedure: ^Ast_Proc, type: ^Type, loc := #caller_location) -> Register_Allocation {
	assert(procedure != nil);
	size := type.register_size;
	if is_untyped_type(type) {
		// todo(josh): figure this out properly. I don't know how to handle untyped stuff here
		// maybe there should be no untyped types at this point??
		size = 1;
	}
	assert(size != 0, tprint(type.kind));

	register := procedure.block.next_available_register;
	procedure.block.next_available_register += size;
	result := Register_Allocation{register, size};
	append(&procedure.block.register_allocations, result);

	return result;
}

Register_Allocation :: struct {
	start: u64,
	size: u64,
}