package crest

using import "core:fmt"
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

	foo_decl := try_find_declaration_in_block(ws.global_scope, "foo");
	assert(foo_decl != nil);
	foo_proc := foo_decl.kind.(Proc_Decl).procedure;

	x_decl := try_find_declaration_in_block(main_proc.block, "x");
	assert(x_decl != nil);
	println(ws.vm.register_memory[x_decl.kind.(Var_Decl).var.register.register]);
}

emit_procedure :: proc(ws: ^Workspace, procedure: ^Ast_Proc, is_main := false) {
	frame_pointer := procedure.block.current_register;
	defer procedure.block.current_register = frame_pointer;

	label(&ws.vm, procedure.name);

	if !is_main {
		stack_pop(&ws.vm, rt);
		i : u64 = 0;
		for param in procedure.params {
			stack_pop(&ws.vm, i);
			i += 1;
		}
		stack_push(&ws.vm, rt);
	}
	
	for var in procedure.variables {
		var.register = alloc_register(procedure.block, var.type);
	}

	emit_block(ws, procedure.block);
	if !is_main {
		emit_return(ws, nil);
	}
}

emit_block :: proc(ws: ^Workspace, block: ^Ast_Block) {
	for stmt in block.stmts {
		switch kind in &stmt.derived {
			case Ast_Var: {
				if kind.expr != nil {
					result_register := emit_expr(ws, kind.expr);
					emit_copy(ws, kind.register, result_register);
				}
			}
			case Ast_If: {
				condition_reg := emit_expr(ws, kind.condition);
				movi(&ws.vm, rim, 1);
				jump_label := aprint("if_", stmt.serial);
				jlt(&ws.vm, jump_label, condition_reg.register, rim);
				emit_block(ws, kind.block);
				label(&ws.vm, jump_label);
			}
			case Ast_Return: {
				emit_return(ws, kind.expr);
			}
			case Ast_Call: {
				emit_call(ws, kind);
			}
			case Ast_Assign: {
				emit_assign(ws, kind);
			}
			case: assert(false, tprint(kind));
		}
	}
}

// todo(josh): implement a `copy` instruction in the VM
emit_copy :: proc(ws: ^Workspace, dst: Register_Allocation, src: Register_Allocation) {
	assert(dst.num_registers == src.num_registers);
	for i in 0..<dst.num_registers {
		mov(&ws.vm, dst.register+i, src.register+i);
	}
}

emit_assign :: proc(ws: ^Workspace, assign: ^Ast_Assign) {
	rhs_reg := emit_expr(ws, assign.right);
	var := assign.left.derived.(Ast_Identifier).declaration.kind.(Var_Decl).var; // todo(josh): other types of rhs than vars
	switch assign.op {
		case .Assign: {
			emit_copy(ws, var.register, rhs_reg);
		}
		case .Plus_Assign: {
			add(&ws.vm, var.register.register, var.register.register, rhs_reg.register);
		}
		case: assert(false, tprint(assign.op));
	}
}

emit_expr :: proc(ws: ^Workspace, expr: ^Ast_Node) -> Register_Allocation {
	result_register := alloc_register(expr.parent, expr.expr_type);
	switch kind in &expr.derived {
		case Ast_Number: {
			switch number_kind in kind.base.constant_value {
				case i64: {
					movis(&ws.vm, result_register.register, number_kind);
				}
				case f64: {
					movif(&ws.vm, result_register.register, number_kind);
				}
				case u64: {
					movi(&ws.vm, result_register.register, number_kind);
				}
				case: assert(false, tprint(number_kind));
			}
		}

		case Ast_Identifier: {
			switch decl_type in kind.declaration.kind {
				case Var_Decl: {
					return decl_type.var.register;
				}
				case: assert(false, tprint(decl_type));
			}
		}

		case Ast_Binary: {
			lhs_reg := emit_expr(ws, kind.lhs);
			rhs_reg := emit_expr(ws, kind.rhs);
			switch kind.op {
				case .Plus: {
					t := kind.lhs.expr_type;
					if is_integer_type(t) {
						if is_signed_type(t) {
							add(&ws.vm, result_register.register, lhs_reg.register, rhs_reg.register);
						}
						else {
							addu(&ws.vm, result_register.register, lhs_reg.register, rhs_reg.register);
						}
					}
					else if is_float_type(t) {
						addf(&ws.vm, result_register.register, lhs_reg.register, rhs_reg.register);
					}
				}
				case .Boolean_Equal: {
					eq(&ws.vm, result_register.register, lhs_reg.register, rhs_reg.register);
				}
				case: assert(false, tprint(kind.op));
			}
		}

		case Ast_Call: {
			emit_call(ws, kind, &result_register, kind.expr_type);
		}

		case: assert(false, tprint(kind));
	}

	return result_register;
}

emit_call :: proc(ws: ^Workspace, ast_call: ^Ast_Call, result_register: ^Register_Allocation = nil, result_type: ^Type = nil) {
	for arg in ast_call.args {
		reg := emit_expr(ws, arg);
	}
	for i in 0..<ast_call.parent.current_register {
		stack_push(&ws.vm, i);
	}
	call(&ws.vm, ast_call.procedure.derived.(Ast_Identifier).name);
	// if result_register != nil {
	// 	stack_pop(&ws.vm, result_register.register);
	// }

	if ast_call.parent.current_register > 0 {
		i := ast_call.parent.current_register-1;
		for {
			stack_pop(&ws.vm, i);
			if i == 0 do break;
			i -= 1;
		}
	}
}

emit_return :: proc(ws: ^Workspace, expr: ^Ast_Node) {
	stack_pop(&ws.vm, rt);
	if expr != nil {
		return_value_reg := emit_expr(ws, expr);
		stack_push(&ws.vm, return_value_reg.register);
	}
	addi(&ws.vm, rt, rt, 1);
	ret(&ws.vm);
}

alloc_register :: proc(block: ^Ast_Block, type: ^Type, loc := #caller_location) -> Register_Allocation {
	size := type.register_size;
	if is_untyped_type(type) {
		// todo(josh): figure this out properly. I don't know how to handle untyped stuff here
		size = 1;
	}
	assert(size != 0, tprint(type.kind));
	register := block.current_register;
	block.current_register += size;
	return Register_Allocation{register, size};
}

Register_Allocation :: struct {
	register: u64,
	num_registers: u64,
}