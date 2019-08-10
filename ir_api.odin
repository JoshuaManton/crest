package crest

using import "core:fmt"

using import "shared:workbench/logging"

IR_Result :: struct {
	global_vars: [dynamic]^IR_Var,
	procedures: [dynamic]^IR_Proc,
	entry_point: ^IR_Proc,
}

IR_Proc :: struct {
	params: [dynamic]^IR_Var,
	vars: [dynamic]^IR_Var,
	stmts: [dynamic]^IR_Stmt,
}

IR_Var :: struct {
	type: ^Type,
	// storage: IR_Storage,
}

// IR_Storage :: union {
// 	Stack_Storage,
// 	Static_Storage,
// }
// Stack_Storage :: struct {
// 	offset_in_frame: u64,
// }
// Static_Storage :: struct {
// 	data_segment_index: int,
// }

IR_Scope :: struct {
	parent: ^IR_Scope,
	procedures: [dynamic]^IR_Proc,
}

// IR_Call :: struct {
// 	target_procedure: ^IR_Proc,
// 	params: []^IR_Var,
// }

IR_Binary :: struct {
	op: IR_Op,
	result: ^IR_Var,
	lhs: ^IR_Var,
	rhs: ^IR_Var,
}

IR_Op :: enum {
	None,
	Add,
	Minus,
	Multiply,
	Divide,
	Mod,
	ModMod,
}

IR_Assign :: struct {
	rhs: ^IR_Var,
}

IR_Stmt :: struct {
	kind: union {
		IR_Binary,
	},
}

Register :: int;

ir_result: ^IR_Result;

ir_procedure :: proc() -> ^IR_Proc {
	return new_clone(IR_Proc{});
}

ir_local_var :: proc(procedure: ^IR_Proc, type: ^Type) -> ^IR_Var {
	var := new_clone(IR_Var{type});
	append(&procedure.vars, var);
	return var;
}
ir_proc_param :: proc(procedure: ^IR_Proc, type: ^Type) -> ^IR_Var {
	var := new_clone(IR_Var{type});
	append(&procedure.params, var);
	return var;
}
ir_global_var :: proc(result: ^IR_Result, type: ^Type) -> ^IR_Var {
	unimplemented();
	return {};
}

ir_stmt :: proc(procedure: ^IR_Proc, kind: $T) -> ^IR_Stmt {
	stmt := new_clone(IR_Stmt{kind});
	append(&procedure.stmts, stmt);
	return stmt;
}

ir_assign :: proc(procedure: ^IR_Proc, destination: ^IR_Var, rhs: ^IR_Var) {
	unimplemented();
}

// ir_call :: proc(parent_procedure: ^IR_Proc, target_procedure: ^IR_Proc, params: [dynamic]^IR_Expr) -> ^IR_Call {
// 	assert(len(params) == len(target_procedure.params));
// 	param_vars: [dynamic]^IR_Var;
// 	for p, idx in params {
// 		assert(p.type == target_procedure.params[idx].type);
// 		v := ir_decompose_expr_into_temporaries(parent_procedure, p);
// 		append(&param_vars, v);
// 	}
// 	return new_clone(IR_Call{target_procedure, param_vars[:]});
// }

ir_binop :: proc(procedure: ^IR_Proc, op: IR_Op, lhs, rhs: ^IR_Var) -> ^IR_Var {
	assert(lhs.type == rhs.type);
	result := ir_local_var(procedure, lhs.type);
	ir_stmt(procedure, IR_Binary{op, result, lhs, rhs});
	return result;
}

ir_return :: proc(procedure: ^IR_Proc, res: ^IR_Var) {
	unimplemented();
}

test_ir :: proc(ws: ^Workspace) {
	ir_result = new(IR_Result);

	procedure := ir_procedure();
	a := ir_proc_param(procedure, type_int);
	b := ir_proc_param(procedure, type_int);
	c := ir_binop(procedure, .Add, a, b);
	d := ir_binop(procedure, .Add, ir_binop(procedure, .Minus, a, b), ir_binop(procedure, .Add, a, b));

	for stmt in procedure.stmts {
		logln(stmt);
	}

	// ir_assign(procedure, c, ir_binop(procedure, .Add, ir_expr(IR_Var_Expr{a}, a.type), ir_expr(IR_Var_Expr{b}, b.type)));
	// ir_return(procedure, c);

	ir_analyze(ir_result);
	emit_ir_result(ir_result);
}

ir_analyze :: proc(using result: ^IR_Result) {
	// figure out stack offsets for all variables
	for procedure in procedures {
		for param in procedure.params {

		}

		for var in procedure.vars {

		}
	}

	// allocate room in static storage for global variables
	for global in global_vars {

	}
}

emit_ir_result :: proc(using result: ^IR_Result) {
	for procedure in procedures {
		emit_ir_procedure(procedure);
	}
}

emit_ir_procedure :: proc(procedure: ^IR_Proc) {

}

Register_Allocation :: struct {
	reg: Register,
	type: ^Type,
}