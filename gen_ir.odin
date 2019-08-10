package crest

IR_Program :: struct {
	procedures: []IR_Proc,
	global_variables: []IR_Var,
}

generate_vm_from_ir :: proc(program: ^IR_Program) {

}
/*
generate_ir_for_workspace :: proc(ws: ^Workspace) -> IR_Result {
	global_scope := make_ir_entity(nil, IR_Scope{nil, nil, nil});

	main_proc: ^Ast_Proc;

	entry_point: ^IR_Proc;
	for procedure in global_scope.procedures {
		if procedure.is_entry_point {
			entry_point = procedure;
			break;
		}
	}
	return IR_Result{global_scope, nil};
}
*/