package crest

IR_Result :: struct {
	global_scope: ^IR_Scope,
	entry_point: ^IR_Proc,
}

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