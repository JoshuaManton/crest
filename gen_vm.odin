package crest

using import "shared:workbench/logging"
using import "../storm"

VM_ALIGNMENT :: 8;

generate_and_execute_workspace :: proc(ws: ^Workspace) {
	assert(false);
	vm: VM;
	for procedure in ws.all_procedures {
		emit_procedure(&vm, procedure);
	}
	logln(vm.instructions_readable);
	// for var in ws.all_global_variables {
	// 	emit_variable(&vm, var);
	// }
}

emit_procedure :: proc(vm: ^VM, procedure: ^Ast_Proc) {
	cur_offset: uint;
	for var in procedure.vars {
		var.offset_in_stack_frame = cur_offset;
		cur_offset = align_forward(cur_offset + var.type.packed_size, VM_ALIGNMENT);
	}

	label(vm, procedure.name);
	logln(procedure);
	logln(procedure.block);
	logln(procedure.block.stmts);
	for stmt in procedure.block.stmts {
		switch kind in &stmt.derived {
			case Ast_Var:           continue;
			case: unhandledcase(kind);
		}
	}
}

emit_variable :: proc(vm: ^VM, var: ^Ast_Var) {
	// reg := alloc_register(vm, var.type.size);
}

alloc_register :: proc(vm: ^VM, size: uint) {

}

// is_power_of_two :: inline proc(x: int) -> bool {
// 	if x <= 0 do return false;
// 	return (x & (x-1)) == 0;
// }