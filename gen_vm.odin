package crest

using import "shared:workbench/logging"
using import "../odin-projects/vm"

VM_ALIGNMENT :: 8;

generate_and_execute_workspace :: proc(ws: ^Workspace) {
	vm: VM;
	for procedure in ws.all_procedures {
		emit_procedure(&vm, procedure);
	}
	// for var in ws.all_global_variables {
	// 	emit_variable(&vm, var);
	// }
}

emit_procedure :: proc(vm: ^VM, procedure: ^Ast_Proc) {
	for stmt in procedure.block.stmts {
		switch kind in &stmt.derived {
			case Ast_Var: {

			}
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