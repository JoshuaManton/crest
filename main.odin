package crest

using import "core:fmt"
	  import "core:os"

using import "../storm"
using import "shared:workbench/logging"

main :: proc() {
	workspace: Workspace;

	parse_ok := parse_file(&workspace, os.args[1]);
	if !parse_ok {
		println("There were errors, exiting.");
		return;
	}

	check_ok := typecheck_workspace(&workspace);
	if !check_ok {
		println("There were errors, exiting.");
		return;
	}



	// gen_odin(&workspace);
	generate_and_execute_workspace(&workspace);
}

Workspace :: struct {
	global_scope: ^Ast_Block,
	current_scope: ^Ast_Block,

	unresolved_identifiers: [dynamic]^Ast_Identifier,
	nodes_to_typecheck: [dynamic]^Ast_Node,
	all_types: [dynamic]^Type,
	all_procedures: [dynamic]^Ast_Proc,
	all_global_variables: [dynamic]^Ast_Var,

	// ir_procedures: [dynamic]^IR_Procedure,
	// ir_global_fields: [dynamic]^IR_Field,
}

