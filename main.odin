package crest

using import "core:fmt"
	  import "core:os"

using import "shared:workbench/logging"

main :: proc() {
	workspace: Workspace;

	parse_ok := parse_workspace(&workspace, os.args[1]);
	if !parse_ok {
		logln("There were errors, exiting.");
		return;
	}

	check_ok := typecheck_workspace(&workspace);
	if !check_ok {
		logln("There were errors, exiting.");
		return;
	}

	gen_odin(&workspace);
}

Workspace :: struct {
	global_scope: ^Ast_Block,
	current_scope: ^Ast_Block,

	unresolved_identifiers: [dynamic]^Ast_Identifier,
	nodes_to_typecheck: [dynamic]^Ast_Node,
	all_types: [dynamic]^Type,
	all_procedures: [dynamic]^Ast_Proc,
}

