package crest

using import "core:fmt"
	  import "core:os"

using import "shared:workbench/logging"

main :: proc() {
	workspace: Workspace;
	workspace.output_file = "output/output.odin";

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
	workspace_name: string,
	output_file: string,
	global_scope: ^Ast_Block,
	current_scope: ^Ast_Block,

	all_files: [dynamic]string,

	unresolved_identifiers: [dynamic]^Ast_Identifier,

	all_depends: [dynamic]Depend_Entry,
	nodes_to_typecheck: [dynamic]^Ast_Node,
	all_types: [dynamic]^Type,
}
