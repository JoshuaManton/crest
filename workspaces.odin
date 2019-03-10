package crest

using import "core:fmt"
	  import "core:os"

using import "shared:workbench/logging"

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

	program_text: [dynamic]u8,
}
