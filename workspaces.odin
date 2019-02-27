package crest

using import "core:fmt"
	  import "core:os"

using import "shared:workbench/logging"

Workspace :: struct {
	name:        string,
	output_file: string,
	syntax_tree: ^Ast_Block,

	all_files: [dynamic]string,

	unresolved_identifiers: [dynamic]^Ast_Identifier,

	all_depends: [dynamic]Depend_Entry,
	nodes_to_typecheck: [dynamic]^Ast_Node,
	all_types: [dynamic]^Type,

	program_text: [dynamic]u8,
}

add_string_to_workspace :: proc(wb: ^Workspace, args: ..string) {
	for arg in args {
		append_string(&wb.program_text, arg);
	}
	append_string(&wb.program_text, "\n\n");
}

add_file_to_workspace :: inline proc(ws: ^Workspace, filename: string) {
	add_string_to_workspace(ws, "#include \"", filename, "\"\n");
}

compile :: inline proc(ws: ^Workspace) {
	ok := parse_workspace(ws, "\"<FILENAME>\"", cast(string)ws.program_text[:]);
	if !ok {
		logln("There were errors, exiting.");
		return;
	}

	gen_odin(ws);
}