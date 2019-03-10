package crest

using import "core:fmt"
	  import "core:os"

using import "shared:workbench/logging"

main :: proc() {
	_workspace: Workspace;
	workspace := &_workspace;
	workspace.output_file = "output/output.odin";

	parse_ok := parse_workspace(workspace, os.args[1]);
	if !parse_ok {
		logln("There were errors, exiting.");
		return;
	}

	check_ok := typecheck_workspace(workspace);
	if !check_ok {
		logln("There were errors, exiting.");
		return;
	}

	gen_odin(workspace);
}
