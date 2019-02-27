package crest

using import "core:fmt"
	  import "core:os"

main :: proc() {
	// preload: Workspace;
	// add_file_to_workspace(&preload, "modules/preload.cr");
	// code := compile(&preload);

	workspace: Workspace;
	workspace.output_file = "output/output.odin";

	add_file_to_workspace(&workspace, os.args[1]);

	compile(&workspace);
}
