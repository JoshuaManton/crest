package crest

using import "shared:workbench/logging"

Declaration :: struct {
	name: string,
	constant_value: Constant_Value,
	kind: Declaration_Kind,
}

Declaration_Kind :: union {
	Type_Decl,
	Proc_Decl,
	Var_Decl,
}

Type_Decl :: struct { // structs and typedefs
	type: ^Type,
}
Proc_Decl :: struct {
	type: ^Type,
	procedure: ^Ast_Proc,
}
Var_Decl :: struct {
	type: ^Type,
	var: ^Ast_Var,
}

resolve_identifiers :: proc(ws: ^Workspace) -> bool {
	for ident in ws.unresolved_identifiers {
		resolved := try_resolve_identifier(ident);
		if !resolved {
			error(ident.base, "Unresolved identifier: ", ident.name);
			return false;
		}
	}
	return true;
}

try_resolve_identifier :: proc(ident: ^Ast_Identifier) -> bool {
	block := ident.base.parent;
	for block != nil {
		decl := try_find_declaration_in_block(block, ident.name);
		if decl != nil {
			ident.declaration = decl;
			return true;
		}
		block = block.base.parent;
	}
	return false;
}

try_find_declaration_in_block :: proc(block: ^Ast_Block, name: string) -> ^Declaration {
	for sym in block.declarations {
		if sym.name == name {
			return sym;
		}
	}
	return nil;
}

create_declaration :: proc(block: ^Ast_Block, name: string, type: ^Type = nil, loc := #caller_location) -> ^Declaration {
	assert(block != nil);

	decl := new_clone(Declaration{name, nil, nil});
	if type != nil {
		decl.kind = Type_Decl{type};
		decl.constant_value = type.id;
	}
	append(&block.declarations, decl);
	return decl;
}

complete_declaration :: inline proc(decl: ^Declaration, kind: $T) {
	decl.kind = kind;
}

queue_identifier_for_resolving :: proc(ws: ^Workspace, ident: ^Ast_Identifier) {
	append(&ws.unresolved_identifiers, ident);
}

