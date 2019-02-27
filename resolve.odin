package crest

Symbol :: struct {
	name: string,
	inferred_type: ^Type,
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
		defer block = block.base.parent;

		for sym in block.symbols {
			if ident.name == sym.name {
				ident.sym = sym;
				return true;
			}
		}
	}
	return false;
}

create_symbol :: proc(block: ^Ast_Block, name: string, inferred_type : ^Type = nil, loc := #caller_location) -> ^Symbol {
	assert(block != nil);
	decl := new_clone(Symbol{name, inferred_type});
	append(&block.symbols, decl);
	return decl;
}

queue_identifier_for_resolving :: proc(ws: ^Workspace, ident: ^Ast_Identifier) {
	resolved := try_resolve_identifier(ident);
	if !resolved {
		append(&ws.unresolved_identifiers, ident);
	}
}

