package crest

Symbol :: struct {
	name: string,
	inferred_type: ^Type,
}

resolve_identifiers :: proc() -> bool {
	for ident in current_workspace.unresolved_identifiers {
		resolved := try_resolve_identifier(ident);
		if !resolved {
			unresolved_identifier(ident.base, ident.name);
			return false;
		}
	}
	return true;
}

try_resolve_identifier :: proc(ident: ^Ast_Identifier) -> bool {
	block := current_block;
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
	assert(current_block != nil);
	decl := new_clone(Symbol{name, inferred_type});
	append(&current_block.symbols, decl);
	return decl;
}

queue_identifier_for_resolving :: proc(ident: ^Ast_Identifier) {
	resolved := try_resolve_identifier(ident);
	if !resolved {
		append(&current_workspace.unresolved_identifiers, ident);
	}
}

