package crest

Symbol :: struct {
	name: string,
	constant_value: Constant_Value,

	// what kind of declaration this symbol refers to
	decl_type: union {
		Type_Decl,
		Proc_Decl,
		Var_Decl,
	},
}

Type_Decl :: struct { // structs and typedefs
	type: ^Type,
}
Proc_Decl :: struct {
	type: ^Type,
}
Var_Decl :: struct {
	type: ^Type,
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

create_symbol :: proc(block: ^Ast_Block, name: string, type: ^Type = nil, loc := #caller_location) -> ^Symbol {
	assert(block != nil);

	symbol := new_clone(Symbol{name, nil, nil});
	// built-in types dont have a declaration
	if type != nil {
		symbol.decl_type = Type_Decl{type};
		symbol.constant_value = type.id;
	}
	append(&block.symbols, symbol);
	return symbol;
}

complete_sym :: inline proc(sym: ^Symbol, decl: $T) {
	sym.decl_type = decl;
}

queue_identifier_for_resolving :: proc(ws: ^Workspace, ident: ^Ast_Identifier) {
	resolved := try_resolve_identifier(ident);
	if !resolved {
		append(&ws.unresolved_identifiers, ident);
	}
}

