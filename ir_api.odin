package crest

using import "core:fmt"

IR_Entity :: struct {
	scope: ^IR_Scope,
	kind: union {
		IR_Scope,
		IR_Proc,
		IR_Variable,
	},
}

IR_Proc :: struct {
	base: ^IR_Entity,
	vars: [dynamic]^IR_Variable,
	is_entry_point: bool,
}

IR_Variable :: struct {
	base: ^IR_Entity,
	type: ^Type,
	active_register: Register,
}

IR_Scope :: struct {
	base: ^IR_Entity,
	parent: ^IR_Scope,
	procedures: [dynamic]^IR_Proc,
}

IR_Assign :: struct {
	result_register: int,
}

IR_Expression :: struct {
	kind: union {
		IR_Call,
		IR_Binary,
	},
}

IR_Call :: struct {

}

IR_Binary :: struct {
	lhs: ^IR_Variable,
	rhs: ^IR_Variable,
	op: IR_Op,
}

IR_Op :: enum {
	None,
	Add,
	Minus,
	Multiply,
	Divide,
	Mod,
	ModMod,
}

Register :: int;

make_ir_entity :: proc(scope: ^IR_Scope, _kind: $T) -> ^T {
	kind := _kind;
	assert(kind.base == nil, "dont set base for a parameter to make_ir_entity");
	e := new_clone(IR_Entity{scope, kind});
	e.scope = scope;
	k := &e.kind.(T);
	k.base = e;
	return k;
}



ir_procedure :: proc() -> ^IR_Proc {
	unimplemented();
	return {};
}

ir_variable :: proc(type: ^Type) -> ^IR_Variable {
	unimplemented();
	return {};
}

ir_add_proc_param :: proc(procedure: ^IR_Proc, variable: ^IR_Variable) {
	unimplemented();
}

ir_assign :: proc(procedure: ^IR_Proc, destination: ^IR_Variable, expr: ^IR_Entity) {
	unimplemented();
}

ir_call :: proc(procedure: ^IR_Proc, call: ^IR_Call) -> ^IR_Variable {
	unimplemented();
	return {};
}

ir_binop :: proc(procedure: ^IR_Proc, binary: ^IR_Binary) -> ^IR_Variable {
	lhs := ir_emit_expr(procedure, binary.lhs);
	rhs := ir_emit_expr(procedure, binary.rhs);
	#complete
	switch binary.op {
		case .Add:      unimplemented();
		case .Minus:    unimplemented();
		case .Multiply: unimplemented();
		case .Divide:   unimplemented();
		case .Mod:      unimplemented();
		case .ModMod:   unimplemented();
		case .None:     assert(false);

	}
	unreachable();
	return {};
}

test :: proc() {
	procedure := ir_procedure();
	px := ir_variable(type_int);
	py := ir_variable(type_int);
	ir_add_proc_param(procedure, px);
	ir_add_proc_param(procedure, py);

	ir_assign(procedure, px, ir_binop(procedure, ir_bin.Add, px, py).base);
	ir_return(procedure, temp);
}









ir_emit_expr :: proc(procedure: ^IR_Proc, expr: ^IR_Expression) -> ^IR_Variable {
	#complete
	switch kind in &expr.kind {
		case IR_Call:   return ir_emit_call(procedure, kind);
		case IR_Binary: return ir_emit_binop(procedure, kind);
		case: panic(tprint(kind));
	}
	unreachable();
	return {};
}

ir_emit_binop :: proc(procedure: ^IR_Proc, binary: ^IR_Binary) -> ^IR_Variable {
	lhs := ir_emit_expr(procedure, binary.lhs);
	rhs := ir_emit_expr(procedure, binary.rhs);
	#complete
	switch binary.op {
		case .Add:      unimplemented();
		case .Minus:    unimplemented();
		case .Multiply: unimplemented();
		case .Divide:   unimplemented();
		case .Mod:      unimplemented();
		case .ModMod:   unimplemented();
		case .None: assert(false);

	}
	unreachable();
	return {};
}