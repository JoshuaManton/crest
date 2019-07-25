package crest

      import "../storm"

Ast_Typespec :: struct {
	using base: ^Ast_Node,
	completed_type: ^Type,
	kind: union {
		Typespec_Identifier,
		Typespec_Ptr,
		Typespec_Array,
		Typespec_List,
		Typespec_Slice,
		Typespec_Union,
	}
}

Typespec_Identifier :: struct {
	ident: ^Ast_Identifier,
}

Typespec_Ptr :: struct {
	typespec: ^Ast_Typespec,
}

Typespec_Array :: struct {
	length_expr: ^Ast_Node,
	typespec: ^Ast_Typespec,
}

Typespec_List :: struct {
	typespec: ^Ast_Typespec,
}

Typespec_Slice :: struct {
	typespec: ^Ast_Typespec,
}

Typespec_Union :: struct {
	types: []^Ast_Node,
}

Ast_Type_Expression :: struct {
	using base: ^Ast_Node,
	typespec: ^Ast_Typespec,
}

Ast_Directive :: struct {
	using base: ^Ast_Node,
	directive: string,
}

Ast_Directive_Include :: struct {
	using base: ^Ast_Node,
	filename: string,
}
Ast_Directive_Assert :: struct {
	using base: ^Ast_Node,
	condition: ^Ast_Node,
}

Ast_Comment :: struct {
	using base: ^Ast_Node,
	text: string,
}

Ast_Proc :: struct {
	using base: ^Ast_Node,
	name: string,
	params: []^Ast_Var,
	return_typespec: ^Ast_Typespec,
	flags: u32,
	block: ^Ast_Block,

	signature_type: ^Type,
	return_type: ^Type,
	declaration: ^Declaration,

	variables: [dynamic]^Ast_Var,
	output_name: string,

	stack_frame_size: u64,
	return_address_reg: Register_Allocation,
	caller_frame_pointer_reg: Register_Allocation,
	registers_in_use: [dynamic]Register_Allocation,

	is_main: bool,
}

Ast_Var :: struct {
	using base: ^Ast_Node,
	name: string,
	typespec: ^Ast_Typespec,
	expr: ^Ast_Node,
	declaration: ^Declaration,
	type: ^Type,

	is_constant: bool,
	is_local: bool,
	is_typedef: bool,

	offset_in_stack_frame: u64,
	// active_register: Maybe(Register_Allocation),
}

Ast_Struct :: struct {
	using base: ^Ast_Node,
	name: string,
	fields: []^Ast_Var,
	declaration: ^Declaration,
	struct_type: ^Type,
}

Ast_Typedef :: struct {
	using base: ^Ast_Node,
	name: string,
	other: ^Ast_Typespec,
	declaration: ^Declaration,
}

Ast_Identifier :: struct {
	using base: ^Ast_Node,
	name: string,
	declaration: ^Declaration,
}

Ast_Assign :: struct {
	using base: ^Ast_Node,
	op: Operator,
	lhs: ^Ast_Node,
	rhs: ^Ast_Node,
}

Ast_If :: struct {
	using base: ^Ast_Node,
	condition: ^Ast_Node,
	block: ^Ast_Block,
	else_ifs: []^Ast_Else_If,
	else_block: ^Ast_Block,
}
Ast_Else_If :: struct {
	using base: ^Ast_Node,
	condition: ^Ast_Node,
	block: ^Ast_Block,
}

Ast_For_I :: struct {
	using base: ^Ast_Node,
	var: ^Ast_Var,
	condition: ^Ast_Node,
	post_stmt: ^Ast_Node,
	block: ^Ast_Block,
}
// Ast_For_Each :: struct {
// 	using base: ^Ast_Node,
// 	var: ^Ast_Var,
// 	array: ^Ast_Node,
// 	block: ^Ast_Block,
// }
Ast_While :: struct {
	using base: ^Ast_Node,
	condition: ^Ast_Node,
	block: ^Ast_Block,
}

Ast_Return :: struct {
	using base: ^Ast_Node,
	procedure: ^Ast_Proc,
	expr: ^Ast_Node,
}

Ast_Block :: struct {
	using base: ^Ast_Node,
	stmts: [dynamic]^Ast_Node,
	declarations: [dynamic]^Declaration,
}

Ast_Call :: struct {
	using base: ^Ast_Node,
	procedure: ^Ast_Node,
	args: []^Ast_Node,
}

Ast_Unary :: struct {
	using base: ^Ast_Node,
	op: Operator,
	rhs: ^Ast_Node,
}

Ast_Binary :: struct {
	using base: ^Ast_Node,
	op: Operator,
	lhs: ^Ast_Node,
	rhs: ^Ast_Node,
}

Ast_Cast :: struct {
	using base: ^Ast_Node,
	typespec: ^Ast_Typespec,
	rhs: ^Ast_Node,
}

Ast_Sizeof :: struct {
	using base: ^Ast_Node,
	typespec: ^Ast_Typespec,
}

Ast_Subscript :: struct {
	using base: ^Ast_Node,
	lhs: ^Ast_Node,
	index: ^Ast_Node,
}

Ast_Selector :: struct {
	using base: ^Ast_Node,
	lhs: ^Ast_Node,
	field: string,
}

Ast_Paren :: struct {
	using base: ^Ast_Node,
	nested_expr: ^Ast_Node,
}

Ast_String :: struct {
	using base: ^Ast_Node,
	text: string,
}

Ast_Number :: struct {
	using base: ^Ast_Node,
	int_value:   i64,
	uint_value:  u64,
	float_value: f64,
	has_a_dot:   bool,
}

Ast_Null :: struct {
	using base: ^Ast_Node,
}

Ast_Node :: struct {
	derived: union {
		Ast_Directive_Include,
		Ast_Directive_Assert,
		Ast_Struct,
		Ast_Proc,
		Ast_Var,
		Ast_Directive,
		Ast_Comment,
		Ast_Assign,
		Ast_If,
		Ast_Else_If,
		Ast_For_I,
		// Ast_For_Each,
		Ast_While,
		Ast_Return,
		Ast_Block,

		Ast_Identifier,
		Ast_Call,
		Ast_Unary,
		Ast_Binary,
		Ast_Cast,
		Ast_Sizeof,
		Ast_Subscript,
		Ast_Selector,
		Ast_Paren,
		Ast_String,
		Ast_Number,
		Ast_Null,

		Ast_Typedef,

		Ast_Typespec,
		Ast_Type_Expression,
	},

	serial: int,
	parent: ^Ast_Block,
	root_token: Token,
	check_state: Check_State,
	depends: [dynamic]^Ast_Node,
	expr_data: Expr_Data,
	parent_procedure: ^Ast_Proc,
	do_not_print: bool,
}

Depend_Entry :: struct {
	node: ^Ast_Node,
	depends_on: ^Ast_Node,
}

Constant_Value :: union {
	i64,
	u64,
	f64,
	bool,
	string,
	TypeID,
}

Expr_Data :: struct {
	type: ^Type,
	mode: Addressing_Mode,
	constant_value: Constant_Value,
}

Addressing_Mode :: enum {
	Invalid,
	RValue,
	LValue,
	No_Value,
	Constant,
}



Pointer_To_Type :: ^Type; // todo(josh): remove, this was for an odin bug a while back

TypeID :: distinct int;



Site :: struct {
	filename: string,
	line: int,
	column: int,
}

Maybe :: union(T: typeid) {
	T,
}