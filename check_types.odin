package crest

Type_Ptr :: struct {
	ptr_to: ^Type,
}

Type_Array :: struct {
	length: u64,
	array_of: ^Type,
}

Type_List :: struct {
	list_of: ^Type,
}

Type_Slice :: struct {
	slice_of: ^Type,
}

Type_Untyped :: struct {
	base: ^Type,
}

// @UnionTypes
// Type_Union :: struct {
// 	types: [dynamic]^Type,
// }

Field :: struct {
	name: string,
	inferred_type: ^Type,
}

Type_Integer :: struct {
	signed: bool,
}

Type_Float :: struct {

}

Type_Bool :: struct {

}

Type_Proc :: struct {
	params: []Field,
	return_type: ^Type,
}

Type_Struct :: struct {
	fields: []string,
	types: []^Type,
	offsets: []u64, // note(josh): in bytes
}

Type :: struct {
	kind: union {
		Type_Integer,
		Type_Float,
		Type_Bool,
		Type_Struct,
		Type_Ptr,
		Type_Array,
		Type_List,
		Type_Slice,
		Type_Untyped,

		// @UnionTypes
		// Type_Union,

		Type_Proc,
	},
	id:    TypeID,
	name: string,
	size:  u64,
	flags: u32,

	binary_operators: map[Operator]Binary_Operator_Info,
	unary_operators:  map[Operator]Unary_Operator_Info,
	constant_conversions: map[^Type]proc(Constant_Value) -> Constant_Value,
}

Check_State :: enum {
	Unchecked,
	// Checking,
	Checked,
}

Type_Flags :: enum u32 {
	Number    = 1 << 0,
	Integer   = 1 << 1,
	Signed    = 1 << 2,
	Unsigned  = 1 << 3,
	Float     = 1 << 4,
	Pointer   = 1 << 5,
	Procedure = 1 << 6,
	Untyped   = 1 << 7,
}

Binary_Operator_Info :: struct {
	result_type: ^Type,
	constant_evaluation_procedure: proc(Constant_Value, Constant_Value) -> Constant_Value,
	constant_only: bool,
}

Unary_Operator_Info :: struct {
	result_type: ^Type,
	constant_evaluation_procedure: proc(Constant_Value) -> Constant_Value,
}