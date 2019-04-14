package crest

Type_Ptr :: struct {
	ptr_to: ^Type,
}

Type_Array :: struct {
	length: uint,
	array_of: ^Type,
}

Type_Dynamic_Array :: struct {
	array_of: ^Type,
}

Type_Slice :: struct {
	slice_of: ^Type,
}

// @UnionTypes
// Type_Union :: struct {
// 	types: [dynamic]^Type,
// }

Field :: struct {
	name: string,
	inferred_type: ^Type,
}

Type_Primitive :: struct {
	name: string,
}

Type_Proc :: struct {
	params: []Field,
	return_type: ^Type,
}

Type_Struct :: struct {
	name: string,
	fields: []Field,
}

Type :: struct {
	kind: union {
		Type_Primitive,
		Type_Struct,
		Type_Ptr,
		Type_Array,
		Type_Dynamic_Array,
		Type_Slice,

		// @UnionTypes
		// Type_Union,

		Type_Proc,
	},
	id:   TypeID,
	aligned_size: uint,
	operators: map[Operator]Operator_Info,
}

Check_State :: enum {
	Unchecked,
	// Checking,
	Checked,
}

Operator_Info :: struct {
	result_type: ^Type,
	constant_evaluation_procedure: proc(Constant_Value, Constant_Value) -> Constant_Value,
	constant_only: bool,
}
