package crest

Type_Ptr :: struct {
	ptr_to: ^Type,
}

Type_Array :: struct {
	length: uint,
	array_of: ^Type,
}

Type_List :: struct {
	list_of: ^Type,
}

Type_Slice :: struct {
	slice_of: ^Type,
}

Type_Untyped :: struct {
	name: string,
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
	fields: []string,
	types: []^Type,
	offsets: []u64, // note(josh): in bytes
}

Type :: struct {
	kind: union {
		Type_Primitive,
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
	id:           TypeID,
	packed_size:   uint,
	aligned_size:  uint,
	register_size: u64,
	flags:         u32,
	operators:     map[Operator]Operator_Info,
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

Operator_Info :: struct {
	result_type: ^Type,
	constant_evaluation_procedure: proc(Constant_Value, Constant_Value) -> Constant_Value,
	constant_only: bool,
}