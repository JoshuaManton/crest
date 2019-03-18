package output

using import "core:fmt"

// print.cr(1:1)
print_string :: proc(str: string) {
	println(str);
}

// print.cr(6:1)
print_int :: proc(i: i32) {
	println(i);
}

// print.cr(11:1)
print_float :: proc(f: f32) {
	println(f);
}

// print.cr(16:1)
print_array :: proc(array: [10]i32) {
	println(array);
}

// demo.cr(3:1)
main :: proc() {
	procedures();
	variables();
	constants();
	loops();
	structs();
	arrays();
}

// demo.cr(12:1)
procedures :: proc() {
	// demo.cr(13:5)
	sqr :: proc(value: f32) -> f32 {
		return value * value;
	}

	print_float(sqr(3) * sqr(5));
}

// demo.cr(20:1)
variables :: proc() {
	some_ptr: ^i32;
	some_int: i32 = 4;
	some_ptr = &some_int;
}

// demo.cr(26:1)
constants :: proc() {
	HENLO: string : "Henlo ";
	WORLD: string : "World!";
	HENLO_WORLD: string : HENLO + WORLD;
}

// demo.cr(33:1)
loops :: proc() {
	i: i32 = 10;
	for i > 0 {
		i -= 1;
	}
}

// demo.cr(40:1)
structs :: proc() {
	// demo.cr(41:5)
	Vector2 :: struct {
		x: f32,
		y: f32,
	}

	// demo.cr(46:5)
	vector_proc :: proc(v: Vector2) -> f32 {
		return v.x * 3.140;
	}

	vec: Vector2;
	vec.x = 3;
	vec.y = vector_proc(vec);
	// demo.cr(54:5)
	Other_Vector2 :: distinct Vector2;
	other: Other_Vector2;
	other.x = 4;
	other.y = 7;
	// other = vec; <- type mismatch
}

// demo.cr(61:1)
arrays :: proc() {
	N: i32 : 16;
	arr: [16]f32;
	arr[2] = 123;
	print_float(arr[0]);
	print_float(arr[2]);
}

