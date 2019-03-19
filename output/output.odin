package output

using import "core:fmt"

// print.cr(1:1)
print_string :: proc(str: string) {
	println(str);
}

// print.cr(6:1)
print_int :: proc(i: int) {
	println(i);
}

// print.cr(11:1)
print_float :: proc(f: f32) {
	println(f);
}

// print.cr(16:1)
print_array :: proc(array: [10]int) {
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
	some_ptr: ^int;
	some_int: int = 4;
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
	i: int = 10;
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

	vec: Vector2;
	vec.x = 1;
	vec.y = 4;
	// type alias
	a: int = 5;
	b: ^int = &a;
	print_int(b^);
	// demo.cr(56:5)
	Other_Vector2 :: distinct Vector2;
	// distinct type
	x: Vector2;
	y: Other_Vector2;
	// y = x; <- type mismatch
}

// demo.cr(63:1)
arrays :: proc() {
	N: int : 16;
	arr: [16]f32;
	arr[2] = 123;
	print_float(arr[0]);
	print_float(arr[2]);
}

