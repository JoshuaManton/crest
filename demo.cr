#include "print.cr"

proc main() {
	procedures();
	variables();
	constants();
	loops();
	structs();
	arrays();
}

proc procedures() {
	proc sqr(var value: float) float {
		return value * value;
	}

	print_float(sqr(3) * sqr(5));
}

proc variables() {
	var some_ptr: ^int;
	var some_int = 4;
	some_ptr = &some_int;
}

proc constants() {
	const HENLO = "Henlo, ";
	const WORLD = "World!";
	const HENLO_WORLD = HENLO + WORLD;
	#assert HENLO_WORLD == "Henlo, World!"
}

proc loops() {
	var i = 10;
	while i > 0 {
		i -= 1;
	}
}

proc structs() {
	struct Vector2 {
		var x: float;
		var y: float;
	}

	proc vector_proc(v: Vector2) float {
		return v.x * 3.14;
	}

	var vec: Vector2;
	vec.x = 3;
	vec.y = vector_proc(vec);
}

proc arrays() {
	var arr: [16]float;
	arr[2] = 123;
	print_float(arr[0]);
	print_float(arr[2]);
}