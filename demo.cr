#include "print.cr" // `#includes` are currently textual but I think I'd prefer a semantic include in the future

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
	const HENLO = "Henlo ";
	const WORLD = "World!";
	const HENLO_WORLD = HENLO + WORLD;
	#assert HENLO_WORLD == "Henlo World!"
}

proc loops() {
	var i = 10;
	while i > 0 {
		i -= 1;
	}
	// todo: c-style for loop and foreach
}

proc structs() {
	type Vector2 {
		var x: float;
		var y: float;
	}

	var vec: Vector2;
	vec.x = 1.0;
	vec.y = 4.0;

	const MyInt = int; // type alias
	#assert MyInt == int
	var a: int = 5;
	var b: ^MyInt = &a;
	print_int(^b);

	type Other_Vector2 Vector2; // distinct type
	#assert Other_Vector2 != Vector2
	var x: Vector2;
	var y: Other_Vector2;
	// y = x; <- type mismatch
}

proc arrays() {
	const N = 16;
	var arr: [N]float;
	arr[0] = 76;
	arr[2] = 123;
	print_float(arr[0]);
	print_float(arr[2]);
}