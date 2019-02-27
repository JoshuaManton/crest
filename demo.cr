#include "demo_other.cr"

proc main() {
	basic_stuff();
	includes();
}

proc basic_stuff() {
	proc sqr(var value: float) float {
		return value * value;
	}
	print_float(sqr(3) * sqr(5));

	//
	var some_ptr: ^int;
	var some_int = 4;
	some_ptr = &some_int;

	//
	var i = 10;
	while i > 0 {
		i -= 1;
	}

	var vec: Vector2;
	vec.x = cast(float)i;
	vec.y = vector_proc(vec);
}


proc includes() {
	some_included_proc();
}



struct Vector2 {
	var x: float;
	var y: float;
}

proc vector_proc(var v: Vector2) float {
	return v.x * 3.14;
}

proc print_string(var str: string) {
	proc println(str: float) #odin;
	println(str);
}

proc print_float(var f: float) {
	proc println(f: float) #odin;
	println(f);
}