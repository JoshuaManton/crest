#include "demo_other.cr"

proc println(str: string) #odin;

proc main() {
	basic_stuff();
	includes();
}

proc basic_stuff() {
	//
	{
		proc println(str: float) #odin;
		proc foo(var value: float, var x: int) float {
			return value * 2;
		}
		println(foo(foo(1, 2), 2));
	}


	//
	var some_ptr: ^int;
	var some_int = 4;
	some_ptr = &some_int;

	//
	struct Vector2 {
		var x: float;
		var y: float;
	}
	var i = 10;
	while i > 0 {
		proc vector_proc(var v: Vector2) float {
			return v.x * 3.14;
		}

		i -= 1;

		var vec: Vector2;
		vec.x = cast(float)i;
		vec.y = vector_proc(vec);
	}
}

proc includes() {
	some_included_proc();
}