package output

using import "core:fmt"

	some_included_proc :: proc() {
		println("I am in an included file!");
	}

		main :: proc() {
		basic_stuff();
		includes();
	}

	basic_stuff :: proc() {
		//
		{
						foo :: proc(value: f32, x: int) -> f32 {
				return value * 2;
			}

			println(foo(foo(1, 2), 2));
		}
		//
		some_ptr: ^int;
		some_int: int = 4;
		some_ptr = &some_int;
		//
		Vector2 :: struct {
			x: f32,
			y: f32,
		}

		i: int = 10;
		for i > 0 {
			vector_proc :: proc(v: Vector2) -> f32 {
				return v.x * 3.140;
			}

			i -= 1;
			vec: Vector2;
			vec.x = cast(f32)i;
			vec.y = vector_proc(vec);
		}
	}

	includes :: proc() {
		some_included_proc();
	}

