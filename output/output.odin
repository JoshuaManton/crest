package output

using import "core:fmt"

	some_included_proc :: proc() {
		print_string("I am in an included file!");
		print_string("I can use procs in the other file!");
	}

	main :: proc() {
		basic_stuff();
		includes();
	}

	basic_stuff :: proc() {
		sqr :: proc(value: f32) -> f32 {
			return value * value;
		}

		print_float(sqr(3) * sqr(5));
		//
		{
			some_ptr: ^u32;
			some_int: u32 = 4;
			some_ptr = &some_int;
		}
		//
		{
			i: u32 = 10;
			for i > 0 {
				i -= 1;
			}
		}
		//
		{
			vec: Vector2;
			vec.x = 3;
			vec.y = vector_proc(vec);
		}
		//
		{
			arr: [64]f32;
			arr[2] = 123;
			print_float(arr[0]);
			print_float(arr[2]);
		}
	}

	includes :: proc() {
		some_included_proc();
	}

	Vector2 :: struct {
		x: f32,
		y: f32,
	}

	vector_proc :: proc(v: Vector2) -> f32 {
		return v.x * 3.140;
	}

	print_string :: proc(str: string) {
				println(str);
	}

	print_float :: proc(f: f32) {
				println(f);
	}

