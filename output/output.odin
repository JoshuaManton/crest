package output

using import "core:fmt"

			slice_proc :: proc(slice: []f32) {
		// for x: float in slice {
		// 	println(x);
		// }
	}

	list_proc :: proc(list: [dynamic]f32) {
		s: string = "hello";
		s2: string;
		s2 = s;
		i: int = 0;
		for i < 10 {
			i += 1;
			println(i);
		}
	}

	Foo :: struct {
		x: int,
		z: bool,
	}

	Bar :: struct {
		foo: Foo,
		y: f32,
	}

	array_proc :: proc(array: [80]f32) {
	}

	main :: proc() {
		slice: []f32;
		list: [dynamic]f32;
		array: [80]f32;
		slice_proc(slice);
		list_proc(list);
		array_proc(array);
		append(&list, 1);
		append(&list, 2);
		append(&list, 3);
		// slice_proc(list[..]);
		// slice_proc(array[..]);
	}

