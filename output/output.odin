package output

using import "core:fmt"

type_id :: distinct int;

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

Hello: string : "Hello ";
World: string : "World!";
Hello_World: string : "Hello World!";
// demo2.cr(9:1)
main :: proc() {
	print_string("Hello World!");
	t: type_id = 23;
	foo: int = 1;
	other: int = 2;
	print_int(foo + other);
}

