package output

using import "core:fmt"

// #include "print.cr"
foo: f32;
Hello: string : "Hello, ";
World: string : "World!";
Hello_World: string : Hello + World;
// demo2.cr(8:1)
main :: proc() {
	// print_string(Hello + World);
}

