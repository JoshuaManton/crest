#include "print.cr"

const Hello = "Hello ";
const World = "World!";
const Hello_World = Hello + World;

const Ptr_To_Int = #type ^int;

proc main() {
	var foo: int;
	var other: Ptr_To_Int;
	other = &foo;
	foo = 1;
	print_int(^other);
	// print_string(Hello + World);
}