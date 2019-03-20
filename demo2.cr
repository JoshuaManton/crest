#include "print.cr"

const Hello = "Hello ";
const World = "World!";
const Hello_World = Hello + World;

const MyInt = int;

proc main() {
	print_string(Hello + World);

	var foo: int = 1;
	var other: MyInt = 2;
	print_int(foo + other);
}