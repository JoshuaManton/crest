proc main() {
	var x: int = foo(8, 9) + foo(foo(8, 9), 3);
}

proc foo(a: int, b: int) int {
	return bar(a, b);
}

proc bar(a: int, b: int) int {
	return a * b;
}
