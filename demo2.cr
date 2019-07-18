proc main() int {
	var a = 12 * 2;
	var b = foo(a);
	return b;
}

proc foo(a: int) int {
	return a + a;
}