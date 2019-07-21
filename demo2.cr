proc main() i16 {
	return factorial(6);
}

proc factorial(n: i16) i16 {
	if n == 1 {
		return 1;
	}
	return n * factorial(n-1);
}
