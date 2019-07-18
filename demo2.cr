proc main(arg: i16) i16 {
	return factorial(arg);
}

proc factorial(n: i16) i16 {
	if n == 1 {
		return 1;
	}
	return n * factorial(n-1);
}
