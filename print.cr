proc print_string(var str: string) {
	proc println(str: string) #odin;
	println(str);
}

proc print_int(var i: int) {
	proc println(i: int) #odin;
	println(i);
}

proc print_float(var f: float) {
	proc println(f: float) #odin;
	println(f);
}

proc print_array(var array: [10]int) {
	proc println(array: [10]int) #odin;
	println(array);
}