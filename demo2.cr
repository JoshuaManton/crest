struct Vector2 {
	var x: float;
	var y: float;
}

proc main() {
	var aa: Vector2;
	aa.x = 5;
	aa.y = 2;

	var bb: Vector2;
	bb.x = 3;
	bb.y = 1;

	var vec = add(aa, add(aa, bb));
}

proc add(v1: Vector2, v2: Vector2) Vector2 {
	var result: Vector2;
	result.x = v1.x + v2.x;
	result.y = v1.y + v2.y;
	return result;
}
