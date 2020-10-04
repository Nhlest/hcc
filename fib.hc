func fib(n: i32): i32 {
	local a: i32 = 0;
	local b: i32 = 1;
	while (n > 0) {
		local c: i32 = a + b;
		a = b;
		b = c;
		n -= 1;
	}
	a;
}
func square(n: i32): i32 {
  local nn: i32 = n;
  local s: i32 = 0;
  while (n > 0) {
    s = s + nn;
    n -= 1;
  }
  s;
}
