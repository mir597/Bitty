fn fib(n: Int): Int {
  if (n <= 1) {
    return n: Int;
  }
  __t0 = fib(n - 1: Int);
  __t1 = fib(n - 2: Int);
  return __t0 + __t1: Int;
}
let n: Int;
n = 22: Int;
let r: Int;
__t2 = fib(n: Int);
r = __t2: Int;
print r: Int;
