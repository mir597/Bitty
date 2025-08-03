fn fib(n) {
  if (n <= 1) {
    return n;
  }
  __t0 = fib(n - 1);
  __t1 = fib(n - 2);
  return __t0 + __t1;
}
let n;
n = 22;
let r;
__t2 = fib(22);
r = __t2;
print r;
