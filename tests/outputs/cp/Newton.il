fn sqrt_newton(x) {
  if (x == 0) {
    return 0.0;
  }
  let g;
  g = x * 1.0;
  let i;
  i = 0;
  while (i < 20) {
    __t0 = x / g;
    __t1 = g + __t0;
    g = 0.5 * __t1;
    i = i + 1;
  }
  return g;
}
fn sqrt_sum(n) {
  let i;
  i = 1;
  let s;
  s = 0.0;
  while (i <= n) {
    __t2 = sqrt_newton(i);
    s = s + __t2;
    i = i + 1;
  }
  return s;
}
let n;
n = 10000;
let s;
__t3 = sqrt_sum(10000);
s = __t3;
print s;
