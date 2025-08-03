fn sqrt_newton(x: Int): Float {
  if (x == 0) {
    return 0.0: Float;
  }
  let g: Float;
  g = x * 1.0: Float;
  let i: Int;
  i = 0: Int;
  while (i < 20: Bool) {
    __t0 = x / g: Float;
    __t1 = g + __t0: Float;
    g = 0.5 * __t1: Float;
    i = i + 1: Int;
  }
  return g: Float;
}
fn sqrt_sum(n: Int): Float {
  let i: Int;
  i = 1: Int;
  let s: Float;
  s = 0.0: Float;
  while (i <= n: Bool) {
    __t2 = sqrt_newton(i: Int);
    s = s + __t2: Float;
    i = i + 1: Int;
  }
  return s: Float;
}
let n: Int;
n = 10000: Int;
let s: Float;
__t3 = sqrt_sum(n: Int);
s = __t3: Float;
print s: Float;
