fn pi_leibniz(n: Int): Float {
  let k: Int;
  k = 0: Int;
  let s: Float;
  s = 0.0: Float;
  let sign: Float;
  sign = 1.0: Float;
  while (k < n: Bool) {
    let denom;
    __t0 = 2 * k: Int;
    denom = __t0 + 1: Int;
    __t1 = sign * 1.0: Float;
    __t2 = __t1 / denom: Float;
    s = s + __t2: Float;
    sign = -sign: Float;
    k = k + 1: Int;
  }
  return 4.0 * s: Float;
}
let n: Int;
n = 130000: Int;
let p: Float;
__t3 = pi_leibniz(n: Int);
p = __t3: Float;
print p: Float;
