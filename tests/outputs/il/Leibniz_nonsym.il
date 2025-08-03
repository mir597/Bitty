fn pi_leibniz(n) {
  let k;
  k = 0;
  let s;
  s = 0.0;
  let sign;
  sign = 1.0;
  while (k < n) {
    let denom;
    __t0 = 2 * k;
    denom = __t0 + 1;
    __t1 = sign * 1.0;
    __t2 = __t1 / denom;
    s = s + __t2;
    sign = -sign;
    k = k + 1;
  }
  return 4.0 * s;
}
let n;
n = 130000;
let p;
__t3 = pi_leibniz(n);
p = __t3;
print p;
