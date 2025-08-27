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
let n;
n = 10000;
let s;
let _inl_p1;
_inl_p1 = n;
let _inl_v2;
_inl_v2 = 1;
let _inl_v3;
_inl_v3 = 0.0;
while (_inl_v2 <= _inl_p1) {
  __t2 = sqrt_newton(_inl_v2);
  _inl_v3 = _inl_v3 + __t2;
  _inl_v2 = _inl_v2 + 1;
}
__t3 = _inl_v3;
s = __t3;
print s;
