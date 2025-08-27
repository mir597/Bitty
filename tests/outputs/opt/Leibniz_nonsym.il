let n;
n = 130000;
let p;
let _inl_p1;
_inl_p1 = n;
let _inl_v2;
_inl_v2 = 0;
let _inl_v3;
_inl_v3 = 0.0;
let _inl_v4;
_inl_v4 = 1.0;
while (_inl_v2 < _inl_p1) {
  let _inl_v5;
  __t0 = 2 * _inl_v2;
  _inl_v5 = __t0 + 1;
  __t1 = _inl_v4 * 1.0;
  __t2 = __t1 / _inl_v5;
  _inl_v3 = _inl_v3 + __t2;
  _inl_v4 = -_inl_v4;
  _inl_v2 = _inl_v2 + 1;
}
__t3 = 4.0 * _inl_v3;
p = __t3;
print p;
