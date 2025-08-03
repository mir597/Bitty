fn tak(x, y, z) {
  if (y < x) {
    __t0 = tak(x - 1, y, z);
    __t1 = tak(y - 1, z, x);
    __t2 = tak(z - 1, x, y);
    __t3 = tak(__t0, __t1, __t2);
    return __t3;
  } else {
    return z;
  }
}
let r;
__t4 = tak(18, 12, 6);
r = __t4;
print r;
