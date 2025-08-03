fn tak(x: Int, y: Int, z: Int): Int {
  if (y < x) {
    __t0 = tak(x - 1: Int, y: Int, z: Int);
    __t1 = tak(y - 1: Int, z: Int, x: Int);
    __t2 = tak(z - 1: Int, x: Int, y: Int);
    __t3 = tak(__t0: Int, __t1: Int, __t2: Int);
    return __t3: Int;
  } else {
    return z: Int;
  }
}
let r: Int;
__t4 = tak(18: Int, 12: Int, 6: Int);
r = __t4: Int;
print r: Int;
