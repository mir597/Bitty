let x: Int;
__t0 = 2 * 3: Int;
x = 1 + __t0: Int;
let y: Int;
y = x + 4: Int;
__t1 = y * 2: Int;
y = x + __t1: Int;
x = x + __t1: Int;
if (x == y) {
  y = y + 1: Int;
} else {
  while (x + 2 < 10: Bool) {
    x = x + 1: Int;
  }
}
return x: Int;
