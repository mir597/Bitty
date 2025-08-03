let x;
__t0 = 2 * 3;
x = 1 + __t0;
let y;
y = x + 4;
__t1 = y * 2;
y = x + __t1;
x = x + __t1;
if (x == y) {
  y = y + 1;
} else {
  while (x + 2 < 10) {
    x = x + 1;
  }
}
return x;
