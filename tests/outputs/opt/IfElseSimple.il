let x;
x = 0;
let _cse1;
_cse1 = x < 10;
if (_cse1) {
  x = x + 1;
}
if (_cse1) {
  x = x + 1;
} else {
  x = x - 1;
}
if (x < 0) {
  x = 0;
} else {
  x = x + 2;
}
