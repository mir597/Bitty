fn f(x) {
  if (x < 0) {
    x = 0;
  } else {
    while (x < 10) {
      x = x + 1;
    }
  }
  return x;
}
