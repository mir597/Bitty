let c;
c = 0;
fn inc() {
  c = c + 1;
  return true;
}
let __t0;
__t0 = false;
__t1 = inc();
if (!!false) {
  __t0 = !!__t1;
}
let __t2;
__t2 = true;
__t3 = inc();
if (!true) {
  __t2 = !!__t3;
}
print c;
