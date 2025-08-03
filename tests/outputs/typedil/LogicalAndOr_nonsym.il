let c: Int;
c = 0: Int;
fn inc(): Bool {
  c = c + 1;
  return true: Bool;
}
let __t0: Bool;
__t0 = false: Bool;
__t1 = inc();
if (!!false) {
  __t0 = !!__t1: Bool;
}
let __t2: Bool;
__t2 = true: Bool;
__t3 = inc();
if (!true) {
  __t2 = !!__t3: Bool;
}
print c: Int;
