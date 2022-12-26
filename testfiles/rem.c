__attribute__((noinline))
unsigned short rem_u(unsigned short u, unsigned short m) {
  return u % m;
}

unsigned short main() { return rem_u(100, 13); }
