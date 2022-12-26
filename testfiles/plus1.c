__attribute__((noinline))
unsigned short p1(unsigned short x) { return x+1; }

unsigned short main() { return p1(p1(0)); }
