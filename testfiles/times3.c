__attribute__((noinline))
unsigned short t3(unsigned short x) { return x*3; }

unsigned short main() { return t3(t3(5)); }
