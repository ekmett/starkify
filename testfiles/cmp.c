__attribute__((noinline))
unsigned short f1(unsigned short i) { return i<5; }

__attribute__((noinline))
unsigned short f2(unsigned short i) { return i<=5; }

__attribute__((noinline))
unsigned short f3(unsigned short i) { return i>5; }

__attribute__((noinline))
unsigned short f4(unsigned short i) { return i>=5; }

unsigned short main() { return f1(1) + f2(2) + f3(3) + f4(4); }
