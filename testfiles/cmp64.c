__attribute__((noinline))
unsigned char f1(unsigned long long i) { return i<5; }

__attribute__((noinline))
unsigned char f2(unsigned long long i) { return i<=5; }

__attribute__((noinline))
unsigned char f3(unsigned long long i) { return i>5; }

__attribute__((noinline))
unsigned char f4(unsigned long long i) { return i>=5; }

unsigned char main() { return f1(1) + f2(2) + f3(3) + f4(4); }
