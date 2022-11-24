__attribute__((noinline))
unsigned int f1(unsigned long long i) { return i<5; }

__attribute__((noinline))
unsigned int f2(unsigned long long i) { return i<=5; }

__attribute__((noinline))
unsigned int f3(unsigned long long i) { return i>5; }

__attribute__((noinline))
unsigned int f4(unsigned long long i) { return i>=5; }

unsigned int main() { return f1(1) + f2(2) + f3(3) + f4(4); }
