__attribute__((noinline))
unsigned char f(unsigned long long i) { return i==5; }

__attribute__((noinline))
unsigned char g(unsigned long long i) { return i!=5; }

// to hopefully exercise the 'eqz' wasm instruction
__attribute__((noinline))
unsigned char h(unsigned long long i) { return i==0; }

unsigned char main() { return f(5) + g(10) + h(0); }
