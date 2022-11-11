__attribute__((noinline))
unsigned long f(unsigned long long i) { return i==5; }

__attribute__((noinline))
unsigned long g(unsigned long long i) { return i!=5; }

// to hopefully exercise the 'eqz' wasm instruction
__attribute__((noinline))
unsigned long h(unsigned long long i) { return i==0; }

unsigned long main() { return f(5) + g(10) + h(0); }
