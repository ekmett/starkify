__attribute__((noinline))
unsigned short f(unsigned short i) { return i==5; }

__attribute__((noinline))
unsigned short g(unsigned short i) { return i!=5; }

// to exercise the 'eqz' wasm instruction
__attribute__((noinline))
unsigned short h(unsigned short i) { return i==0; }

unsigned short main() { return f(10) + g(20); }
