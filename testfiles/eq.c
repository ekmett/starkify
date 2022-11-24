__attribute__((noinline))
unsigned int f(unsigned int i) { return i==5; }

__attribute__((noinline))
unsigned int g(unsigned int i) { return i!=5; }

// to exercise the 'eqz' wasm instruction
__attribute__((noinline))
unsigned int h(unsigned int i) { return i==0; }

unsigned int main() { return f(10) + g(20); }
