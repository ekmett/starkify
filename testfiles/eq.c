__attribute__((noinline))
int f(int i) { return i==5; }

__attribute__((noinline))
int g(int i) { return i!=5; }

// to exercise the 'eqz' wasm instruction
__attribute__((noinline))
int h(int i) { return i==0; }


int main() { return f(10) + g(20); }
