__attribute__((noinline))
unsigned int p1(unsigned int x) { return x+1; }

unsigned int main() { return p1(p1(0)); }
