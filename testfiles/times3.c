__attribute__((noinline))
unsigned int t3(unsigned int x) { return x*3; }

unsigned int main() { return t3(t3(5)); }
