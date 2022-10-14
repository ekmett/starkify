__attribute__((noinline))
int p1(int x) { return x+1; }

int main() { return p1(p1(0)); }
