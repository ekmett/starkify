__attribute__((noinline))
int t3(int x) { return x*3; }

int main() { return t3(t3(5)); }
