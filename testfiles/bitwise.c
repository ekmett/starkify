__attribute__((noinline))
int f1(int i) { return i << 5; }

__attribute__((noinline))
int f2(int i) { return i >> 5; }

__attribute__((noinline))
int f3(int i) { return i & 5; }

__attribute__((noinline))
int f4(int i) { return i | 5; }

__attribute__((noinline))
int f5(int i) { return i ^ 5; }

int main() { return f1(1) + f2(2) + f3(3) + f4(4) + f5(5); }
