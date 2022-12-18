__attribute__((noinline))
unsigned int rem_u(unsigned int u, unsigned int m) {
  return u % m;
}

unsigned int main() { return rem_u(100, 13); }
