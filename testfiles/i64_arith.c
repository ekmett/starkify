__attribute__((noinline))
unsigned long long add(unsigned long long i, unsigned long long j) {
  return i+j;
}
__attribute__((noinline))
unsigned long long sub(unsigned long long i, unsigned long long j) {
  return i-j;
}
__attribute__((noinline))
unsigned long long mul(unsigned long long i, unsigned long long j) {
  return i*j;
}

unsigned long long main() { return add(8, 2) + sub(5, 1) + mul(2,15); }
