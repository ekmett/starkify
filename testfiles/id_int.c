__attribute__((noinline))
unsigned int id_int(unsigned int i) { return i; }

unsigned int main() { return id_int(13); }
