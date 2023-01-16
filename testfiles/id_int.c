__attribute__((noinline))
unsigned short id_int(unsigned short i) { return i; }

unsigned short main() { return id_int(13); }
