__attribute__((noinline))
void f(unsigned char* c) { *c = '?'; }

__attribute__((noinline))
void g(unsigned short* s) { *s = 120; }

unsigned short main() {
  unsigned char c = 'a';
  f(&c);
  unsigned short s = 12;
  g(&s);
  unsigned short cnew = (unsigned short)c;
  unsigned short r = cnew + s;
  return r;
}
