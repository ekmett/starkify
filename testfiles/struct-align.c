typedef struct {
    unsigned char c1;
    unsigned long long i;
    unsigned char c2;
} foo;

__attribute__((noinline))
void set_thing(foo* f) { f->c2 = 'z'; }

unsigned char main() {
    foo f = { 'a', 143, 'b' };
    set_thing(&f);
    return f.c2;
}
