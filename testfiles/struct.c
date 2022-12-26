typedef struct {
  unsigned short x, y;
} v2;

__attribute__((noinline))
unsigned short sq(v2* u) { return (u->x * u->x + u->y * u->y); }

unsigned short main() {
    v2 u = { 2, 1 };
    return sq(&u);
}
