typedef struct {
  unsigned long long x, y;
} v2;

__attribute__((noinline))
unsigned long long sq(v2* u) { return (u->x * u->x + u->y * u->y); }

unsigned long long main() {
    v2 u = { 2, 1 };
    return sq(&u);
}
