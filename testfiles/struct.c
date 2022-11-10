typedef struct {
  int x, y;
} v2;

/* __attribute__((noinline))
int gety(v2* u) { return u->y; } */
__attribute__((noinline))
int sq(v2* u) { return (u->x * u->x + u->y * u->y); }

int main() {
    v2 u = { 2, 1 };
    return sq(&u);
}
