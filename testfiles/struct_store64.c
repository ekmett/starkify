typedef struct {
  unsigned long long int x, y;
} v2;

/* __attribute__((noinline))
int gety(v2* u) { return u->y; } */
__attribute__((noinline))
void sety1(v2* u) { u->y = 1; }

long long int main() {
    v2 u = { 2, 3 };
    sety1(&u);
    return u.y;
}
