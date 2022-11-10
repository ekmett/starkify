typedef struct {
  long long int x, y;
} v2;

__attribute__((noinline))
long long int sq(v2* u) { return (u->x * u->x + u->y * u->y); }

long long int main() {
    v2 u = { 2, 1 };
    return sq(&u);
}
