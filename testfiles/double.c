__attribute__((noinline))
unsigned int dbl(unsigned int x) { return 2*x; }

unsigned int main()
{
    unsigned int x = 23;
    return dbl(x);
}
