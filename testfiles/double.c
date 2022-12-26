__attribute__((noinline))
unsigned short dbl(unsigned short x) { return 2*x; }

unsigned short main()
{
    return dbl(23);
}
