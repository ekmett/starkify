__attribute__((noinline))
unsigned short f(unsigned short x) { return x+1; }

__attribute__((noinline))
unsigned short g(unsigned short x) { return x-1; }

unsigned short main()
{
    unsigned short x = 1231;
    if(x >= 0)
      return f(x);
    else
      return g(x);
}
