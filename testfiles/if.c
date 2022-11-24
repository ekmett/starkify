__attribute__((noinline))
unsigned int f(unsigned int x) { return x+1; }

__attribute__((noinline))
unsigned int g(unsigned int x) { return x-1; }

unsigned int main()
{
    unsigned int x = 1231;
    if(x >= 0)
      return f(x);
    else
      return g(x);
}
