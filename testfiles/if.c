__attribute__((noinline))
int f(int x) { return x+1; }

__attribute__((noinline))
int g(int x) { return x-1; }

int main()
{
    int x = 1231;
    if(x >= 0)
      return f(x);
    else
      return g(x);
}
