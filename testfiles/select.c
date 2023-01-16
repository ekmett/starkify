__attribute__((noinline)) unsigned short f(unsigned short i)
{
  if (i == 2)
    return 3;
  else
    return 4;
}

unsigned short main() { return f(2) + f(5); }
