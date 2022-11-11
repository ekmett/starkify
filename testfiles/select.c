__attribute__((noinline)) unsigned int f(unsigned int i)
{
  if (i == 2)
    return 3;
  else
    return 4;
}

unsigned int main() { return f(2) + f(5); }
