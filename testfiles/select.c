__attribute__((noinline)) int f(int i)
{
  if (i == 2)
    return 3;
  else
    return 4;
}

int main() { return f(2) + f(5); }
