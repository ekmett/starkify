__attribute__((noinline)) int then_branch() {
  return 3;
}

__attribute__((noinline)) int else_branch() {
  return 4;
}

__attribute__((noinline)) int if_test(int i)
{
  if (i == 2)
    return then_branch();
  else
    return else_branch();
}

int main() { return if_test(2) + if_test(5); }
