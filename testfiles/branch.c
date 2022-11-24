__attribute__((noinline)) unsigned int then_branch() {
  return 3;
}

__attribute__((noinline)) unsigned int else_branch() {
  return 4;
}

__attribute__((noinline)) unsigned int if_test(unsigned int i)
{
  if (i == 2)
    return then_branch();
  else
    return else_branch();
}

unsigned int main() { return if_test(2) + if_test(5); }
