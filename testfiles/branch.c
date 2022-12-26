__attribute__((noinline)) unsigned short then_branch() {
  return 3;
}

__attribute__((noinline)) unsigned short else_branch() {
  return 4;
}

__attribute__((noinline)) unsigned short if_test(unsigned short i)
{
  if (i == 2)
    return then_branch();
  else
    return else_branch();
}

unsigned short main() { return if_test(2) + if_test(5); }
