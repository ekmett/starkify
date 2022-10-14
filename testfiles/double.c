__attribute__((noinline))
int dbl(int x) { return 2*x; }

int main()
{ 
    int x = 23;
    return dbl(x);
}
