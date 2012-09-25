# 1 "sample.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "sample.c"
# 1 "sample.h" 1





int foo(int x);
# 2 "sample.c" 2

int foo(int x) {
  return (x + 5);
}

int main(int argc, char * argv[]) {
  return foo(10);
}
