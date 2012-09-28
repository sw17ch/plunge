#include "sample.h"
#include "foo.h"
#include "bar.h"

int foo(int x) {
  return ADD_THINGS(x, 5);
}

int main(int argc, char * argv[]) {
  return foo(10);
}
