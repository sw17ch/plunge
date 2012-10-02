#include "sample.h"
#include "foo.h"
#include "bar.h"

#define A_DEFN(x) \
  struct a_struct_named_##x { \
    int int_##x; \
    float float_##x; \
  };

/*
 * This is a long comment. It will be stripped in the CPP output. It's good to
 * include things like this in test files.
 */

int foo(int x) {
  A_DEFN(neato);
  return ADD_THINGS(x, 5);
}

int main(int argc, char * argv[]) {
  return foo(10);
}
