#include <stdio.h>
#include "foo.h"
#include "bar.h"

/*
 * This is a long comment. It will be stripped in the CPP output. It's good to
 * include things like this in test files.
 */

int main(int argc, char * argv[]) {
  printf("Hello World!\n");
  return 0;
}
