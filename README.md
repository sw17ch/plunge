Plunge
======

An exploratory tool for the C Language.
---------------------------------------

Usage
=====

Right now, it's pretty simple. Feed it a C file, and it will use a `gcc` from
your path and output a table representing what parts of your C file become
equivalent parts of the preprocessed file.

As an example, let's look at the following input file:

```C
#define BOOP (999)

int main(int argc, char * argv[]) {
    return BOOP;
}
```

If we name this file `boop.c`, and we run plunge over it, we will see the
following output:

```C
************************************   # 1 "example.c"
************************************   # 1 "<built-in>"
************************************   # 1 "<command-line>"
************************************   # 1 "example.c"
#define BOOP (999)

int main(int argc, char * argv[]) {    int main(int argc, char * argv[]) {
    return BOOP;                           return (999);
}
```

The left hand side of this output is the original C file. The right hand side
is the preprocessed output. You'll notice on the left there are a some lines
full of asterisks. Lines full of asterisks represent lines that do not have a
corresponding line on the opposite side.

This becomes more interesting when using `#include` statements. Try it!
