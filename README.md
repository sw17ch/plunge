Plunge
======

An exploratory tool for the C Language.
---------------------------------------

Usage
=====

Build
-----

You'll need the [Haskell Platform](http://www.haskell.org/platform/) or other
equivalent Haskell configuration.

```sh
git clone https://github.com/sw17ch/plunge.git
cd plunge
cabal install --only-dependencies
cabal configure
cabal build
```

Execute
-------

After running the above command, the binary will be located at:
`./dist/build/plunge/plunge`. You can run the program like this:

```sh
./dist/build/plunge/plunge [your_file.c]
```
The full list of options can be found by passing `--help` or `-?` to `plunge`.

```
$ ./dist/build/plunge/plunge --help
Plunge 0.0.0.0, (C) John Van Enk 2012

Usage: plunge (-i|--input-file FILE) [-g|--gcc-option OPTION] [-p|--line-pad STRING] [-e|--empty-line STRING] [-m|--max-width NUMBER] [-v|--vert-sep STRING] [-h|--horiz-sep STRING]

Available options:
  -h,--help                Show this help text
  -i,--input-file FILE     The C file to analyze.
  -g,--gcc-option OPTION   An option to pass to GCC. Can be specified multiple times.
  -p,--line-pad STRING     String to use to pad lines. (default: " ")
  -e,--empty-line STRING   String to use to represent empty lines (default: "-")
  -m,--max-width NUMBER    How wide each column of output is allowed to be (default: 80)
  -v,--vert-sep STRING     What string to use to separate the two columns (default: " | ")
  -h,--horiz-sep STRING    What string to use to separate horizontal segments (default: "-")
```

Explanation
-----------

Right now, it's pretty simple. Feed it a C file, and it will use a `gcc` from
your path and output a table representing what parts of your C file become
equivalent parts of the preprocessed file.

Example
-------

Here's a sample session using `example_files/sample.c` from this project.
```
$ plunge -i example_files/sample.c -m 40 -g -DCAT
........................................ | # 1 "example_files/sample.c"            
-----------------------------------------------------------------------------------
........................................ | # 1 "<built-in>"                        
-----------------------------------------------------------------------------------
........................................ | # 1 "<command-line>"                    
-----------------------------------------------------------------------------------
........................................ | # 1 "example_files/sample.c"            
-----------------------------------------------------------------------------------
#include "sample.h"                      | # 1 "example_files/sample.h" 1          
........................................ |                                         
........................................ |                                         
........................................ |                                         
........................................ |                                         
........................................ |                                         
........................................ | int foo(int x);                         
........................................ | # 2 "example_files/sample.c" 2          
-----------------------------------------------------------------------------------
#include "foo.h"                         | # 1 "example_files/foo.h" 1             
........................................ | # 1 "example_files/bar.h" 1             
........................................ | int baz(void);                          
........................................ | # 2 "example_files/foo.h" 2             
........................................ | int bar(void);                          
........................................ | # 3 "example_files/sample.c" 2          
-----------------------------------------------------------------------------------
........................................ | # 15 "example_files/sample.c"           
-----------------------------------------------------------------------------------
                                         | ........................................
#define A_DEFN(x) \                      | ........................................
  struct a_struct_named_##x { \          | ........................................
    int int_##x; \                       | ........................................
    float float_##x; \                   | ........................................
  };                                     | ........................................
                                         | ........................................
/*                                       | ........................................
 * This is a long comment. It will be st | ........................................
 * include things like this in test file | ........................................
 */                                      | ........................................
                                         | ........................................
-----------------------------------------------------------------------------------
int foo(int x) {                         | int foo(int x) {                        
  A_DEFN(neato);                         |   struct a_struct_named_neato { int int_
  return ADD_THINGS(x, 5);               |   return (x + 5);                       
}                                        | }                                       
                                         |                                         
#ifdef CAT                               |                                         
void cat(void) {                         | void cat(void) {                        
}                                        | }                                       
#endif                                   |                                         
                                         |                                         
int main(int argc, char * argv[]) {      | int main(int argc, char * argv[]) {     
  return foo(10);                        |   return foo(10);                       
}                                        | }                                       
```

This example shows how to specify the file to preprocess, the maximum column
width and a preprocessor option (`-DCAT`). The output shows the original C file
on the left and the preprocessed file on the right. The lines are aligned such
that it is easy to tell which preprocessed code resulted from which C code.

Lines filled with dots are lines that did no have any corresponding data from
the other side.

For completness, here's another run of the same file that uses different
options.

```
Shiny:plunge [git:branch master]$ ./dist/build/plunge/plunge -i example_files/sample.c -m 10
.......... | # 1 "examp
-----------------------
.......... | # 1 "<buil
-----------------------
.......... | # 1 "<comm
-----------------------
.......... | # 1 "examp
-----------------------
#include " | # 1 "examp
.......... |           
.......... |           
.......... |           
.......... |           
.......... |           
.......... | int foo(in
.......... | # 2 "examp
-----------------------
#include " | # 1 "examp
.......... | # 1 "examp
.......... | int baz(vo
.......... | # 2 "examp
.......... | int bar(vo
.......... | # 3 "examp
-----------------------
.......... | # 15 "exam
-----------------------
           | ..........
#define A_ | ..........
  struct a | ..........
    int in | ..........
    float  | ..........
  };       | ..........
           | ..........
/*         | ..........
 * This is | ..........
 * include | ..........
 */        | ..........
           | ..........
-----------------------
int foo(in | int foo(in
  A_DEFN(n |   struct a
  return A |   return (
}          | }         
           |           
#ifdef DOG |           
void doggy |           
}          |           
#endif     |           
           |           
int main(i | int main(i
  return f |   return f
}          | }
```
