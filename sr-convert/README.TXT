No binary is included with this release. To build one, you need the
MinGW compiler. Here are the steps.

[1] Extract this source into a directory by itself.

[2] Make sure the MinGW "bin" directory is on your PATH.

[3] Use mingw32-make.

This code will NOT compile with a commercial compiler (such as Visual
Studio) without some editing. The HTML manual describes how to comment
out the extended inline assembly (which is also necessary when porting
to non-x86 CPUs). This reduces performance by about 50%.

This exact source code will also build with Linux, except that the
build tools are probably already on your PATH, and you just use make.

You can ignore the warnings about deprecated or antiquated headers.
