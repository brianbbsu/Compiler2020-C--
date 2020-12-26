# C-- Compiler
Author:
* B08902107 蘇柏瑄
* B07902123 蔡奇夆

## HW5 - Code Generation

We use `int main ();` instead of `int MAIN ();` as the entry point of the program.

TODO: check every function name starts with "gen", if it does not allocate any register, change the name to "_gen..."
TODO: change all 64 bit integer operation to 32 bit, only the needed situation like array dereference can use 64 bit operation.
