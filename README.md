# WCC (full name tbd)

## Usage:

Run the thing to get x86 asm output
`stack run > a.s` 

Bootstrap it with a simple c program

```c
#include <stdio.h>
int fib(int);
int main(int a) {
  printf("%d\n", fib(10));
  return 0;
}
```
`gcc main.c a.s && ./a.out`

Get confirm that the output result is indeed `55`
