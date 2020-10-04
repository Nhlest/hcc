#include <stdio.h>
int fib(int);
int square(int);
int main(int a) {
  printf("%d\n", square(2) + fib(10));
  return 0;
}
