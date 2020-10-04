# WCC (full name tbd)

## Usage:

Run the thing to get x86 asm output
`stack run > a.s` 

Bootstrap it with a simple c program

`gcc bootstrap.c a.s && ./a.out`

Confirm that the output result (2 squared + 55th fibonacci number) is indeed `59`
