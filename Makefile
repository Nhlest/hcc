all:
	stack run > a.s
	gcc bootstrap.c a.s -o bootstrap
clean:
	-rm -f a.s bootstrap
