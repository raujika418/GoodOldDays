OBJS= hash.o inout.o lex_yy.o opcode.o table.o y_tab.o lua.o iolib.o mathlib.o strlib.o

CFLAGS= -O2 -I. -g

T= lua

all:	$T

$T:	$(OBJS)
	gcc -o $@ $(OBJS) -lm -g

$OBJS:	%.c
	gcc -o hash.o -c hash.c -g
	gcc -o inout.o -c inout.c -g
	gcc -o lex_yy.o -c lex_yy.c -g
	gcc -o opcode.o -c opcode.c -g
	gcc -o table.o -c table.c -g
	gcc -o y_tab.o -c y_tab.c -g
	gcc -o lua.o -c lua.c -g
	gcc -o iolib.o -c iolib.c -g
	gcc -o mathlib.o -c mathlib.c -g
	gcc -o strlib.o -c strlib.c -g

A=--------------------------------------------------------------------------
test:	$T
	@echo "$A"
	./$T sort.lua main
	@echo "$A"
	./$T globals.lua | sort | column
	@echo "$A"
	./$T array.lua
	@echo "$A"
	./$T save.lua
	@echo "$A"
	./$T test.lua retorno_multiplo norma

clean:
	rm -f $T $(OBJS) core core.*

diff:
	diff . fixed | grep -v ^Only
