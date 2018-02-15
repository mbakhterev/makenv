cc = gcc
cflags = -Wall -Werror -pedantic -pipe

coptimization = -O3
cdebug = -g3

cstd = c1x
cppstd = c++0x

dep = $(cc) -M

lnk = gcc
lflags = -pipe

loptimization = -Wl,-s -static
ldebug = -g3

ar = ar

lex = flex -Caer -8 --yylineno --bison-locations

yacc = bison -Wall -L C --locations

# Переменные для подстройки конкретных целей 

strictfix = -U__STRICT_ANSI__ -D_REENT_ONLY
c99lexfix = -Wno-unused-but-set-variable $(strictfix)
