echo = echo -e

cc = gcc
cflags =  -Wall -Werror -pedantic -c -pipe

coptimization = -O3 -march=native -mtune=native
cdebug = -g3

cstd = c1x
cppstd = c++0x

dep = $(cc)

lnk = gcc
lflags = -pipe

loptimization = -Wl,-s 
ldebug = -g3

ar = ar

lex = flex -Caer -8 --yylineno --bison-locations

yacc = bison -Wall -L C --locations

# Переменные для подстройки конкретных целей 

strictfix = -U__STRICT_ANSI__ -I_REENT_ONLY
c99lexfix = -Wno-unused-but-set-variable $(strictfix)
