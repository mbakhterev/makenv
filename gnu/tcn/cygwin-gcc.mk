echo = echo -e

cc = gcc -Wall -Werror -pedantic -c -pipe
cflags =
coptimization = -O3 -march=native -mtune=native
cdebug = -g3
cstd = c1x
cppstd = c++0x
dep = $(cc)

lnk = gcc -pipe
lflags =
loptimization = -Wl,-s 
ldebug = -g3

ar = ar

lex = flex -Caer -8 --yylineno --bison-locations

yacc = bison -Wall -L C --locations

# Переменные для подстройки конкретных целей 

strictfix = -U__STRICT_ANSI__ -I_REENT_ONLY
c99lexfix = -Wno-unused-but-set-variable $(strictfix)
