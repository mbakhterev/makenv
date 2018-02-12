echo = echo -e

cc = clang
cflags = -Wall -Werror -pedantic -pipe

cdebug = -g -flto -O0
coptimization = -flto -O0

cstd=c1x
cppstd=c++0x
dep = $(cc)

lnk = clang
lflags = -pipe

ldebug = -g -flto
loptimization = -O3 -Wl,-s -flto

ar = ar

lex = flex -Caer -8 --yylineno --bison-locations

yacc = bison -Wall -L C --locations
