echo = echo -e

cc = clang -Wall -Werror -pedantic -c -pipe
cflags =
cdebug = -g -flto -O0
coptimization = -flto -O0
cstd=c1x
cppstd=c++0x
dep = $(cc)

lnk = clang -pipe
lflags =
ldebug = -g -flto
loptimization = -O3 -Wl,-s -flto
ar = ar

lex = flex -Caer -8 --yylineno --bison-locations

yacc = bison -Wall -L C --locations
