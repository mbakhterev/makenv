echo = echo -e

cc = gcc
cflags = -Wall -Werror -pedantic -pipe

coptimization = -flto
cdebug = -g3 -flto

cstd = c1x
cppstd = c++11
dep = $(cc)

lnk = gcc
lflags = -pipe

loptimization = \
	-flto -fuse-linker-plugin -static -Wl,-s -O3 -march=native -mtune=native

ldebug = -g3 -flto -static

ar = gcc-ar

lex = flex -Caer -8 --yylineno --bison-locations
yacc = bison -Wall -L C --locations

# Переменные для подстройки конкретных целей 

c99lexfix = -Wno-unused-but-set-variable
