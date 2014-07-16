echo = echo -e

cc = gcc -Wall -Werror -pedantic -c -pipe
cflags =

coptimization = -flto
cdebug = -g -flto

# coptimization = -O3 -march=native -mtune=native
# cdebug = -g

cstd = c1x
cppstd = c++11
dep = $(cc)

lnk = gcc -pipe
lflags =

loptimization = -flto -fuse-linker-plugin -static -Wl,-s -O3 -march=native -mtune=native
ldebug = -g -flto -static

# loptimization = -Wl,-s
# ldebug = -g

ar = gcc-ar

lex = flex -Caer -8 --yylineno --bison-locations
yacc = bison -Wall -L C --locations

# Переменные для подстройки конкретных целей 

c99lexfix = -Wno-unused-but-set-variable
