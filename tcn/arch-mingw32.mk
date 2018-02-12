echo = echo -e

cc = i686-w64-mingw32-gcc -Wall -Werror -pedantic -c -pipe
cflags =

coptimization = -O3
cdebug = -g 

cstd = c1x
cppstd = c++11
dep = $(cc)

lnk = i686-w64-mingw32-gcc -pipe
lflags =

loptimization = -static -Wl,-s
ldebug = -g -static

ar = i686-w64-mingw32-gcc-ar

lex = flex -Caer -8 --yylineno --bison-locations
yacc = bison -Wall -L C --locations
