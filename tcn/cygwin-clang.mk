echo = echo -e

cc = clang -Wall -Werror -pedantic -c -pipe
cflags =
cdebug = -g -flto -O0
coptimization = -flto -O0
cstd=c1x
cppstd=c++0x
dep = $(cc)

lnk = llvm-ld -native
lflags =
ldebug =
loptimization = -Xlinker=-static -Xlinker=-Wl,-s
ar = llvm-ar

lex = flex -Caer -8 --yylineno --bison-locations

yacc = bison -Wall -L C --locations

# Подстройка конкретных целей

strictfix = -U__STRICT_ANSI__ -U_REENT_ONLY
c99lexfix = $(strictfix)
