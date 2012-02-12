cc = gcc -Wall -Werror -pedantic -c -pipe
cflags = -I .
cdebug = -g
coptimization = -flto -O0
cstd = c1x
cppstd = c++0x

lnk = gcc -pipe
lflags =
ldebug = -g
loptimization = -flto -O3 -Wl,-s
