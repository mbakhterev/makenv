cc = gcc -Wall -Werror -pedantic -c -pipe
cflags = -I .
cdebug = -g
coptimization = -O3
cstd = c99
cppstd = c++0x

lnk = gcc -pipe
lflags =
ldebug = -g
loptimization = -O3 -Wl,-s
