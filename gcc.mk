cc = gcc -Wall -Werror -pedantic -c -pipe
cflags = -I .
cdebug = -g
coptimization = -flto -O0

lnk = gcc -pipe
lflags =
ldebug = -g
loptimization = -flto -O4 -Wl,-s
