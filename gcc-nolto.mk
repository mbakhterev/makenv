cc = gcc -Wall -Werror -pedantic -c -pipe
cflags = -I .
cdebug = -g
coptimization = -O4

lnk = gcc -pipe
lflags =
ldebug = -g
loptimization = -O4 -Wl,-s
