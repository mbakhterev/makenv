cc = gcc -std=c1x -Wall -Werror -pedantic -c -pipe
cflags = -I .
cdebug = -g
coptimization = -flto 

lnk = gcc -pipe
lflags =
ldebug = -g
loptimization = -flto -O4 -Wl,-s
