cc = clang -std=c1x -Wall -Werror -pedantic -c -pipe
cflags = -I .
cdebug = -g
coptimization = -flto 
cstd=c1x
cppstd=c++0x

lnk = clang -pipe
lflags =
ldebug = -g
loptimization = -flto -O4 -Wl,-s
