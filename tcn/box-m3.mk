cc = gcc -march=armv7-m -mtune=cortex-m3
cflags = -Wall -Werror -pedantic -pipe

coptimization = -flto -Os
cdebug = -g3 -flto

cstd = c11
cppstd = c++14
dep = $(cc)

lnk = gcc -march=armv7-m -mtune=cortex-m3
lflags = -pipe

loptimization = -flto -fuse-linker-plugin -static -Os
ldebug = -g3 -flto -static

ar = gcc-ar
