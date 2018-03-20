cc = gcc -march=armv7-m -mcpu=cortex-m3 -mthumb -nostdlib
cflags = -Wall -Werror -pedantic -pipe

coptimization = -flto -Os
cdebug = -g3 -flto

cstd = c11
cppstd = c++14
dep = $(cc) -M

lnk = gcc -march=armv7-m -mcpu=cortex-m3 -mthumb -nostdlib
lflags = -pipe

loptimization = -flto -fuse-linker-plugin -static -Os -Wl,--gc-sections
ldebug = -g3 -flto -static

ar = gcc-ar

as := as -march=armv7-m -mcpu=cortex-m3 -mthumb -c
objcopy := objcopy
