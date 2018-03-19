cc = arm-none-eabi-gcc -march=armv7-m -mtune=cortex-m3 -nostdlib
cflags = -Wall -Werror -pedantic -pipe

coptimization = -flto -Os
cdebug = -g3 -flto

cstd = c11
cppstd = c++14
dep = $(cc) -M

lnk = arm-none-eabi-gcc -march=armv7-m -mtune=cortex-m3 -nostdlib
lflags = -pipe

loptimization = -flto -fuse-linker-plugin -static -Os
ldebug = -g3 -flto -static

ar = arm-none-eabi-ar
