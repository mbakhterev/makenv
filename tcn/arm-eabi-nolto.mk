cc := arm-none-eabi-gcc -march=armv7-m -mcpu=cortex-m3 -mthumb -c
cflags := -Wall -Werror -pedantic -pipe

coptimization := -Os
cdebug := -g3

cstd := c11
cppstd := c++14
dep := $(cc) -M

lnk := arm-none-eabi-gcc -march=armv7-m -mtune=cortex-m3 -mthumb
lflags := -pipe

loptimization := -static -Os -Wl,--gc-sections
ldebug := -g3 -static

ar := arm-none-eabi-ar

as := arm-none-eabi-as -march=armv7-m -mcpu=cortex-m3 -mthumb
objcopy := arm-none-eabi-objcopy
