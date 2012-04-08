include $(addprefix $(dir $(lastword MAKEFILE_LIST)),gcc-common.mk)

cc = i686-pc-mingw32-gcc -Wall -Werror -pedantic -c -pipe
coptimization = -O3
cstd = c99
cppstd = c++0x

lnk = i686-pc-mingw32-gcc -pipe
loptimization = -O3 -Wl,-s

undefine toolchain
