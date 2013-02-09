# include $(addprefix $(dir $(lastword $(MAKEFILE_LIST))),gcc-common.mk)

cc = gcc -Wall -Werror -pedantic -c -pipe
cflags =
coptimization = -flto
cdebug = -g -flto
cstd = c1x
cppstd = c++0x

lnk = gcc -pipe
lflags =
loptimization = -flto -Wl,-s -O3 -march=native -mtune=native
ldebug = -g -flto

ar = ar

undefine toolchain
