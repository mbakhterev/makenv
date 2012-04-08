include $(addprefix $(dir $(lastword $(MAKEFILE_LIST))),gcc-common.mk)

cc = gcc -Wall -Werror -pedantic -c -pipe
coptimization = -flto -O0
cstd = c1x
cppstd = c++0x

lnk = gcc -pipe
loptimization = -flto -O3 -Wl,-s

undefine toolchain
