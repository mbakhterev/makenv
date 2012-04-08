include $(addprefix $(dir $(lastword $(MAKEFILE_LIST))),gcc-common.mk)

cc = gcc -Wall -Werror -pedantic -c -pipe
coptimization = -O3
cstd = c99
cppstd = c++0x

lnk = gcc -pipe
loptimization = -O3 -Wl,-s

undefine toolchain
