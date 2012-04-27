include $(addprefix $(dir $(lastword $(MAKEFILE_LIST))),gcc-common.mk)

cc = gcc -Wall -Werror -pedantic -c -pipe
coptimization = $(gccopt)
cstd = c99
cppstd = c++0x

lnk = gcc -pipe
loptimization = -Wl,-s $(gccopt)

undefine toolchain
