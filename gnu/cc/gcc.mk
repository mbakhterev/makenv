include $(addprefix $(dir $(lastword $(MAKEFILE_LIST))),gcc-common.mk)

cc = gcc -Wall -Werror -pedantic -c -pipe
coptimization = -flto $(gccopt)
cstd = c1x
cppstd = c++0x

lnk = gcc -pipe
loptimization = -flto -Wl,-s $(gccopt)

# undefine toolchain
