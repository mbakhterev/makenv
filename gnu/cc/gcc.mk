# include $(addprefix $(dir $(lastword $(MAKEFILE_LIST))),gcc-common.mk)

gccopt = -O3 -march=native -mtune=native

cc = gcc -Wall -Werror -pedantic -c -pipe
cflags =
coptimization = -flto $(gccopt)
cdebug = -g
cstd = c1x
cppstd = c++0x

lnk = gcc -pipe
lflags =
loptimization = -flto -Wl,-s $(gccopt)
ldebug = -g

ar = ar
