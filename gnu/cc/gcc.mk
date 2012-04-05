include $(addprefix $(dir $(lastword MAKEFILE_LIST)),gcc-common.mk)

cc = $(cctarget) -Wall -Werror -pedantic -c -pipe
cflags = -I .
cdebug = -g
coptimization = -flto -O0
cstd = c1x
cppstd = c++0x

lnk = $(cctarget) -pipe
lflags =
ldebug = -g
loptimization = -flto -O3 -Wl,-s


undefine toolchain
undefine cctarget
