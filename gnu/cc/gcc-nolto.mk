include $(addprefix $(dir $(lastword MAKEFILE_LIST)),gcc-common.mk)

cc = cctarget -Wall -Werror -pedantic -c -pipe
cflags = -I .
cdebug = -g
coptimization = -O3
cstd = c99
cppstd = c++0x

lnk = cctarget -pipe
lflags =
ldebug = -g
loptimization = -O3 -Wl,-s
