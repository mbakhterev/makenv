cc = clang -Wall -Werror -pedantic -c -pipe
cflags =
cdebug = -g -flto -O0
coptimization = -flto -O0
cstd=c1x
cppstd=c++0x

ifeq ($(shell uname -s),CYGWIN_NT-5.2)
lnk = llvm-ld -native
lflags =
ldebug =
loptimization = -Xlinker=-static -Xlinker=-Wl,-s
ar = llvm-ar
$(warning Cygwin llvm-ld instead clang driver)
else
lnk = clang -pipe
lflags =
ldebug = -g -flto
loptimization = -O3 -Wl,-s -flto
ar = ar
endif

undefine toolchain
