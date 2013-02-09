cc = clang -Wall -Werror -pedantic -c -pipe
cflags =
cdebug = -g -flto
coptimization = -flto -O3
cstd=c1x
cppstd=c++0x

lnk = llvm-ld -native
lflags =
ldebug =
loptimization = -Xlinker=-static -Xlinker=-Wl,-s

ar = ar

undefine toolchain
