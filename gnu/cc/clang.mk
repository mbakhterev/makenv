cc = clang -Wall -Werror -pedantic -c -pipe
cflags =
cdebug = -g
coptimization = -flto -O4
cstd=c1x
cppstd=c++0x

lnk = llvm-ld -native
lflags =
ldebug =
loptimization = -Xlinker=-static -Xlinker=-Wl,-s

ar = ar

undefine toolchain
