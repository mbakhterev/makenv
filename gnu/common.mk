makedir = $(abspath $(dir $(firstword $(MAKEFILE_LIST))))

ifndef toolchain
toolchain = gcc
endif

ifndef foreign
foreign = $(abspath $(makedir)/../foreign)
endif

ifndef bld
bld = $(abspath $(makedir)/../bld)
endif

include $(foreign)/mkenv/gnu/cc/$(toolchain).mk

ifeq ($(debug), Y)
copt = $(cdebug)
lflags += $(ldebug)
else
copt = $(coptimization)
lflags += $(loptimization)
endif

o2d = $(patsubst %.o,%.d,$(1))
c2o = $(addprefix $(1)/, $(patsubst %.c,%.o,$(2)))
cpp2o = $(addprefix $(1)/, $(patsubst %.cpp,%.o,$(2)))

mkpath = \
	{ test -d '$(1)' \
 		|| { echo 'error: no build dir: $(1)' 1>&2; false; }; } \
 	&& { test -d $(2) || mkdir -p '$(2)'; }

$(bld)/bin/%: %.sh
	@ echo -e '\tinstall\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& install -m 755 $< $@

$(bld)/bin/%:
	@ echo -e '\tlink\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(lnk) $(lflags) $^ -o $@

$(bld)/%.d: %.c
	@ echo -e '\tdep\t$<' \
	&& $(call mkpath,$(bld),$(@D)) \
 	&& $(cc) $(cflags) -x c -std=$(cstd) -MM $< \
 		| awk '{ gsub("$(*F).o", "$(@D)/$(*F).o $@"); print }' > $@

$(bld)/%.d: %.cpp
	@ echo -e '\tdep c++\t$<' \
	&& $(call mkpath,$(bld),$(@D)) \
 	&& $(cc) $(cflags) -x c++ -std=$(cppstd) -MM $< \
 		| awk '{ gsub("$(*F).o", "$(@D)/$(*F).o $@"); print }' > $@
 
$(bld)/%.o: %.c
	@ echo -e '\tcc\t$<' \
	&& $(cc) $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<
 
$(bld)/%.o: %.cpp
	@ echo -e '\tcc c++\t$<' \
	&& $(cc) $(cflags) $(copt) -x c++ -std=$(cppstd) -o $@ $<
