ifndef bld
$(error "bld root isn't defined")
endif

ifndef foreign
$(error "foreign root isn't defined")
endif

ifndef toolchain
$(error "toolchain isn't defined")
endif

include $(foreign)/mkenv/$(toolchain).mk

ifdef debug
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

$(bld)/bin/%:
	@ echo -e '\tlnk\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(lnk) $(lflags) $^ -o $@

$(bld)/%.d: %.c
	@ echo -e '\tdep\t$<' \
	&& $(call mkpath,$(bld),$(@D)) \
 	&& $(cc) $(cflags) -x c -std=c1x -MM $< \
 		| awk '{ gsub("$(*F).o", "$(@D)/$(*F).o $@"); print }' > $@
 
$(bld)/%.o: %.c
	@ echo -e '\tcc\t$<' \
	&& $(cc) $(cflags) $(copt) -x c -std=c1x -o $@ $<

$(bld)/%.d: %.cpp
	@ echo -e '\tdep\t$<' \
	&& $(call mkpath,$(bld),$(@D)) \
 	&& $(cc) $(cflags) -x c++ -std=c++0x -MM $< \
 		| awk '{ gsub("$(*F).o", "$(@D)/$(*F).o $@"); print }' > $@
 
$(bld)/%.o: %.cpp
	@ echo -e '\tcc c++\t$<' \
	&& $(cc) $(cflags) $(copt) -x c++ -std=c++0x -o $@ $<
