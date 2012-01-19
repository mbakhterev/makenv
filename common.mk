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

c2o = $(addprefix $(1)/, $(patsubst %.c, %.o, $(2)))
o2d = $(patsubst %.o, %.d, $(1))

mkpath = \
	{ test -d '$(1)' \
 		|| { echo 'error: no build dir: $(1)' 1>&2; false; }; } \
 	&& { test -d $(2) || mkdir -p '$(2)'; }

$(bld)/lib/%.a:
	@ echo -e '\tlib\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(ar) rc $@ $^

$(bld)/bin/%:
	@ echo -e '\tlnk\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(lnk) $(lflags) $^ -o $@

$(bld)/%.d: %.c
	@ echo -e '\tdep\t$<' \
	&& $(call mkpath,$(bld),$(@D)) \
 	&& $(cc) $(cflags) -MM $< \
 		| awk '{ gsub("$(*F).o", "$(@D)/$(*F).o $@"); print }' > $@
 
$(bld)/%.o: %.c
	@ echo -e '\tcc\t$<' \
	&& $(cc) $(cflags) $(copt) -o $@ $<
