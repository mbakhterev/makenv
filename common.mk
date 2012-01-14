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

$(bld)/%.d: ./%.c
	@ echo -e '\tdep\t$<' \
	&& { test -d '$(bld)' \
		|| { echo 'error: no build dir: $(bld)' 1>&2; false; }; } \
	&& { test -d $(@D) || mkdir -p '$(@D)'; } \
	&& $(cc) $(cflags) -MM $< \
		| awk '{ gsub("$(*F).o", "$(@D)/$(*F).o $@"); print }' > $@

$(bld)/%.o: ./%.c
	@ echo -e '\tcc\t$<' \
	&& $(cc) $(cflags) $(copt) -o $@ $<

$(bld)/lib/%.a:
	@ echo -e '\tlib\t$@' \
	&& { test -d '$(bld)' \
		|| { echo 'error: no build dir: $(bld)' 1>&2; false; }; } \
	&& { test -d $(@D) || mkdir -p '$(@D)'; } \
	&& $(ar) rc $@ $^

$(bld)/bin/%:
	@ echo -e '\tlnk\t$@' \
	&& { test -d '$(bld)' \
		|| { echo 'error: no build dir: $(bld)' 1>&2; false; }; } \
	&& { test -d $(@D) || mkdir -p '$(@D)'; } \
	&& $(lnk) $(lflags) $^ -o $@

c2o = $(addprefix $(1)/, $(patsubst %.c, %.o, $(2)))
o2d = $(patsubst %.o, %.d, $(1))
