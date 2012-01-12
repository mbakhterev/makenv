ifdef debug
	cflags += $(cdebug)
	lflags += $(ldebug)
else
	cflags += $(coptimization)
	lflags += $(loptimization)
endif

ifndef bld
$(error "bld root isn't defined")
endif

ifndef foreign
$(error "foreign root isn't defined")
endif

$(bld)/%.d: ./%.c
	echo -e '\tdep\t$<' \
	&& { test -d '$(bld)' \
		|| { echo 'error: no build dir: $(bld)' 1>&2; false; }; } \
	&& { test -d $(@D) || mkdir -p '$(bld)'; } \
	&& $(cc) $(cflags) -MM $< \
		| awk '{ gsub("$(*F).o", "$(@D)/$(*F).o $@"); print }' > $@

$(bld)/%.o: ./%.c
	echo -e '\tcc\t$<' \
	&& $(cc) $(cflags) -o $@ $<

c2o = $(addprefix $(1)/, $(patsubst %.c, %.o, $(2)))
o2d = $(patsubst %.o, %.d, $(1))
