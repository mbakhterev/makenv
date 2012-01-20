$(bld)/lib/%.a:
	@ echo -e '\tlib\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(ar) rc $@ $^
