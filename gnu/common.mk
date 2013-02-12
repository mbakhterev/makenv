ifndef foreign
$(error foreign root dir isn't defined)
endif

ifndef bld
$(error build root dir isn't defined)
endif

include toolchain.mk
include $(foreign)/mkenv/gnu/tex/texlive.mk
include $(foreign)/mkenv/gnu/lexy/flex-yacc.mk

ifeq ($(debug), Y)
copt = $(cdebug)
lflags += $(ldebug)
else
copt = $(coptimization)
lflags += $(loptimization)
endif

o2d = $(patsubst %.o,%.d,$(1))
c2o = $(addprefix $(1)/,$(patsubst %.c,%.o,$(2)))
cpp2o = $(addprefix $(1)/,$(patsubst %.cpp,%.o,$(2)))

mkpath = \
	{ test -d '$(1)' \
		|| { echo 'error: no build dir: $(1)' 1>&2; false; }; } \
	&& { test -d $(2) || mkdir -p '$(2)'; }

bldpath = $(bld)/$(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

bits = $(bld)/B
bitspath = $(bits)/$(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

sub = { sed -e 's:$(1):$(2):g'; }

B = $(bld)/bin
L = $(bld)/lib
I = $(bld)/include

$(bits)/%.d: %.c
	@ echo -e '\tdep\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(cc) $(cflags) -x c -std=$(cstd) -MM $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) > $@

$(bits)/%.d: %.cpp
	@ echo -e '\tdep c++\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(cc) $(cflags) -x c++ -std=$(cppstd) -MM $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) > $@

$(bits)/%.d: $(bits)/%.c
	@ echo -e '\tdep\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(cc) $(cflags) -x c -std=$(cstd) -MM -MG $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) \
		| $(call sub,\s\+\([^/]\+\.h\)\s*, $(@D)/\1 ) > $@

$(bits)/%.lex.c: %.l
	@ echo -e '\tlex\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(lex) -o $@ $<

$(bits)/%.tab.c $(bits)%.tab.h: %.y
	@ echo -e '\tyacc\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(yacc) -d -b '$(bits)/$*' $<

$(bits)/%.o: %.c
	@ echo -e '\tcc\t$@' \
	&& $(cc) $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(bits)/%.o: %.cpp
	@ echo -e '\tcc c++\t$@' \
	&& $(cc) $(cflags) $(copt) -x c++ -std=$(cppstd) -o $@ $<

$(bits)/%.o: $(bits)/%.c
	@ echo -e '\tcc gen\t$@' \
	&& $(cc) $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(B)/%: %.sh
	@ echo -e '\tinstall\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& install -m 755 $< $@

$(B)/%:
	@ echo -e '\tlink\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(lnk) $^ -o $@ $(lflags)

$(L)/%.a:
	@ echo -e '\tlib\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& { [ -f '$@' ] && rm $@ || true; } \
	&& $(ar) rc $@ $^

$(I)/%.h: lib/%.h
	@ echo -e '\theader\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& install -m 755 $< $@

$(bld)/%.pdf: $(bld)/%.tex
	@ echo -e '\ttex\t$@' \
	&& (cd $(@D) && ($(tex) $< && $(tex) $<)) \
		| iconv -f $(texcode) \
		| sed -ne 's:^$(bld):\.:g; p'

$(bld)/%.pdf: $(bld)/%.xtex
	@ echo -e '\txetex\t$@' \
	&& (cd $(@D) && ($(xetex) $< && $(tex) $<)) \
		| sed -ne 's:^$(bld):\.:g; p'

$(bld)/%.tex: %.tex
	@ echo -e '\tcp\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& iconv -t $(texcode) < $< > $@

$(bld)/%.xtex: %.xtex
	@ echo -e '\tcp\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& cp $< $@

$(bld)/%.sty: %.sty
	@ echo -e '\tcp\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& iconv -t $(texcode) < $< > $@
