ifndef foreign
$(error foreign root dir isn't defined)
endif

ifndef bld
$(error build root dir isn't defined)
endif

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

suborig = $(subst file,,$(origin $(1)))
checkdefs = $(if $(strip $(foreach v,$(1),$(call suborig,$(v)))),$(error $(2)))

include toolchain.mk

ifndef echo
$(error echo with escape interpretation isn't defined)
endif

ifdef cc # C/C++ Compiler rules group

vars = dep cc cstd cppstd cflags cdebug coptimization
$(call checkdefs,$(vars),C/C++ Compiler group needs: $(vars))

ifeq ($(debug), Y)
copt = $(cdebug)
else
copt = $(coptimization)
endif

o2d = $(patsubst %.o,%.d,$(1))
c2o = $(addprefix $(1)/,$(patsubst %.c,%.o,$(2)))
cpp2o = $(addprefix $(1)/,$(patsubst %.cpp,%.o,$(2)))

$(bits)/%.d: %.c
	@ $(echo) '\tdep\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(dep) $(cflags) -x c -std=$(cstd) -MM $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) > $@

$(bits)/%.d: %.cpp
	@ $(echo) '\tdep c++\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(dep) $(cflags) -x c++ -std=$(cppstd) -MM $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) > $@

$(bits)/%.d: $(bits)/%.c
	@ $(echo) '\tdep\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(dep) $(cflags) -x c -std=$(cstd) -MM -MG $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) \
		| $(call sub,\s\+\([^/]\+\.h\)\s*, $(@D)/\1 ) > $@

$(bits)/%.o: %.c
	@ $(echo) '\tcc\t$@' \
	&& $(cc) $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(bits)/%.o: %.cpp
	@ $(echo) '\tcc c++\t$@' \
	&& $(cc) $(cflags) $(copt) -x c++ -std=$(cppstd) -o $@ $<

$(bits)/%.o: $(bits)/%.c
	@ $(echo) '\tcc gen\t$@' \
	&& $(cc) $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(I)/%.h: lib/%.h
	@ $(echo) '\theader\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& install -m 755 $< $@

endif # C/C++ Compiler group

ifdef lnk # LiNK group

vars = lnk lflags ldebug loptimization ar
$(call checkdefs,$(vars),LiNK group needs: $(vars))

ifeq ($(debug), Y)
lflags += $(ldebug)
else
lflags += $(loptimization)
endif

$(B)/%:
	@ $(echo) '\tlink\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(lnk) $^ -o $@ $(lflags)

$(L)/%.a:
	@ $(echo) '\tlib\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& { [ -f '$@' ] && rm $@ || true; } \
	&& $(ar) rc $@ $^

endif

ifdef lex

$(bits)/%.lex.c: %.l
	@ $(echo) '\tlex\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(lex) -o $@ $<

endif

ifdef yacc

$(bits)/%.tab.c $(bits)%.tab.h: %.y
	@ $(echo) '\tyacc\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& $(yacc) -d -b '$(bits)/$*' $<

endif

ifdef tex # LaTeX group

vars = texcode
$(call checkdefs,$(vars),LaTeX group needs: $(vars))

$(bld)/%.pdf: $(bld)/%.tex
	@ $(echo) '\ttex\t$@' \
	&& (cd $(@D) && ($(tex) $< && $(tex) $<)) \
		| iconv -f $(texcode) \
		| sed -ne 's:^$(bld):\.:g; p'

$(bld)/%.tex: %.tex
	@ $(echo) '\tcp\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& iconv -t $(texcode) < $< > $@

$(bld)/%.sty: %.sty
	@ $(echo) '\tcp\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& iconv -t $(texcode) < $< > $@

endif # LaTeX group

ifdef xetex # XeLaTeX group

$(bld)/%.pdf: $(bld)/%.xtex
	@ $(echo) '\txetex\t$@' \
	&& (cd $(@D) && ($(xetex) $< && $(tex) $<)) \
		| sed -ne 's:^$(bld):\.:g; p'

$(bld)/%.xtex: %.xtex
	@ $(echo) '\tcp\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& cp $< $@

endif # XeLaTeX group

$(B)/%: %.sh
	@ $(echo) '\tinstall\t$@' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& install -m 755 $< $@
