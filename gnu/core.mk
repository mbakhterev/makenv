# Гораздо удобнее написать используемые функции на чем-то более вменяемом, чем язык функций make.

# Для успешной работы должна быть задана директория для сборки. Проверяем
# наличие переменной и вычисляем физический путь до него.

ifndef BDIR
$(error build root dir is not defined)
else
bdir = $(shell readlink -f '$(BDIR)')
endif

mkpath = \
	{ test -d '$(1)' \
		|| { echo 'error: no build dir: $(1)' 1>&2; false; }; } \
	&& { test -d $(2) || mkdir -p '$(2)'; }

bits = $(bdir)/B
rootpath = $(patsubst %/,%,$(dir $(firstword $(MAKEFILE_LIST))))
nodepath = $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
bitspath = $(bits)/$(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

sub = { sed -e 's:$(1):$(2):g'; }

ifndef TCN
TCN = toolchain
endif

include $(call rootpath)/$(TCN).mk

B = $(bdir)/bin
T = $(bdir)/tst
L = $(bdir)/lib
I = $(bdir)/include

suborig = $(subst file,,$(origin $(1)))
checkdefs = $(if $(strip $(foreach v,$(1),$(call suborig,$(v)))),$(error $(2)))

ifndef echo
$(error echo with escape interpretation isn't defined)
endif

$(B)/%.sh:
	@ $(echo) '\tinstall\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& install -m 755 $< $@

$(T)/%.sh:
	@ $(echo) '\tinstall\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& install -m 755 $< $@

ifdef cc # C/C++ Compiler rules group

vars = dep cc cstd cppstd cflags cdebug coptimization
$(call checkdefs,$(vars),C/C++ Compiler group needs: $(vars))

ifeq ($(DBG), Y)
copt = $(cdebug)
else
copt = $(coptimization)
endif

o2d = $(patsubst %.o,%.d,$(1))
c2o = $(addprefix $(1)/,$(patsubst %.c,%.o,$(2)))
cpp2o = $(addprefix $(1)/,$(patsubst %.cpp,%.o,$(2)))

$(bits)/%.d: %.c
	@ $(echo) '\tdep\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(dep) $(cflags) -x c -std=$(cstd) -MM $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) > $@

$(bits)/%.d: %.cpp
	@ $(echo) '\tdep c++\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(dep) $(cflags) -x c++ -std=$(cppstd) -MM $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) > $@

$(bits)/%.d: $(bits)/%.c
	@ $(echo) '\tdep\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(dep) $(cflags) -x c -std=$(cstd) -MM -MG $< \
		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) \
		| $(call sub,\s\+\([^/]\+\.h\)\s*, $(@D)/\1 ) > $@

$(bits)/%.o: %.c
	@ $(echo) '\tcc\t$@' \
	&& $(cc) -c $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(bits)/%.o: %.cpp
	@ $(echo) '\tcc c++\t$@' \
	&& $(cc) -c $(cflags) $(copt) -x c++ -std=$(cppstd) -o $@ $<

$(bits)/%.o: $(bits)/%.c
	@ $(echo) '\tcc gen\t$@' \
	&& $(cc) -c $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(bits)/%.h: %.h
	@ $(echo) '\theader\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& install -m 755 $< $@

define headroute
$(I)/$1/%.h: $2/%.h
	@ $(echo) '\theader\t$$@' \
	&& $(call mkpath,$(bdir),$$(@D)) \
	&& install -m 755 $$< $$@
endef

endif # C/C++ Compiler group

ifdef lnk # LiNK group

vars = lnk lflags ldebug loptimization ar
$(call checkdefs,$(vars),LiNK group needs: $(vars))

ifeq ($(DBG), Y)
lflags += $(ldebug)
else
lflags += $(loptimization)
endif

$(B)/%:
	@ $(echo) '\tlink\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(lnk) $^ -o $@ $(lflags) 

$(T)/%:
	@ $(echo) '\tlink\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(lnk) $^ -o $@ $(lflags)

$(L)/%.a:
	@ $(echo) '\tlib\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& { [ -f '$@' ] && rm $@ || true; } \
	&& $(ar) rc $@ $^

endif

ifdef lex

$(bits)/%.lex.c: %.l
	@ $(echo) '\tlex\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(lex) -o $@ $<

endif

ifdef yacc

$(bits)/%.tab.c $(bits)%.tab.h: %.y
	@ $(echo) '\tyacc\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(yacc) -d -b '$(bits)/$*' $<

endif

ifdef tex # LaTeX group

vars = texcode
$(call checkdefs,$(vars),LaTeX group needs: $(vars))

$(bdir)/%.pdf: $(bdir)/%.tex
	@ $(echo) '\ttex\t$@' \
	&& (cd $(@D) && ($(tex) $< && $(tex) $<)) \
		| iconv -f $(texcode) \
		| sed -ne 's:^$(bdir):\.:g; p'

$(bdir)/%.tex: %.tex
	@ $(echo) '\tcp\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& iconv -t $(texcode) < $< > $@

$(bdir)/%.sty: %.sty
	@ $(echo) '\tcp\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& iconv -t $(texcode) < $< > $@

endif # LaTeX group

ifdef xetex # XeLaTeX group

$(bdir)/%.pdf: $(bdir)/%.xtex
	@ $(echo) '\txetex\t$@' \
	&& (cd $(@D) && ($(xetex) $< && $(tex) $<)) \
		| sed -ne 's:^$(bdir):\.:g; p'

$(bdir)/%.xtex: %.xtex
	@ $(echo) '\tcp\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& cp $< $@

endif # XeLaTeX group

