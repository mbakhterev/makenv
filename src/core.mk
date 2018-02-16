# Удобнее написать вспомогательные функции на более дружелюбном языке, чем язык
# функций make с асинхронной (совсем не RiDE) семантикой. Использую встроенную
# поддержку Guile Scheme

$(guile (load "$(dir $(lastword $(MAKEFILE_LIST)))core.scm"))
runscm-path := $(guile runscm-path)

# Для успешной работы должна быть задана директория для сборки. Проверяем
# наличие переменной и вычисляем физический путь до него.

ifndef BDIR
$(error BDIR (build root dir) is not defined)
else
bdir = $(shell readlink -f '$(BDIR)')

# Нет желания каждый раз передавать bdir на сторону guile, разбирать и проверять
# наличие директории. А это нужно для логики ensure-path!, чтобы проверять
# корректность путей. Поэтому запоминаем
$(guile (bdir-set! "$(bdir)"))
endif

bits = $(bdir)/B

# rootpath = $(patsubst %/,%,$(dir $(firstword $(MAKEFILE_LIST))))
# nodepath = $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
# bitspath = $(bits)/$(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

# sub = { sed -e 's:$(1):$(2):g'; }

ifndef TCN
TCN = toolchain
endif

include $(guile root)/$(TCN).mk

# B := $(bdir)/bin
# T := $(bdir)/tst
# L := $(bdir)/lib
# I := $(bdir)/include

# FIXME: это хак, чтобы согласовать переменные в guile и make. Автоматически это
# согласование плохо работает, при возврате строк из guile для обработки в make.

B := $(guile B)
T := $(guile T)
L := $(guile L)
I := $(guile I)

# suborig = $(subst file,,$(origin $(1)))
# checkdefs = $(if $(strip $(foreach v,$(1),$(call suborig,$(v)))),$(error $(2)))

# О специальных переменных в make:
#
#   @  - путь до текущей цели
#   @D - имя директории текущей цели
#   *F - имя исходного файла

$(B)/%.sh:
	@ $(guile (echo-install "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ install -m 755 $< $@

$(T)/%.sh:
	@ $(guile (echo-install "$@"))
	@ $(guile (ensure-path! "$(@D)")) 
	@ install -m 755 $< $@

# Группа правил для компиляции C/C++ исходников. Правила включаются, если
# определена переменная cc. Для работы правил нужны дополнительные переменные,
# в их определённости убеждается check-vars.

ifdef cc

vars = cc dep cstd cppstd cflags cdebug coptimization
$(guile (check-vars "C/C++ compilation group" "$(vars)"))

ifeq ($(DBG), Y)
copt = $(cdebug)
else
copt = $(coptimization)
endif

# o2d = $(patsubst %.o,%.d,$(1))
# c2o = $(addprefix $(1)/,$(patsubst %.c,%.o,$(2)))
# cpp2o = $(addprefix $(1)/,$(patsubst %.cpp,%.o,$(2)))

# $(bits)/%.d: %.c
# 	@ $(guile (echo-dep "$@"))
# 	@ $(guile (ensure-path! "$(@D)"))
# 	@ $(dep) $(cflags) -x c -std=$(cstd) -MM $< \
# 		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) > $@
#
# 
# $(bits)/%.d: %.cpp
# 	@ $(guile (echo-dep-c++ "$@")) 
# 	@ $(guile (ensure-path! "$(@D)"))
# 	@ $(dep) $(cflags) -x c++ -std=$(cppstd) -MM $< \
# 		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) > $@
# 
# $(bits)/%.d: $(bits)/%.c
# 	@ $(guile (echo-dep "$@")) 
# 	@ $(guile (ensure-path! "$(@D)"))
# 	@ $(dep) $(cflags) -x c -std=$(cstd) -MM -MG $< \
# 		| $(call sub,$(*F).o,$(@D)/$(*F).o $@) \
# 		| $(call sub,\s\+\([^/]\+\.h\)\s*, $(@D)/\1 ) > $@

# Компиляторы С/С++ вычисляют зависимости в довольно неудобной логике. Поэтому
# необходимо редактирование результатов. Это делает процедура fix-deps,
# пропуская через себя вывод указанной программы

$(bits)/%.d: %.c
	@ $(guile (echo-dep "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(guile (fix-deps "$(dep) $(cflags) -x c -std=$(cstd) -MG '$<'" "$@"))

$(bits)/%.d: %.cpp
	@ $(guile (echo-dep-c++ "$@")) 
	@ $(guile (ensure-path! "$(@D)"))
	@ $(guile (fix-deps "$(dep) $(cflags) -x c++ -std=$(cppstd) -MG '$<'" "$@"))

$(bits)/%.d: $(bits)/%.c
	@ $(guile (echo-dep/gen "$@")) 
	@ $(guile (ensure-path! "$(@D)"))
	@ $(guile (fix-deps "$(dep) $(cflags) -x c -std=$(cstd) -MG '$<'" "$@"))

$(bits)/%.o: %.c
	@ $(guile (echo-c "$@"))
	@ $(cc) -c $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(bits)/%.o: %.cpp
	@ $(guile (echo-c++ "$@"))
	@ $(cc) -c $(cflags) $(copt) -x c++ -std=$(cppstd) -o $@ $<

$(bits)/%.o: $(bits)/%.c
	@ $(guile (echo-c/gen "$@"))
	@ $(cc) -c $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(bits)/%.h: %.h
	@ $(guile (echo-h/gen "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ install -m 755 $< $@

# define headroute-m
# $(I)/$1/%.h: $2/%.h
# 	@ $(echo) '\theader\t$$@' \
# 	&& $(call mkpath,$(bdir),$$(@D)) \
# 	&& install -m 755 $$< $$@
# endef

endif # группа правил компиляции C/C++

# Группа правил для сборки объектных файлов. FIXME: почему отдельно от C/C++?

ifdef lnk # LiNK group

vars = lnk lflags ldebug loptimization ar
$(guile (check-vars "link group" "$(vars)"))

# $(call checkdefs,$(vars),LiNK group needs: $(vars))

ifeq ($(DBG), Y)
lflags += $(ldebug)
else
lflags += $(loptimization)
endif

$(B)/%:
	@ $(guile (echo-link "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(lnk) $^ -o $@ $(lflags) 

$(T)/%:
	@ $(guile (echo-link "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(lnk) $^ -o $@ $(lflags)

$(L)/%.a:
	@ $(guile (echo-lib "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(ar) cr $@ $^

endif # группа правил сборки объектных файлов

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

