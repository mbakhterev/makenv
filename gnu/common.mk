ifndef foreign
$(error foreign root dir isn't defined)
endif

ifndef toolchain
$(error C toolchain isn't defined)
endif

ifndef bld
$(error build root dir isn't defined)
endif

include $(foreign)/mkenv/gnu/cc/$(toolchain).mk
include $(foreign)/mkenv/gnu/tex/texlive.mk

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

bldpath = $(bld)/$(dir $(lastword $(MAKEFILE_LIST)))

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

$(bld)/%.pdf: $(bld)/%.tex
	@ echo -e '\ttex\t$<' \
	&& (cd $(@D) && ($(tex) $< && $(tex) $<) | iconv -f $(texcode))

$(bld)/%.tex: %.tex
	@ echo -e '\tcp\t$<' \
	&& $(call mkpath,$(bld),$(@D)) \
	&& iconv -t $(texcode) < $< > $@
