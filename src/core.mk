# Удобнее написать вспомогательные функции на более дружелюбном языке, чем язык
# функций make с асинхронной (совсем не RiDE) семантикой. Использую встроенную
# поддержку Guile Scheme

$(guile (load "$(dir $(lastword $(MAKEFILE_LIST)))core.scm"))

# Сперва загружаем файл с настройками инструментария, в нём могут быть
# определены и другие полезные переменные

ifndef TCN
TCN := toolchain
endif

include $(guile (tcn-path "$(TCN)"))

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


# FIXME: это хак, чтобы согласовать переменные в guile и make. Автоматически это
# согласование плохо работает, при возврате строк из guile для обработки в make.

B := $(guile B)
T := $(guile T)
L := $(guile L)
I := $(guile I)
D := $(guile D)

#FIXME: возможно, эта переменная больше не нужна

bits := $(guile bits)

# Преобразование списка слов make (не выяснил достоверно, что является словами
# в make) в список строк для guile
word-list = (list $(foreach w,$(1),"$(w)"))

# О специальных переменных в make:
#
#   @  - путь до текущей цели
#   @D - имя директории текущей цели
#   *F - имя исходного файла
#   ^  - все предпосылки
#   <  - первая предпосылка

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
	@ $(guile (ensure-path! "$(@D)"))
	@ $(cc) -c $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(bits)/%.o: %.cpp
	@ $(guile (echo-c++ "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(cc) -c $(cflags) $(copt) -x c++ -std=$(cppstd) -o $@ $<

$(bits)/%.o: $(bits)/%.c
	@ $(guile (echo-c/gen "$@"))
	@ $(cc) -c $(cflags) $(copt) -x c -std=$(cstd) -o $@ $<

$(bits)/%.h: %.h
	@ $(guile (echo-h/gen "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ install -m 755 $< $@

endif # группа правил компиляции C/C++

# Группа правил для сборки объектных файлов. FIXME: почему отдельно от C/C++?

ifdef lnk

vars = lnk lflags ldebug loptimization ar
$(guile (check-vars "link group" "$(vars)"))

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

# Группа правил для низкоуровневых сборок

ifdef objcopy

$(guile (check-vars "low level builds" "objcopy as"))

$(B)/%.elf:
	@ $(guile (echo-elf "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(lnk) $^ -o $@ -Wl,-Map=$@.map $(lflags)

$(B)/%.bin: $(B)/%.elf
	@ $(guile (echo-bin "$@"))
	@ $(objcopy) -O binary $< $@

$(B)/%.hex: $(B)/%.elf
	@ $(guile (echo-hex "$@"))
	@ $(objcopy) -O ihex $< $@

$(T)/%.elf:
	@ $(guile (echo-elf "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(lnk) -Wl,-Map=$@.map $^ $(lflags) -o $@

$(T)/%.bin: $(T)/%.elf
	@ $(guile (echo-bin "$@"))
	@ $(objcopy) -O binary $< $@

$(T)/%.hex: $(T)/%.elf
	@ $(guile (echo-hex "$@"))
	@ $(objcopy) -O ihex $< $@

$(L)/%.o:
	@ $(guile (echo-o "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ install -m 755 $< $@

$(bits)/%.o: %.s
	@ $(guile (echo-asm "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(as) -o $@ $<

endif # группа правил для низкоуровневых сборок

# Группа правил для flex/yacc. FIXME: пока не трогаю

ifdef yacc

vars := yacc lex
$(guile (check-vars "Flex/YACC group" "$(vars)"))

$(bits)/%.lex.c: %.l
	@ $(echo) '\tlex\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(lex) -o $@ $<

$(bits)/%.tab.c $(bits)%.tab.h: %.y
	@ $(echo) '\tyacc\t$@' \
	&& $(call mkpath,$(bdir),$(@D)) \
	&& $(yacc) -d -b '$(bits)/$*' $<

endif # группа правил flex/yacc

# Группа правил для LaTeX и XeLaTeX

ifdef tex 

vars = tex xtex biber bibtex bib-engine texcode
$(guile (check-vars "{Xe}LaTeX group" "$(vars)"))

$(bits)/%.tex: %.tex
	@ $(guile (echo-tex/cnv "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ iconv -t $(texcode) < $< > $@

$(bits)/%.xtex: %.xtex
	@ $(guile (echo-xtex/cp "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ cp $< $@

$(bits)/%.sty: %.sty
	@ $(guile (echo-sty/cnv "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ iconv -t $(texcode) < $< > $@

$(bits)/%.bib: %.bib
	@ $(guile (echo-bib/cnv "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ iconv -t $(texcode) < $< > $@

$(bits)/%.png: %.png
	@ $(guile (echo-cp "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ cp $< $@

# Такая структура правила необходима, чтобы выводить сообщения об ошибках в
# другой локали. Запуск biber -- дорогая операция, поэтому запускается по
# необходимости: изменились ссылки в tex-файле или изменилась одна из
# bib-предпосылок. Такой анализ проводит процедура biberize!, для чего ей и
# нужен список всех предпосылок.

$(bits)/%.pdf: $(bits)/%.tex
	{ cd $(@D)                     \
			&& $(guile (bibify! "$^"))   \
			&& $(guile (echo-tex "$@"))  \
			&& $(tex) $(<F) >"$@".out    \
			&& $(guile (bibify-end!)); } \
		|| { iconv -cf $(texcode) $(guile (tex-log "$@")); exit -1; }

$(bits)/%.pdf: $(bits)/%.xtex
	@ $(guile (echo-xtex "$@"))
	@ { cd $(@D) && $(xtex) $(<F) && $(guile (bibify! "$^")); } >"$@".out \
		|| { cat $(guile (tex-log "$@")); exit -1; }

$(D)/%.pdf:
	@ $(guile (echo-cp "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ cp $< $@

endif # группа правил {Xe}LaTeX

# Группа правил преобразований markdown

ifdef md2pdf

$(guile (check-vars "Markdown translation group" "m2pdf"))

$(bits)/%.pdf: %.md
	@ $(guile (echo-md "$@"))
	@ $(guile (ensure-path! "$(@D)"))
	@ $(md2pdf) -o $@ $<

endif # группа правил для преобразований markdown
