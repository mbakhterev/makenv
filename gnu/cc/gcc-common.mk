tcc = gcc

ifdef ttarget
cctarget += -b $(ttarget)
endif

ifdef tcversion
cctarget += -V $(tversion)
endif

undefine ttarget
undefine tversion

ar = ar
