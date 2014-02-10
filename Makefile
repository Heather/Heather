# Note that it's not real makefile...
# e.g. it's real makefile
# but it designed for travis

.PHONY: all all_linux clean linux

all:        clean | h
all_linux:  clean | linux

linux: dosed | h

dosed:
	sed -i '/Win32/d' h.cabal

h:
	cabal-1.18 install --only-dependencies
	cabal-1.18 configure
	cabal-1.18 build

clean:
	@echo " --- Clean binaries --- "
	rm -f h
	@echo " --- Clean temp files --- "
	find . -name '*~' -delete;
	find . -name '#*#' -delete;