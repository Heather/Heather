# Note that it's not real makefile...
# e.g. it's real makefile
# but it designed for travis

.PHONY: all all_linux clean linux

all:        clean | sharingan
all_linux:  clean | linux

linux: dosed | sharingan

dosed:
	sed -i '/Win32/d' sharingan.cabal

sharingan:
	cabal-1.18 install --only-dependencies
	cabal-1.18 configure
	cabal-1.18 build

clean:
	@echo " --- Clean binaries --- "
	rm -f sharingan
	@echo " --- Clean temp files --- "
	find . -name '*~' -delete;
	find . -name '#*#' -delete;