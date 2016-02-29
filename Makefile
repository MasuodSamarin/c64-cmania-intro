.SILENT:

.PHONY: all clean build

X64 = x64

all: intro

SRC = src/intro.s 

intro: ${SRC}
	cl65 -d -g -Ln bin/$@.sym -o bin/$@-dev.prg -u __EXEHDR__ -t c64 -C intro.cfg $^
	$(X64) -moncommands bin/$@.sym bin/$@-dev.prg

clean:
	rm -f src/*.o bin/*.sym bin/*.prg
