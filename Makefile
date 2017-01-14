.SILENT:

.PHONY: all clean build

X64 = x64

all: intro

SRC = src/intro.s 

intro: ${SRC}
	cl65 -d -g -Ln bin/$@.sym -o bin/$@.prg -u __EXEHDR__ -t c64 -C intro.cfg $^
	exomizer sfx sys -x1 -Di_line_number=2016 -o bin/$@-dev.prg bin/$@.prg
	$(X64) -moncommands bin/$@.sym bin/$@.prg

clean:
	rm -f src/*.o bin/*.sym bin/*.prg
