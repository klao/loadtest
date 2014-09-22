SOURCES=$(wildcard *.hs)
BINARIES=$(SOURCES:.hs=)

all: $(BINARIES)

%: %.hs FORCE
	ghc -Wall -O -i../lib --make $@

clean:
	rm -f $(BINARIES) .depend *.o *.hi
	rm -f ../lib/*.{o,hi}

.PHONY: all clean FORCE
