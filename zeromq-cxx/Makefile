# ZMQ=$(HOME)/zeromq

SOURCES=$(wildcard *.cc)
BINARIES=$(SOURCES:.cc=)

all: $(BINARIES)

ifneq ($(MAKECMDGOALS),clean)
STARTUP_COMMAND := $(shell \
        rm -f .depend ; \
        echo "[DEPEND]" >&2; \
        gcc -MM $(SOURCES) > .depend \
        )
include .depend
endif

# %: %.o
# 	g++ -o $@ $< -L$(ZMQ)/lib -lzmq -Wl,-rpath,$(ZMQ)/lib
# %.o: %.cc
# 	g++ -Wall -O3 -c $< -I$(ZMQ)/include

%: %.o
	g++ -o $@ $< -lzmq
%.o: %.cc
	g++ -Wall -O3 -c $<

# Disable the default rule:
%: %.cc

clean:
	rm -f $(BINARIES) .depend *.o

.PHONY: all clean
