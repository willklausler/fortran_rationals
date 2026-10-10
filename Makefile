# make(1) build, an alternative to fpm.
#   make                  library, tests, examples and app
#   make test             run the tests
#   make FC=ifx FFLAGS=-O2
#   make install PREFIX=$HOME/.local
#   make doc              HTML documentation in build/doc (needs ford)

ifeq ($(origin FC),default)
FC = gfortran
endif
FFLAGS ?= -O2
PREFIX ?= /usr/local

B := build/make
LIB := $(B)/libfortran_rationals.a
PROGS := $(B)/test_rationals $(B)/demo_arithmetic $(B)/demo_approximation $(B)/hilbert_inverse

.PHONY: all test install doc clean

all: $(LIB) $(PROGS)

# Compiling inside $(B) puts the .mod files there for every compiler.
$(LIB): src/rationals.f90
	mkdir -p $(B)
	cd $(B) && $(FC) $(FFLAGS) -c $(CURDIR)/$< -o rationals.o
	ar rcs $@ $(B)/rationals.o

LINK = $(FC) $(FFLAGS) -I$(B) $< $(LIB) -o $@
$(B)/test_rationals: test/test_rationals.f90 $(LIB)
	$(LINK)
$(B)/demo_%: example/demo_%.f90 $(LIB)
	$(LINK)
$(B)/hilbert_inverse: app/main.f90 $(LIB)
	$(LINK)

test: $(B)/test_rationals
	$<

install: $(LIB)
	install -d $(PREFIX)/lib $(PREFIX)/include $(PREFIX)/share/man/man3
	install -m 644 $(LIB) $(PREFIX)/lib
	install -m 644 $(B)/*.mod $(PREFIX)/include
	install -m 644 man/rationals.3 $(PREFIX)/share/man/man3

# The user manual page is llms.txt, so there is one reference to maintain.
doc:
	mkdir -p build/pages
	{ printf -- '---\ntitle: User manual\n---\n\n'; sed 1d llms.txt; } > build/pages/index.md
	ford ford.md
	cp llms.txt build/doc/

clean:
	rm -rf $(B)
