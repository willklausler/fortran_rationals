# Build, test and install fortran_rationals without fpm.
#
#   make                  build the static library
#   make test             build and run the unit tests
#   make example          build and run the example
#   make install          install library, module and pkg-config file
#   make uninstall        remove the installed files
#   make clean            remove the build directory
#
# Variables (override on the command line, e.g. `make FC=ifx FFLAGS=-O3`):
#   FC, FFLAGS            compiler and flags (gfortran, ifx, nvfortran)
#   PREFIX, DESTDIR       install location; DESTDIR stages a package

ifeq ($(origin FC),default)
  FC = gfortran
endif
FFLAGS  ?= -O2
PREFIX  ?= /usr/local
LIBDIR  ?= $(PREFIX)/lib
INCDIR  ?= $(PREFIX)/include
BUILD   ?= build/make
VERSION  = 0.1.0

ifneq (,$(findstring gfortran,$(FC)))
  MODFLAG = -J
else
  MODFLAG = -module
endif

LIB  = $(BUILD)/libfortran_rationals.a
OBJS = $(BUILD)/rationals.o
MODS = $(BUILD)/rationals.mod
PC   = $(BUILD)/fortran_rationals.pc

.PHONY: all test example install uninstall clean

all: $(LIB) $(PC)

$(BUILD):
	mkdir -p $@

$(BUILD)/rationals.o: src/rationals.f90 | $(BUILD)
	$(FC) $(FFLAGS) $(MODFLAG) $(BUILD) -c $< -o $@

$(LIB): $(OBJS)
	ar rcs $@ $^

$(PC): Makefile | $(BUILD)
	printf '%s\n' 'prefix=$(PREFIX)' 'libdir=$(LIBDIR)' 'includedir=$(INCDIR)' '' \
	  'Name: fortran_rationals' \
	  'Description: Rational number derived type with checked exact arithmetic' \
	  'Version: $(VERSION)' \
	  'Libs: -L$${libdir} -lfortran_rationals' \
	  'Cflags: -I$${includedir}' > $@

$(BUILD)/%: test/%.f90 $(LIB)
	$(FC) $(FFLAGS) -I$(BUILD) $< $(LIB) -o $@

$(BUILD)/%: example/%.f90 $(LIB)
	$(FC) $(FFLAGS) -I$(BUILD) $< $(LIB) -o $@

test: $(BUILD)/rationals_test
	$<

example: $(BUILD)/rationals_example
	$<

install: all
	install -d $(DESTDIR)$(LIBDIR) $(DESTDIR)$(LIBDIR)/pkgconfig $(DESTDIR)$(INCDIR)
	install -m 644 $(LIB) $(DESTDIR)$(LIBDIR)
	install -m 644 $(MODS) $(DESTDIR)$(INCDIR)
	install -m 644 $(PC) $(DESTDIR)$(LIBDIR)/pkgconfig

uninstall:
	rm -f $(DESTDIR)$(LIBDIR)/libfortran_rationals.a \
	      $(DESTDIR)$(LIBDIR)/pkgconfig/fortran_rationals.pc \
	      $(addprefix $(DESTDIR)$(INCDIR)/,$(notdir $(MODS)))

clean:
	rm -rf $(BUILD)
