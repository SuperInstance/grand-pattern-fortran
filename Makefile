# Grand Pattern Fibonacci Dual-Direction Architecture - Fortran
FC = gfortran
FFLAGS = -std=f2018 -Wall -Wextra -O2 -Jmod
SRCDIR = src
TESTDIR = tests

SRCS = $(SRCDIR)/types.f90 $(SRCDIR)/ops.f90
TEST_SRC = $(TESTDIR)/test_gp.f90

.PHONY: all test clean

all: test_gp

mod:
	mkdir -p mod

$(SRCDIR)/types.o: $(SRCDIR)/types.f90 | mod
	$(FC) $(FFLAGS) -c $< -o $@

$(SRCDIR)/ops.o: $(SRCDIR)/ops.f90 $(SRCDIR)/types.o | mod
	$(FC) $(FFLAGS) -c $< -o $@

test_gp: $(TEST_SRC) $(SRCDIR)/types.o $(SRCDIR)/ops.o | mod
	$(FC) $(FFLAGS) -o $@ $^

test: test_gp
	./test_gp

clean:
	rm -f *.o mod/*.mod test_gp $(SRCDIR)/*.o
