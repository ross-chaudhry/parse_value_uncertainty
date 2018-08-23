# -- Build options set for gfortran 8.2
FC = gfortran
FCFLAGS = -Og -Wall -Wcharacter-truncation -Wimplicit-interface -Wunused-parameter -fcheck=all -std=f2008 -pedantic -fbacktrace

PROGRAMS = self_test_driver
all: $(PROGRAMS)

# -- Add extra dependencies
self_test_driver self_test_driver.o: parse_value_uncertainty.o

# -- Generic rules for building
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%: %.o
	$(FC) $(FCFLAGS) $^ -o $@

.PHONY: check test-pdf clean distclean
check:
	./self_test_driver

test-pdf:
	pdflatex pvu_test.tex

clean:
	rm -f *.o *.mod
	rm -f pvu_test.aux pvu_test.log pvu_test.pdf pvu_test.tex

distclean:
	$(MAKE) clean
	rm -f $(PROGRAMS)
