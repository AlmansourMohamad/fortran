### Makefile ---

## Author: j.hoffmann@fh-aachen.de
## Version: $Id: Makefile,v 0.0 2016/02/11 18:43:26 ax006ho Exp $
## Keywords:
## X-URL:

# A simple Makefile for compiling and executing Fortran programs
FC = gfortran  # change the compiler name according to your needs
FFLAGS = -Wall

.SUFFIXES:
.SUFFIXES: .f .f90 .f95 .f03 .f08 .mod .o

# the basic rules for compiling and executing Fortran programs
%: %.f90
	$(FC) $(FFLAGS) $^ -o $@ && ./$@

%: %.f95
	$(FC) $(FFLAGS) $^ -o $@ && ./$@

%: %.f03
	$(FC) $(FFLAGS) $^ -o $@ && ./$@

%: %.f08
	$(FC) $(FFLAGS) $^ -o $@ && ./$@

### Makefile ends here
