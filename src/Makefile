# Project: fortran-maze-solber
# Author: Jessy Williams (jkwill87)
# Contact: jessy@jessywilliams.com
# Created: Friday, February 5, 2016

FC = gfortran
FCFLAGS = -cpp -Wall -O2 -std=f2003

fms: fms_types.o fms_msg.o fms_solve.o fms.o
	$(FC) -o fms fms_types.o fms_msg.o fms_solve.o fms.o

fms_types.o: fms_types.f90
	$(FC) -c $(FCFLAGS) fms_types.f90

fms_msg.o: fms_types.mod fms_msg.f90
	$(FC) -c $(FCFLAGS) fms_msg.f90

fms_solve.o: fms_msg.mod fms_types.mod fms_solve.f90
	$(FC) -c $(FCFLAGS) fms_solve.f90
	
fms.o: fms_msg.mod fms_types.mod fms_solve.mod fms.f90
	$(FC) -c $(FCFLAGS) fms.f90

clean:
	rm -r -f *.o *.mod *.dSYM *.exe fms a.out
