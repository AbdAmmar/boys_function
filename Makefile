FC=gfortran
FFLAGS=-O2 
SRC=call_test.f90 cpx_boys.f90 cpx_erf.f90
OBJ=${SRC:.f90=.o} 

%.o: %.f90 
	$(FC) $(FFLAGS) -o $@ -c $<

test: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) 

clean: #cleans all the old compilation files
	@rm -f *.mod *.o maths
