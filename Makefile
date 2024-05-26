FC=gfortran
#FFLAGS=-ffree-form -O3 -Wall -Wextra
SRC=circles.f90
OBJ=${SRC:.f90=.o}
BASE=${SRC:.f90=}

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

circles: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	rm -f *.o *.mod $(BASE)
