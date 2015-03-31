FC = gfortran
FFLAGS = -Wall -Wextra -O3 -march=native -ffast-math #-std=f95
LDFLAGS =

COMPILE = $(FC) $(FFLAGS)
LINK = $(FC) $(LDFLAGS)

OBJS =
OBJS += lattice_init.o
OBJS += cluster.o
OBJS += calculations.o
OBJS += myprog.o
all: myprog

myprog: $(OBJS)
	$(LINK) -o $@ $^ $(LIBS)

%.o: %.f90
	$(COMPILE) -o $@ -c $<

.PHONY: clean
clean:
	$(RM) myprog $(OBJS) *.mod *.txt
