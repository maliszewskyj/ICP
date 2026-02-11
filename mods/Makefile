FC = gfortran
FFLAGS = -ffree-line-length-132 -ffixed-line-length-132
MODS = asd.o histo.o dartz.o eventflag.o focus.o globals.o magnmodel.o
MODS+= icpsig.o motors.o polar.o polfig.o refl.o smg_prep.o
MODS+= tc.o tc_careful.o tdb.o trj.o vdata.o virtmot.o xray.o zcoeffs.o
MODS+= homedir.o hwlong.o lia.o ad201.o fndlam.o instr.o

all: mods

mods: $(MODS)

.F.o:
	$(FC) $(FFLAGS) -c $< 

clean:
	rm -f *~ *.mod *.o
