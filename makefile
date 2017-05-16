FC=gfortran
subjects=extract_seed.o abstime.o decimate.o rm_resp.o avg.o sacio.o
subjects1=extract_eq_seed.o abstime1.o decimate.o rm_resp.o avg.o sacio.o
subjects2=extract_seed_short.o abstime.o decimate.o rm_resp.o avg.o sacio.o
all:sacio.mod extract_seed extract_eq extract_seed_short
sacio.mod:sacio.f90
	$(FC) -c $^
extract_seed:$(subjects)
	$(FC) $^ -o $@ 
extract_seed_short:$(subjects2)
	$(FC) $^ -o $@ 
extract_eq:$(subjects1)
	$(FC) $^ -o $@ -fbounds-check
%.o:%.f90
	$(FC) $^ -c
clean:
	rm *.o *.mod
install:
	cp extract_seed extract_seed_short extract_eq ../bin
