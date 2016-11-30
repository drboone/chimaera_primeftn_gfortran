# Makefile, Boone, 11/29/16
# Build Chimaera from Prime FTN sources, for *ix

.PRECIOUS:	%.f

%.f:	%.FTN
	perl primeftnxlate.pl $< $@
	
%.o:	%.f KEYS.INS.f ERRD.INS.f G_KEYS.INS.f CHIMAERA.INS.f
	gfortran -std=legacy -fdollar-ok -cpp -o $@ $<

chimaera:	CHIMAERA.o GGRSUB.o

G_KEYS.INS.FTN:	G$$KEYS.INS.FTN
	cp 'G$$KEYS.INS.FTN' G_KEYS.INS.FTN

clean:
	rm -f *.f *.o chimaera G_KEYS.INS.FTN
