29 November, 2016

This is the Chimaera text adventure game as retrieved from a PRIMOS
distribution tape.  The unix wrappings are supposed to convert the
idiomatic Prime FORTRAN code (an f66 dialect with various extensions
and "special" behavior) so it will work with gfortran.

The fixes applied are not yet complete.

Some system library functions still need to be written.

The program was written to use the Glaxo Research library, which is
present.  But some of it is written in assembler, so those routines
need to be replaced too, and they're not written either.

See, for documentation of the Prime FTN compiler and system library:

http://bitsavers.trailing-edge.com/pdf/prime/fdr/FDR3057_FortranPgmGuide_1979.pdf

http://bitsavers.trailing-edge.com/pdf/prime/doc/DOC3621-190_PrimosSubrRef_19.4_Apr85.pdf
