29 November 2016 /drb

This is the Chimaera text adventure game as retrieved from a PRIMOS
distribution tape.  The unix wrappings are supposed to convert the
idiomatic Prime FORTRAN code (an f66 dialect with various extensions
and "special" behavior) so it will work with gfortran.

The fixes applied are not yet complete.

Some system library functions still need to be written.

The program was written to use the Glaxo Research library, which is
present.  But some of it is written in assembler, so those routines
need to be replaced too, and they're not written either.

There are a number of large issues remaining:

*	Type of library routines not matching the way the program code is
	seen by the compiler.

*	gfortran doesn't like assigning strings to INTEGER*2 arrays, very
	common in Prime FTN code.
*	The hack I inserted to rewrite DECODEs into READs on internal
	strings won't work on INTEGER*2 arrays; apparently it wants real
	CHARACTERs.
*	Out-of-bounds array references when using some of the O$* keys.
*	Lots of type mismatches, some of which look like assumed IMPLICITs
	that aren't actually in the code.

I'm starting to think that the hack of scripting all of the required
edits isn't viable.  Better perhaps to generate a large diff (possibly
with the help of the existing translator script), and apply that from
the Makefile.

See, for documentation of the Prime FTN compiler and system library:

http://bitsavers.trailing-edge.com/pdf/prime/fdr/FDR3057_FortranPgmGuide_1979.pdf
http://bitsavers.trailing-edge.com/pdf/prime/doc/DOC3621-190_PrimosSubrRef_19.4_Apr85.pdf
