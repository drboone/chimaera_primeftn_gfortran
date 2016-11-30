#!/usr/bin/perl
# primeftnxlate.pl, Boone, 11/29/16
# Translate Prime FTNisms for gfortran
#
# Modifications:
# 11/29/16 Boone      Initial coding
# End Modifications

# Files

	$inf = shift;
	$outf = shift;

# Read input

	open(IN, $inf) ||
		die "usage: $0 infile outfile";
	@lines = <IN>;
	close(IN);

# Simple string substitutions

	s/\/\*/\!/g								for (@lines);
	s/\*\//  /g								for (@lines);
	/^\$INSERT/ && s/$/\"/					for (@lines);
	s/SYSCOM>//								for (@lines);
	s/^\$INSERT /\#include \"/				for (@lines);
	s/\.INS\.FTN/.INS.f/					for (@lines);
	s/KEYS\.F/KEYS.INS.f/					for (@lines);
	s/ERRD\.F/ERRD.INS.f/					for (@lines);
	s/G\$KEYS/G_KEYS/						for (@lines);
	s/:([0-7]+)/oct16tosigned10($1)/eg		for (@lines);
	s/\s+$/\n/g								for (@lines);
	s/DECODE\(([^,]+),([^,]+),([^,]+),ERR=(\d+)\) (.*)/READ(UNIT=$3,ERR=$4) $5/
											for (@lines);

# Delete a few things

	@lines = grep (!/NOLIST/, @lines);
	@lines = grep (!/LIST/, @lines);

# Multiple-line handling

	$block = join('', @lines);

	$block =~ s/^(\s+)PARAMETER(.*\n)((.....[X\+0-9].*\n|C.*\n)+)/\1PARAMETER \(\2\3     +\)\n/mg;

#use re 'debugcolor';

	$block =~ s/(^\s+(SUBROUTINE|\w FUNCTION).*\n)((.....\+.*\n)*)((#include.*\n|C.*\n)+)(\s+IMPLICIT.*\n)/$1$3$7$5/mg;

# Write output

	open(OUT, '>', $outf) ||
		die "unable to open output $outf: $!";
	print OUT $block;
	close(OUT);

# Done

	exit(0);

###############################################################################
# 16-bit octal conversion, signed
###############################################################################

sub oct16tosigned10
{
	my $oct = shift;
	my $dec;

	$dec = oct($oct);
	if ($dec > 32767)
	{
		$dec = -(32768 - oct($oct));
	}
	else
	{
		return $dec;
	}
}
