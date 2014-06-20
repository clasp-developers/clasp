#!/usr/bin/perl -w

my $perl = shift;
my $srcdir = shift;
my $destdir = shift;

if (! $perl || ! $srcdir || ! $destdir) {
    print "ERROR: invalid argument to generate-all-asm.pl\n";
    print "usage: generate-all-asm.pl [PERL] [SRCDIR] [DESTDIR]\n";
    exit 1;
}

open(DATAFILE, "$srcdir/asm-data.txt") || die "Could not open data file: $!\n";

my $ASMARCH = "";
my $ASMFORMAT = "";
my $ASMFILE = "";

while(<DATAFILE>) {
    if (/^#/) { next; }
    ($ASMARCH, $ASMFORMAT, $ASMFILE) = /(.*)\t(.*)\t(.*)/;
    if (! $ASMARCH || ! $ASMFORMAT) { next; }

    print "--> Generating assembly for \"$ASMARCH\" \"$ASMFORMAT\"\n";
    system("$perl \'$srcdir/generate-asm.pl\' \'$ASMARCH\' \'$ASMFORMAT\' \'$srcdir/base\' \'$destdir/generated/atomic-$ASMFILE.s\'");

}
