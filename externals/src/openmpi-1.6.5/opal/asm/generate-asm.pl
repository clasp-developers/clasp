#!/usr/bin/perl -w


my $asmarch = shift;
my $asmformat = shift;
my $basedir = shift;
my $output = shift;

if ( ! $asmarch) { 
    print "usage: generate-asm.pl [ASMARCH] [ASMFORMAT] [BASEDIR] [OUTPUT NAME]\n";
    exit(1);
}

open(INPUT, "$basedir/$asmarch.asm") || 
    die "Could not open $basedir/$asmarch.asm: $!\n";
open(OUTPUT, ">$output") || die "Could not open $output: $1\n";

$CONFIG = "default";
$TEXT = "";
$GLOBAL = "";
$SUFFIX = "";
$GSYM = "";
$LSYM = "";
$TYPE = "";
$SIZE = 0;
$ALIGN_LOG = 0;
$DEL_R_REG = 0;
$IS64BIT = 0;

($CONFIG, $TEXT, $GLOBAL, $SUFFIX, $GSYM, $LSYM, $TYPE, $SIZE, $ALIGN_LOG, $DEL_R_REG, $IS64BIT, $GNU_STACK) = (
    $asmformat =~ /(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)\-(.*)/);

if (0) {
print "$asmformat\n";
print "CONFIG: $CONFIG\n";
print "TEXT: $TEXT\n";
print "GLOBAL: $GLOBAL\n";
print "SUFFIX: $SUFFIX\n";
print "GSYM: $GSYM\n";
print "LSYM: $LSYM\n";
print "GNU_STACK: $GNU_STACK\n";
}

my $current_func = "";
my $delete = 0;

# load our configuration
do "$basedir/$CONFIG.conf" or die "Could not open config file $basedir/$CONFIG.conf: $!\n";

while (<INPUT>) {
    s/TEXT/$TEXT/g;
    s/GLOBAL/$GLOBAL/g;
    s/REFGSYM\((.*)\)/$GSYM$1/g;
    s/REFLSYM\((.*)\)/$LSYM$1/g;
    s/GSYM\((.*)\)/$GSYM$1$SUFFIX/g;
    s/LSYM\((.*)\)/$LSYM$1$SUFFIX/g;

    if ($DEL_R_REG == 0) {
	s/cr([0-9][0-9]?)/$1/g;
        s/r([0-9][0-9]?)/$1/g;
    }

    if (/START_FILE/) {
        $_ = start_file();
    }

    if (/START_FUNC\((.*)\)/) {
        $current_func = $1;
        $_ = start_func($current_func);
    }

    if (/END_FUNC\((.*)\)/) {
        $current_func = $1;
        $_ = end_func($current_func);
    }

    if ($ALIGN_LOG == 0) {
        s/ALIGN\((\d*)\)/.align $1/g;
    } else {
        # Ugh...
        if (m/ALIGN\((\d*)\)/) {
            $val = $1;
            $result = 0;
            while ($val > 1) { $val /= 2; $result++ }
            s/ALIGN\((\d*)\)/.align $result/;
        }
    }

    if (/^\#START_64BIT/) {
        $_ = "";
        if ($IS64BIT == 0) {
            $delete = 1;
    }
    }
    if (/^\#END_64BIT/) { 
        $_ = "";
        $delete = 0; 
    }

    if ($delete == 0) {
        print OUTPUT $_;
    }
}

if ($GNU_STACK == 1) {
    if ($asmarch eq "ARM") {
        print OUTPUT "\n\t.section\t.note.GNU-stack,\"\",\%progbits\n";
    } else {
        print OUTPUT "\n\t.section\t.note.GNU-stack,\"\",\@progbits\n";
    }
}

close(INPUT);
close(OUTPUT);
