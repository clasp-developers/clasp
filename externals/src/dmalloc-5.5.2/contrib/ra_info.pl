#!/usr/bin/perl
#
# script to run gdb on return-addresses
# Usage: $0 malloc-log-file binary
#
# Copyright 2000 by Gray Watson
#
# This file is part of the dmalloc package.
#
# Permission to use, copy, modify, and distribute this software for
# any purpose and without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies, and that the name of Gray Watson not be used in advertising
# or publicity pertaining to distribution of the document or software
# without specific, written prior permission.
#
# Gray Watson makes no representations about the suitability of the
# software described herein for any purpose.  It is provided "as is"
# without express or implied warranty.
#
# The author may be contacted via http://dmalloc.com/
#
# $Id: ra_info.pl,v 1.7 2000/11/20 17:44:06 gray Exp $
#

#
# Use this Perl script to run gdb and get information on the return-address
# (ra) addresses from a dmalloc logfile output.  This will search for
# any ra= lines and will examine them and try to get the line number.
#
# NOTE: you may want to direct the output from the script to a file
# else gdb seems to prompt for a return like you are on the end of a
# page.
#
# Be sure to send me mail if there is an easier way to do all this.
#

###############################################################################
# usage message
#
if (@ARGV != 2 ) {
  die "Usage:  $0  dmalloc-log  binary-that-generated-log\n";
}

$malloc = @ARGV[0];
$command = @ARGV[1];

open(malloc, "<" . $malloc) || die "Could not open $malloc: $!\n";
open (gdb, "|gdb -nx -q $command") || die "Could not run gdb: $!\n";
$| = 1;

# get rid of the (gdb)
printf (gdb "set prompt\n");
printf (gdb "echo \\n\n");

# load in the shared libraries
printf (gdb "sharedlibrary\n");
printf (gdb "add-shared-symbol-files\n");

while ( <malloc> ) {
  $count = 0;
  $line = $_;
  while ( $line =~ m,ra=0x[0-9a-fA-F]+, ) {
    $count++;
    $address = $_;
    $address =~ s/.*ra=(0x[0-9a-fA-F]+).*\n?/\1/;
    $line =~ s/ra=0x[0-9a-fA-F]+//;
    
    printf (gdb "echo -----------------------------------------------\\n\n");
    printf (gdb "echo Address = '%s' line %d num %d\\n\n",
	    $address, $., $count);
    printf (gdb "x %s\n", $address);
    printf (gdb "info line *(%s)\n", $address);
  }
}
$| = 0;

close(gdb);
close(malloc);
