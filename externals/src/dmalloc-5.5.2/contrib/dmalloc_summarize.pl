#!/usr/bin/perl5 -w
#
# dmalloc_summarize -- summarizes dmalloc log files
#
# Copyright 1997 by USC/ISI All rights reserved.
#                                                                
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation, advertising
# materials, and other materials related to such distribution and use
# acknowledge that the software was developed by the University of
# Southern California, Information Sciences Institute.  The name of
# the University may not be used to endorse or promote products
# derived from this software without specific prior written
# permission.
# 
# THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
# $Id: dmalloc_summarize.pl,v 1.1 1997/07/07 08:13:52 gray Exp $
# 

sub usage {
    print STDERR <<END;
usage: $0 [executable] < logfile

Post-process a dmalloc logfile (read from standard input).

If an EXECUTABLE is specified, unknown symbols will be translated
according to that executable.
END
    exit 1;
}

require 5.000;
use strict;
# needed for gdb communications
use IPC::Open2;

# process args
use Getopt::Long;
&usage if ($#ARGV >= 1 && $ARGV[0] eq '-?');
my($exe) = undef;
if ($#ARGV >= 0) {
    $exe = $ARGV[0];
};

# initialize counters
my($totcount, $totgross) = (0, 0);

######################################################################

#
# Store key into the hash or increment existing value
#
sub safe_inc {
  my($hashref, $key, $inc) = @_;
  if (defined($hashref->{$key})) {
    $hashref->{$key} += $inc;
  } else {
    $hashref->{$key} = $inc;
  };
}

######################################################################

my($gdb_pid);

#
# Start a gdb pipe to resolve unknown return addresses.
#
sub start_gdb {
  $SIG{'PIPE'} = sub { mydie("ERROR: premature end-of-data.\n"); };
  $gdb_pid = open2('GDB_RDR', 'GDB_WTR', 'gdb', '-nx', '-q', $exe) ||
    die "$0: cannot run gdb on $exe\n";
  # tidy things up
  # prompt becomes a magic number to look for
  print GDB_WTR "set prompt (gdb)\\n\n";
  print GDB_WTR "set print asm-demangle on\n";
  print GDB_WTR "set height 0\n";
}

#
# Huh?
#
sub never_called {
    <GDB_RDR>; <GDB_WTR>;   # hack for warnings
}

#
# Lookup an unknown ra-address with gdb
#
sub interpret_name {
  my($name) = @_;
  return $name if ($name !~ /^ra/);
  return $name if (!defined($exe));
  
  start_gdb() if (!defined($gdb_pid));
  
  ($a) = ($name =~ /ra=([0-9a-fA-FxX]+)/);
  return $name if (!defined($a));
  
  print GDB_WTR "info line *($a)\n";
  
  my($something) = undef;
  my($file_line, $function);
  while (<GDB_RDR>) {
    if (/^\(gdb\)$/) {
      last if ($something);
      next;   # skip prompts
    };
    $something = 1;
    if (/^Line (\d+) of "([^\"]+)"/) {
      $file_line = "$2:$1";
    };
    if (/\<(.*)\+\d+\>/) {
      $function = $1;
    };
  };
  my($n) = "";
  $n .= "$function " if (defined($function));
  $n .= "[$file_line] " if (defined($file_line));
  $n = $name if ($n eq '');
  return $n;
}

######################################################################

my(%allocers) = ();

# read the data
sub read_data {
  while (<STDIN>) {
    next if (! /\d:\s+not freed:\s+'([^\']+)'\s+\((\d+)\s+bytes\)\s+from\s+'([^\']*)'$/);
    my($pointer, $size, $allocer) = ($1, $2, $3);
    
    if (! defined($allocers{$allocer})) {
      $allocers{$allocer} = {};
      $allocers{$allocer}->{'sizes'} = {};
    };
    safe_inc($allocers{$allocer}, 'nsizes', 1)
      if (! defined($allocers{$allocer}->{'sizes'}{$size}));
    safe_inc($allocers{$allocer}->{'sizes'}, $size, 1);
    safe_inc($allocers{$allocer}, 'subcount', 1);
    safe_inc($allocers{$allocer}, 'subgross', $size);
    $totcount++;
    $totgross += $size;
  }
}

#
# Print the report line
#
sub form {
  printf "%10s %10s %10s %s\n", @_[1,2,3,0];
}

sub by_size {
  return $allocers{$b}->{'subgross'} <=> $allocers{$a}->{'subgross'};
}

#
# Dump out the report
#
sub print_report {
  form ('function', 'size', 'count', 'gross');
  form ('total', '', $totcount, $totgross);
  my($allocer, $size);
  
  foreach $allocer (sort by_size keys %allocers) {
    my($head) = interpret_name($allocer);
    my($sizes);
    if ($allocers{$allocer}->{'nsizes'} > 1) {
      my($subcount, $subgross) = ($allocers{$allocer}->{'subcount'},
				  $allocers{$allocer}->{'subgross'});
      form($head, 'subtotal', $subcount, $subgross);
      $head = "\"";
    };
    foreach $size (sort {$a<=>$b} keys %{$allocers{$allocer}->{'sizes'}}) {
      my($count) = $allocers{$allocer}->{'sizes'}{$size};
      my($gross) = $count * $size;
      form ($head, $size, $count, $gross);
      $head = "\"";
    };
  };
}

read_data;
print_report;

exit 0;
