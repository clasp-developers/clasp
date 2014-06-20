#!/usr/bin/env perl

# Copyright (c) 2010 Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This script provides a way to modify the configure script using regular
# expressions, as opposed to a context diff which uses hardcoded context
# patterns. This is useful since the contents of the output configure script are
# not always known before hand. (Note: the script doesn't actually invoke the
# "patch" command)

use strict;
use File::Basename;
use Getopt::Long;
use Data::Dumper;

# Grab the name of this script
my $program = basename($0);

my $file_arg = "./configure";
my $help_arg;
my $debug_arg;

&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions(
    "debug|d" => \$debug_arg,
    "file|f=s" => \$file_arg,
    "help|h" => \$help_arg,
);

# Help argument
if ($help_arg) {
    print "
 $program -
  -f|--file    File to patch (default: $file_arg)
  -d|--debug   Keep original copy of configure for debugging
  -h|--help    This help menu
";
    exit;
}

# Call the main subroutine and exit
&modify_configure_for_sun_fortran($file_arg);
exit;

sub modify_configure_for_sun_fortran {
    my ($file) = @_;

    # By default, patch the libtool script in the cwd
    $file = $file_arg if (! -e $file);

    # Keep backup copies of the file lying around for debugging
    # purposes
    if (defined($debug_arg)) {
        my $rand = RandomString(10);
        my $cmd = "cp $file $file.$rand\n";
        system($cmd);
    }

    # Read in the libtool script file
    my $contents = Read($file);
    die("Couldn't Read $file!\n") if (!$contents);

    # Note: we have to use octal escapes to match '*Sun\ F*) and the 
    # four succeeding lines in the bourne shell switch statement.
    #   \ = 134
    #   ) = 051
    #   * = 052
    #
    # Below is essentially an upstream patch for Libtool which we want 
    # made available to Open MPI users running older versions of Libtool
    foreach my $tag (("", "_F77", "_FC")) {

        # We have to change the search pattern and substitution on each
        # iteration to take into account the tag changing
        my $search_string = '\052Sun\134 F\052.*\n.*\n\s+' .
            "lt_prog_compiler_pic${tag}" . '.*\n.*\n.*\n.*\n';
        my $replace_string = "
        *Sun\\ Ceres\\ Fortran* | *Sun*Fortran*\\ [[1-7]].* | *Sun*Fortran*\\ 8.[[0-3]]*)
          # Sun Fortran 8.3 passes all unrecognized flags to the linker
          lt_prog_compiler_pic${tag}='-KPIC'
          lt_prog_compiler_static${tag}='-Bstatic'
          lt_prog_compiler_wl${tag}=''
          ;;
        *Sun\\ F* | *Sun*Fortran*)
          lt_prog_compiler_pic${tag}='-KPIC'
          lt_prog_compiler_static${tag}='-Bstatic'
          lt_prog_compiler_wl${tag}='-Qoption ld '
          ;;
        ";

        print("=== Patching configure for Sun Studio Fortran version strings ($tag)\n");
        $contents =~ s/$search_string/$replace_string/;
    }

    # Write changed file out
    Write($file, $contents);
}

sub Read {
    my ($file) = @_;

    my $contents;
    open (INPUT, $file) or warn "Can't open $file: $!";
    while (<INPUT>) {
        $contents .= $_;
    }
    close(INPUT) or warn "Can't close $file: $!";
    return $contents;
}

sub Write {
    my ($filename, $body) = @_;

    # Write out the file
    die("Failed to write to file: $!") if (! open(FILE, "> $filename"));

    print FILE $body;
    close FILE;
}

my $_seeded;
sub RandomString {
    # length of the random string to generate
    my $length_of_randomstring = shift;

    # Need something at least sorta random -- doesn't have to be
    # entirely unique (see "srand" in perlfunc(1))
    if (!$_seeded) {
        srand(time() ^ $$ ^ unpack "%L*", `hostname`);
        $_seeded = 1;
    }

    my @chars = ('a'..'z','A'..'Z','0'..'9','_');
    my $random_string;

    foreach (1..$length_of_randomstring) {
        $random_string .= $chars[rand @chars];
    }
    return $random_string;
}
#!/usr/bin/env perl

# Copyright (c) 2010 Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This script provides a way to modify the configure script using regular
# expressions, as opposed to a context diff which uses hardcoded context
# patterns. This is useful since the contents of the output configure script are
# not always known before hand. (Note: the script doesn't actually invoke the
# "patch" command)

use strict;
use File::Basename;
use Getopt::Long;
use Data::Dumper;

# Grab the name of this script
my $program = basename($0);

my $file_arg = "./configure";
my $help_arg;
my $debug_arg;

&Getopt::Long::Configure("bundling");
my $ok = Getopt::Long::GetOptions(
    "debug|d" => \$debug_arg,
    "file|f=s" => \$file_arg,
    "help|h" => \$help_arg,
);

# Help argument
if ($help_arg) {
    print "
 $program -
  -f|--file    File to patch (default: $file_arg)
  -d|--debug   Keep original copy of configure for debugging
  -h|--help    This help menu
";
    exit;
}

# Call the main subroutine and exit
&modify_configure_for_sun_fortran($file_arg);
exit;

sub modify_configure_for_sun_fortran {
    my ($file) = @_;

    # By default, patch the libtool script in the cwd
    $file = $file_arg if (! -e $file);

    # Keep backup copies of the file lying around for debugging
    # purposes
    if (defined($debug_arg)) {
        my $rand = RandomString(10);
        my $cmd = "cp $file $file.$rand\n";
        system($cmd);
    }

    # Read in the libtool script file
    my $contents = Read($file);
    die("Couldn't Read $file!\n") if (!$contents);

    # Note: we have to use octal escapes to match '*Sun\ F*) and the 
    # four succeeding lines in the bourne shell switch statement.
    #   \ = 134
    #   ) = 051
    #   * = 052
    #
    # Below is essentially an upstream patch for Libtool which we want 
    # made available to Open MPI users running older versions of Libtool
    foreach my $tag (("", "_F77", "_FC")) {

        # We have to change the search pattern and substitution on each
        # iteration to take into account the tag changing
        my $search_string = '\052Sun\134 F\052.*\n.*\n\s+' .
            "lt_prog_compiler_pic${tag}" . '.*\n.*\n.*\n.*\n';
        my $replace_string = "
        *Sun\\ Ceres\\ Fortran* | *Sun*Fortran*\\ [[1-7]].* | *Sun*Fortran*\\ 8.[[0-3]]*)
          # Sun Fortran 8.3 passes all unrecognized flags to the linker
          lt_prog_compiler_pic${tag}='-KPIC'
          lt_prog_compiler_static${tag}='-Bstatic'
          lt_prog_compiler_wl${tag}=''
          ;;
        *Sun\\ F* | *Sun*Fortran*)
          lt_prog_compiler_pic${tag}='-KPIC'
          lt_prog_compiler_static${tag}='-Bstatic'
          lt_prog_compiler_wl${tag}='-Qoption ld '
          ;;
        ";

        print("=== Patching configure for Sun Studio Fortran version strings ($tag)\n");
        $contents =~ s/$search_string/$replace_string/;
    }

    # Write changed file out
    Write($file, $contents);
}

sub Read {
    my ($file) = @_;

    my $contents;
    open (INPUT, $file) or warn "Can't open $file: $!";
    while (<INPUT>) {
        $contents .= $_;
    }
    close(INPUT) or warn "Can't close $file: $!";
    return $contents;
}

sub Write {
    my ($filename, $body) = @_;

    # Write out the file
    die("Failed to write to file: $!") if (! open(FILE, "> $filename"));

    print FILE $body;
    close FILE;
}

my $_seeded;
sub RandomString {
    # length of the random string to generate
    my $length_of_randomstring = shift;

    # Need something at least sorta random -- doesn't have to be
    # entirely unique (see "srand" in perlfunc(1))
    if (!$_seeded) {
        srand(time() ^ $$ ^ unpack "%L*", `hostname`);
        $_seeded = 1;
    }

    my @chars = ('a'..'z','A'..'Z','0'..'9','_');
    my $random_string;

    foreach (1..$length_of_randomstring) {
        $random_string .= $chars[rand @chars];
    }
    return $random_string;
}
