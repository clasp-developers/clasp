#! /bin/csh -f
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

set srcdir="$1"
set builddir="`pwd`"
set distdir="$builddir/$2"
set OMPI_VERSION="$3"
set OMPI_SVN_VERSION="$4"

if ("$distdir" == "") then
    echo "Must supply relative distdir as argv[2] -- aborting"
    exit 1
elif ("$OMPI_VERSION" == "") then
    echo "Must supply version as argv[1] -- aborting"
    exit 1
endif

# we can catch some hard (but possible) to do mistakes by looking at
# our tree's revision number, but only if we are in the source tree.
# Otherwise, use what configure told us, at the cost of allowing one
# or two corner cases in (but otherwise VPATH builds won't work)
set svn_r=$OMPI_SVN_VERSION
if (-d .svn) then
    set svn_r="r`svnversion .`"
endif

set start=`date`
cat <<EOF
 
Creating Open MPI distribution
In directory: `pwd`
Version: $OMPI_VERSION
Started: $start
 
EOF

umask 022

if (! -d "$distdir") then
    echo "*** ERROR: dist dir does not exist"
    echo "*** ERROR:   $distdir"
    exit 1
endif

#
# See if we need to update the version file with the current SVN
# revision number.  Do this *before* entering the distribution tree to
# solve a whole host of problems with VPATH (since srcdir may be
# relative or absolute)
#
set cur_svn_r="`grep '^svn_r' ${distdir}/VERSION | cut -d= -f2`"
if ("$cur_svn_r" == "-1") then
    sed -e 's/^svn_r=.*/svn_r='$svn_r'/' "${distdir}/VERSION" > "${distdir}/version.new"
    cp "${distdir}/version.new" "${distdir}/VERSION"
    rm -f "${distdir}/version.new"
    # need to reset the timestamp to not annoy AM dependencies
    touch -r "${srcdir}/VERSION" "${distdir}/VERSION"
    echo "*** Updated VERSION file with SVN r number"
else
    echo "*** Did NOT update VERSION file with SVN r number"
endif

# Copy configure.params and autogen.subdirs files into distribution.
# This should really be in each component's Makefile.am, but that's
# never going to happen.  So copy here automagically.
echo "*** Copying configure.params files"
set dirs=opal
if (-d orte) then
    set dirs="$dirs orte"
endif
if (-d ompi) then
    set dirs="$dirs ompi"
endif
find $dirs -name "configure.params" -exec cp -f -p "{}" "$distdir/{}" \; >& /dev/null
echo "*** Copying autogen.subdirs files"
find $dirs -name "autogen.subdirs" -exec cp -f -p "{}" "$distdir/{}" \; >& /dev/null

#########################################################
# VERY IMPORTANT: Now go into the new distribution tree #
#########################################################
cd "$distdir"
echo "*** Now in distdir: $distdir"

#
# Get the latest config.guess and config.sub from ftp.gnu.org
#

echo "*** Downloading latest config.sub/config.guess from ftp.gnu.org..."
cd config
set configdir="`pwd`"
mkdir tmp.$$
cd tmp.$$
# Official HTTP git mirrors for config.guess / config.sub
wget -t 1 -T 10 -O config.guess 'http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=master'
wget -t 1 -T 10 -O config.sub 'http://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=master'
chmod +x config.guess config.sub

# Recently, ftp.gnu.org has had zero-legnth config.guess / config.sub
# files, which causes the automated nightly SVN snapshot tarball to
# fail to be made correctly.  This is a primitive attempt to fix that.
# If we got zero-length files from wget, use a config.guess /
# config.sub from a known location that is more recent than what ships
# in the current generation of auto* tools.  Also check to ensure that
# the resulting scripts are runnable (Jan 2009: there are un-runnable
# scripts available right now because of some git vulnerability).

# Before you complain about this too loudly, remember that we're using
# unreleased software...

set happy=0
if (! -f config.guess || ! -s config.guess) then
    echo " - WARNING: Got bad config.guess from ftp.gnu.org (non-existent or empty)"
else
    ./config.guess >& /dev/null
    if ($status != 0) then
        echo " - WARNING: Got bad config.guess from ftp.gnu.org (not executable)"
    else
        if (! -f config.sub || ! -s config.sub) then
            echo " - WARNING: Got bad config.sub from ftp.gnu.org (non-existent or empty)"
        else
            ./config.sub `./config.guess` >& /dev/null
            if ($status != 0) then
                echo " - WARNING: Got bad config.sub from ftp.gnu.org (not executable)"
            else
                echo " - Got good config.guess and config.sub from ftp.gnu.org"
                cp config.sub config.guess ..
                set happy=1
            endif
        endif
    endif
endif

if ("$happy" == "0") then
    echo " - WARNING: using included versions for both config.sub and config.guess"
endif
cd ..
rm -rf tmp.$$
cd ..


#
# Find all the config.guess/config.sub files, and replace them with
# the ones that we've downloaded
#

echo "*** Now in: `pwd`"
echo "*** Replacing config.sub/config.guess with latest from ftp.gnu.org..."
foreach file (config.guess config.sub)
    foreach dir (opal orte ompi)
        if (-d $dir) then
            find $dir -name $file \
                -exec chmod +w {} \; \
                -exec cp -f $configdir/$file {} \; \
                -print
        endif
    end
end


#
# Put in date/version number in man pages
# JMS don't have man pages yet -- this is a straight copy from LAM7
#

set ver="$OMPI_VERSION"
#echo "*** Updating version date/number in man pages"
#rm -f manfiles
#find man -type f | grep -v Makefile > manfiles

#set date="`date '+%B, %Y'`"
#cp $srcdir/config/doctext.nroff.def .
#foreach file (`cat manfiles` doctext.nroff.def)
#    sed -e "s/-RELEASEDATE-/$date/g" $file > foo
#    sed -e "s/-RELEASEVERSION-/$ver/g" foo > bar
#    rm -f $file # Needed 'cause automake makes hard links, not copies
#    mv bar $file
#    rm -f foo
#end
#rm -f manfiles

#
# Make all the man pages -- doctext needs to be in your path
# JMS: Don't have man pages yet; need to do this at some point
#

#
# Now we need to list all these generated man pages in the Makefile.am
# and Makefile.in in man/man3.  Ick!
# JMS: Will probably need to do this as well.  Sigh.
#

#echo "*** Frobbing Makefile.am and Makefile.in..."
#cd ../../man/man3
#set files="`ls MPI_*3 MPIO_*3 XMPI_*3 MPIL_*3`"

#
# This is unfortunately necessary because $files is too long to do a
# single sed search/replace.  Ugh.
# JMS: Will probably need to do this as well.  Sigh.
#

#echo "*** Adding man files to Makefile.in..."
#foreach file ($files)
#    set name_prefix="`echo $file | cut -c1-4`"
#    if ("$name_prefix" == "MPI_") then
#	set letter="`echo $file | cut -c5`"
#	set div="`expr $letter \> F`"
#	set line="generated_man_$div"
#    else
#	set line="generated_man_other"
#    endif
#    echo " - $file / $line"
#    foreach fix (Makefile.am Makefile.in)
#	sed -e "s/$line =/$line =$file /" $fix > $fix.new
#	chmod +w $fix
#	mv -f $fix.new $fix
#	chmod -w $fix
#    end
#end
#cd ../..

#
# Put the release version number in the README and INSTALL files
#

set files="README INSTALL"
echo "*** Updating version number in $files..."
foreach file ($files)
    echo " - Setting $file"
    if (-f $file) then
	sed -e "s/OMPI_VERSION/$ver/g" $file > bar
	mv -f bar $file
    endif
end

#
# All done
#

cat <<EOF
*** Open MPI version $ver distribution created
 
Started: $start
Ended:   `date`
 
EOF

