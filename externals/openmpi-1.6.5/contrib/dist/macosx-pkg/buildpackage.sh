#!/bin/bash

# Copyright (c) 2001-2006 The Trustees of Indiana University.  
#                         All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved. 
# 
# This file is part of the Open MPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# Open MPI source distribution.
#
#

#
# Build a Mac OS X package for use by Installer.app
#
# Usage: buildpackage.sh <tarball> [prefix]
#
# Prefix defaults to /usr/local


########################################################################
#
# Configuration Options
#
########################################################################

#
# User-configurable stuff
#
OMPI_PACKAGE="openmpi"
OMPI_PREFIX="/usr/local/"
OMPI_OPTIONS="--disable-mpi-f77 --without-cs-fs --enable-mca-no-build=ras-slurm,pls-slurm,gpr-null,sds-pipe,sds-slurm,pml-cm NM=\"nm -p\""
OMPI_OSX_README="ReadMe.rtf"
# note - if want XGrid support, make sure that a cocoa-supported 
# architecture appears first on the list.  Otherwise, we won't
# lipo that component and it will be dropped
OPAL_ARCH_LIST="ppc ppc64 i386 x86_64"
OMPI_SDK="/Developer/SDKs/MacOSX10.4u.sdk"

#
# Not so modifiable stuff
#
BUILD_TMP="/tmp/buildpackage-$$"
if test ! "$2" = ""; then
    OMPI_PREFIX="$2"
fi

OMPI_STARTDIR=`pwd`

echo "--> Configuration options:"
echo "    Package Name:   $OMPI_PACKAGE"
echo "    Prefix:         $OMPI_PREFIX"
echo "    Config Options: $OMPI_OPTIONS"
echo "    Architectures:  $OPAL_ARCH_LIST"
echo "    Target SDK:     $OMPI_SDK"
echo ""

########################################################################
#
# Start actual code that does stuff
#
########################################################################

#
# Sanity check
#
fulltarball="$1"
if test "$fulltarball" = ""; then
    echo "Usage: buildpackage.sh <tarball> [prefix]"
    exit 1
fi
if test ! -f $fulltarball; then
    echo "*** Can't find $fulltarball!"
    exit 1
fi
echo "--> Found tarball: $fulltarball"

#
# Find version info
#
tarball=`basename $fulltarball`
first="`echo $tarball | cut -d- -f2`"
version="`echo $first | sed -e 's/\.tar\.gz//'`"
unset first
echo "--> Found OMPI version: $version"

OMPI_VER_PACKAGE="${OMPI_PACKAGE}-${version}"

#
# Sanity check that we can continue
#
if test -d "/Volumes/${OMPI_VER_PACKAGE}"; then
    echo "*** Already have disk image (/Volumes/${OMPI_VER_PACKAGE}) mounted."
    echo "*** Unmount and try again"
    exit 1
fi

if test ! -r "${OMPI_OSX_README}"; then
    echo "*** Can not find ${OMPI_OSX_README} in `pwd`."
    exit 1
else
    OMPI_OSX_README="`pwd`/${OMPI_OSX_README}"
fi


#
# Clean out the environment a bit
#
echo "--> Cleaning environment"
PATH=/bin:/sbin/:/usr/bin
LANGUAGE=C
LC_ALL=C
LC_MESSAGES=
LANG=
export PATH LANGUAGE LC_ALL LC_MESSAGES LANG
unset LD_LIBRARY_PATH CC CXX FC F77 OBJC

#
# Make some play space
#
echo "--> Making play space: $BUILD_TMP"
if test -d $BUILD_TMP; then
    echo "Build dir $BUILD_TMP exists - exiting"
    exit 1
fi
# -p is safe - will only run on OS X
mkdir -p $BUILD_TMP


########################################################################
#
# Configure, Build, and Install Open MPI
#
########################################################################

#
# Put tarball in right place
#
echo "--> Copying tarball"
cp $fulltarball $BUILD_TMP/.

cd $BUILD_TMP

#
# Expand tarball
#

# we know there can't be spaces in $tarball - filename only
cmd="tar xzf $tarball"
echo "--> Untarring source: $cmd"

eval $cmd
srcdir="$BUILD_TMP/openmpi-$version"
if test ! -d "$srcdir"; then
    echo "*** Didn't find $srcdir as expected - aborting"
    exit 1
fi

build_arch=`uname -p`"-apple-darwin"`uname -r`

real_install=1
for arch in $OPAL_ARCH_LIST ; do
    builddir="$BUILD_TMP/build-$arch"
    mkdir "$builddir"

    case "$arch" in
        ppc)
            host_arch="powerpc-apple-darwin"`uname -r`
            ;;
        ppc64)
            # lie, but makes building on G4 easier
            host_arch="powerpc64-apple-darwin"`uname -r`
            ;;
        i386)
            host_arch="i386-apple-darwin"`uname -r`
            ;;
        x86_64)
            host_arch="x86_64-apple-darwin"`uname -r`
            ;;
        *)
            echo "**** Could not find arch string for $arch ****"
            exit 1
            ;;
    esac

    #
    # Run configure
    # 
    cd $builddir
    config="$srcdir/configure CFLAGS=\"-arch $arch -isysroot $OMPI_SDK\" CXXFLAGS=\"-arch $arch -isysroot $OMPI_SDK\" OBJCFLAGS=\"-arch $arch -isysroot $OMPI_SDK\" --prefix=$OMPI_PREFIX $OMPI_OPTIONS --build=$build_arch --host=$host_arch"
    echo "--> Running configure: $config"
    eval $config > "$BUILD_TMP/configure.out-$arch" 2>&1

    if test $? != 0; then
        echo "*** Problem running configure - aborting!"
        echo "*** See $BUILD_TMP/configure.out-$arch for help."
        exit 1
    fi

    #
    # Build
    #
    cmd="make -j 4 all"
    echo "--> Building: $cmd"
    eval $cmd > "$BUILD_TMP/make.out-$arch" 2>&1

    if test $? != 0; then
        echo "*** Problem building - aborting!"
        echo "*** See $BUILD_TMP/make.out-$arch for help."
        exit 1
    fi

    #
    # Install into tmp place
    #
    if test $real_install -eq 1 ; then
        distdir="dist"
        real_install=0
    else
        distdir="dist-$arch"
    fi
    fulldistdir="$BUILD_TMP/$distdir"
    cmd="make DESTDIR=$fulldistdir install"
    echo "--> Installing:"
    eval $cmd > "$BUILD_TMP/install.out-$arch" 2>&1

    if test $? != 0; then
        echo "*** Problem installing - aborting!"
        echo "*** See $BUILD_TMP/install.out-$arch for help."
        exit 1
    fi

    #
    # Copy in special doc files
    #
    SPECIAL_FILES="README LICENSE"
    echo "--> Copying in special files: $SPECIAL_FILES"
    pushd $srcdir >/dev/null
    mkdir -p  "${fulldistdir}/${OMPI_PREFIX}/share/openmpi/doc"
    cp $SPECIAL_FILES "${fulldistdir}/${OMPI_PREFIX}/share/openmpi/doc/."
    if [ ! $? = 0 ]; then
        echo "*** Problem copying files $SPECIAL_FILES.  Aborting!"
        exit 1
    fi
    popd >/dev/null

    distdir=
    fulldistdir=
done


########################################################################
#
# Make the fat binary
#
########################################################################
print_arch_if() {
    case "$1" in
        ppc)
            echo "#ifdef __ppc__" >> mpi.h
            ;;
        ppc64)
            echo "#ifdef __ppc64__" >> mpi.h
            ;;
        i386)
            echo "#ifdef __i386__" >> mpi.h
            ;;
        x86_64)
            echo "#ifdef __x86_64__" >> mpi.h
            ;;
        *)
            echo "*** Could not find arch #ifdef for $1"
            exit 1
            ;;
    esac
} 

# Set arch to the first arch in the list.  Go through the for loop,
# although we'll break out at the end of the first time through.  Look
# at the other arches that were built by using ls.
for arch in $OPAL_ARCH_LIST ; do
    cd $BUILD_TMP
    other_archs=`ls -d dist-*`
    fulldistdir="$BUILD_TMP/dist"

    echo "--> Creating fat binares and libraries"
    for other_arch in $other_archs ; do
        cd "$fulldistdir"

        # <prefix>/bin - don't copy in 64 bit binaries
        if echo $other_arch | grep -v 64 > /dev/null ; then
            files=`find ./${OMPI_PREFIX}/bin -type f -print`
            for file in $files ; do
                other_file="$BUILD_TMP/${other_arch}/$file"
                if test -r $other_file ; then
                    lipo -create $file $other_file -output $file
                fi
            done
        fi

        # <prefix>/lib - ignore .la files
        files=`find ./${OMPI_PREFIX}/lib -type f -print | grep -v '\.la$'`
        for file in $files ; do
            other_file="$BUILD_TMP/${other_arch}/$file"
            if test -r $other_file ; then
                lipo -create $file $other_file -output $file
            else
                echo "Not lipoing missing file $other_file"
            fi
        done

    done

    cd $BUILD_TMP

    echo "--> Creating multi-architecture mpi.h"
    # mpi.h
    # get the top of mpi.h
    mpih_top=`grep -n '@OMPI_BEGIN_CONFIGURE_SECTION@' $BUILD_TMP/dist/${OMPI_PREFIX}/include/mpi.h | cut -f1 -d:`
    mpih_top=`echo "$mpih_top - 1" | bc`
    head -n $mpih_top $BUILD_TMP/dist/${OMPI_PREFIX}/include/mpi.h > mpih_top.txt

    # now the bottom of mpi.h
    mpih_bottom_top=`grep -n '@OMPI_END_CONFIGURE_SECTION@' $BUILD_TMP/dist/${OMPI_PREFIX}/include/mpi.h | cut -f1 -d:`
    mpih_bottom_bottom=`wc -l $BUILD_TMP/dist/${OMPI_PREFIX}/include/mpi.h | cut -f1 -d/`
    mpih_bottom=`echo "$mpih_bottom_bottom - $mpih_bottom_top" | bc`
    tail -n $mpih_bottom $BUILD_TMP/dist/${OMPI_PREFIX}/include/mpi.h > mpih_bottom.txt

    # now get our little section of fun
    mpih_top=`echo "$mpih_top + 1" | bc`
    mpih_fun_len=`echo "$mpih_bottom_top - $mpih_top + 1" | bc`
    head -n $mpih_bottom_top $BUILD_TMP/dist/${OMPI_PREFIX}/include/mpi.h | tail -n $mpih_fun_len > mpih_$arch.txt

    # start putting it back together
    rm -f mpi.h
    cat mpih_top.txt > mpi.h

    print_arch_if $arch
    cat mpih_$arch.txt >> mpi.h
    echo "#endif" >> mpi.h

    for other_arch_dir in $other_archs ; do
        other_arch=`echo $other_arch_dir | cut -f2 -d-`
        mpih_top=`grep -n '@OMPI_BEGIN_CONFIGURE_SECTION@' $BUILD_TMP/$other_arch_dir/${OMPI_PREFIX}/include/mpi.h | cut -f1 -d:`
        mpih_bottom_top=`grep -n '@OMPI_END_CONFIGURE_SECTION@' $BUILD_TMP/$other_arch_dir/${OMPI_PREFIX}/include/mpi.h | cut -f1 -d:`
        mpih_fun_len=`echo "$mpih_bottom_top - $mpih_top + 1" | bc`
        head -n $mpih_bottom_top $BUILD_TMP/$other_arch_dir/${OMPI_PREFIX}/include/mpi.h | tail -n $mpih_fun_len > mpih_$other_arch.txt

        print_arch_if $other_arch
        cat mpih_$other_arch.txt >> mpi.h
        echo "#endif" >> mpi.h
    done

    cat mpih_bottom.txt >> mpi.h
    mv mpi.h $BUILD_TMP/dist/${OMPI_PREFIX}/include/.
    rm mpih*
    break
done

# set component load errors to false, as we're almost always going to
# fail to load the XGrid components on 64 bit systems, and users don't
# need to see that.
echo "mca_component_show_load_errors = 0" >> $BUILD_TMP/dist/${OMPI_PREFIX}/etc/openmpi-mca-params.conf

########################################################################
#
# Do all the package mojo
#
########################################################################

#
# Prep package info
#
debug_file="${BUILD_TMP}/disk.out"
touch "$debug_file"
echo "--> Creating Package Info:"

cd $BUILD_TMP

pkdir="${BUILD_TMP}/${OMPI_PACKAGE}.pkg"
mkdir -p ${pkdir}
mkdir ${pkdir}/Contents
mkdir ${pkdir}/Contents/Resources
mkdir ${pkdir}/Contents/Resources/English.lproj
echo 'pmkrpkg1' > ${pkdir}/Contents/PkgInfo

infofile=${pkdir}/Contents/Resources/English.lproj/${OMPI_PACKAGE}.info

echo "Title Open MPI ${version}" > ${infofile}
echo "Version ${version}" >> ${infofile}
echo "Description Install Open MPI ${version}" >> ${infofile}
echo 'DefaultLocation /' >> ${infofile}
echo 'DeleteWarning' >> ${infofile}
echo 'NeedsAuthorization YES' >> ${infofile}
echo 'Required NO' >> ${infofile}
echo 'Relocatable NO' >> ${infofile}
echo 'RequiresReboot NO' >> ${infofile}
echo 'UseUserMask NO' >> ${infofile}
echo 'OverwritePermissions NO' >> ${infofile}
echo 'InstallFat NO' >> ${infofile}

echo "--> Copying OS X-specific ReadMe into package"
cp "${OMPI_OSX_README}" "${pkdir}/Contents/Resources/ReadMe.rtf"
if [ ! $? = 0 ]; then
    echo "*** Could not copy in ReadMe.rtf.  Aborting!"
    exit 1
fi

echo "--> Creating pax file"
CWD=`pwd`
cd "$fulldistdir"
pax -w -f "${pkdir}/Contents/Resources/${OMPI_PACKAGE}.pax" . >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Failed building pax file.  Aborting!"
    echo "*** Check $debug_file for information"
    cd "$CWD"
    exit 1
fi
cd "$CWD"
unset CWD


echo "--> Compressing pax file"
gzip "${pkdir}/Contents/Resources/${OMPI_PACKAGE}.pax" >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Failed compressing pax file.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

echo "--> Creating bom file"
mkbom "$fulldistdir" "${pkdir}/Contents/Resources/${OMPI_PACKAGE}.bom" >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Failed building bom file.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

echo "--> Generating sizes file:"
sizesfile="${pkdir}/Contents/Resources/${OMPI_PACKAGE}.sizes"

numFiles=`du -a ${fulldistdir} | wc -l`
installedSize=`du -s ${fulldistdir} | cut -f1`
compressedSize=`du -s ${fulldistdir} | cut -f1`

echo "NumFiles ${numFiles}" > ${sizesfile}
echo "InstalledSize ${installedSize}" >> ${sizesfile}
echo "CompressedSize ${compressedSize}" >> ${sizesfile}
cat ${sizesfile}

#
# Make a disk image in read-write mode
#
echo "--> Creating Disc Image"
# Allocated about 2.5MB more than we need, just to be safe.  If that
# number is less than about 5MB, make 5MB to keep disk utilities
# happy.
sectorsAlloced=`echo 2*${compressedSize}+50|bc`
if [ $sectorsAlloced -lt 10000 ]; then
    sectorsAlloced=10000
fi
hdiutil create -ov "${BUILD_TMP}/${OMPI_VER_PACKAGE}RW" -sectors ${sectorsAlloced} >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Failed hdiutil create.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

mountLoc=`hdid -nomount ${BUILD_TMP}/${OMPI_VER_PACKAGE}RW.dmg | grep HFS | cut -f1`
/sbin/newfs_hfs -v ${OMPI_VER_PACKAGE} ${mountLoc} >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Failed building HFS+ file system.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

hdiutil eject ${mountLoc} >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Could not unmount $mountLoc.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

#
# Copy above package into the disk image
#
echo "--> Copying Package to Disc Image"
hdid "${BUILD_TMP}/${OMPI_VER_PACKAGE}RW.dmg" >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Could not mount ${BUILD_TMP}/${OMPI_VER_PACKAGE}RW.dmg.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

if [ ! -d "/Volumes/${OMPI_VER_PACKAGE}" ]; then
    echo "*** /Volumes/${OMPI_VER_PACKAGE} does not exist.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

cp -R "${pkdir}" "/Volumes/${OMPI_VER_PACKAGE}"
if [ ! $? = 0 ]; then
    echo "*** Error copying ${OMPI_VER_PACKAGE}.pkg.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

#
# Converting Disk Image to read-only (and shrink to size needed)
#
cmd="hdiutil eject ${mountLoc}"
echo "--> Ejecting R/W disk: $cmd"
eval $cmd >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Error ejecting R/W disk.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

cmd="hdiutil resize \"${BUILD_TMP}/${OMPI_VER_PACKAGE}RW.dmg\" -sectors min"
echo "--> Resizing: $cmd"
eval $cmd >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Error resizing disk.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

cmd="hdiutil convert \"${BUILD_TMP}/${OMPI_VER_PACKAGE}RW.dmg\" -format UDRO -o \"/tmp/${OMPI_VER_PACKAGE}.dmg\""
echo "--> Converting to R-O: $cmd"
eval $cmd >> "$debug_file" 2>&1
if [ ! $? = 0 ]; then
    echo "*** Error converting disk to read-only.  Aborting!"
    echo "*** Check $debug_file for information"
    exit 1
fi

echo "--> Compressing disk image"
gzip --best "/tmp/${OMPI_VER_PACKAGE}.dmg"

echo "--> Cleaning up the staging directory"
rm -rf "${BUILD_TMP}"
if [ ! $? = 0 ]; then
    echo "*** Could not clean up ${BUILD_TMP}."
    echo "You may want to clean it up yourself."
    exit 1
fi

echo "--> Done.  Package is at: /tmp/${OMPI_VER_PACKAGE}.dmg.gz"
