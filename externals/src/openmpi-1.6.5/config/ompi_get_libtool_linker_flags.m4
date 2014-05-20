dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CHECK_LINKER_FLAGS],[
#
# libtool has been created by this point
# Try to see if it will add any additional flags for dependant libraries
#

AC_MSG_CHECKING([for libtool-supplied linker flags])

libtool="$1"
extra_flags="$2"

# Get the directory where libtool lives

start="`pwd`"
d="`dirname $libtool`"
cd "$d"
libtool="`pwd`/libtool"
cd "$start"
unset d start

# Make a playground to work in

mkdir conftest.$$
cd conftest.$$

cat > foo.c <<EOF
int foo(void) { return 0; }
EOF

ompi_check_linker_flags_work() {
    OMPI_LOG_MSG([$cmd], [yes])
    eval $cmd >&5 2>&5
    if test -n "[$]1"; then
	output=`eval $cmd 2>/dev/null | head -n 1 | sed -e 's,^libtool: *,,' -e 's,^link: *,,'`
    fi
    status="$?"
    OMPI_LOG_MSG([\$? = $status], [yes])
    if test "$status" != "0"; then
	AC_MSG_RESULT([libtool error!])
	AC_MSG_ERROR([Cannot continue])
    fi
}

#
# First make a sample library with the current LDFLAGS and LIBS
#

cmd="$libtool --mode=compile --tag=CC $CC $CFLAGS -c -o foo.o foo.c"
ompi_check_linker_flags_work
cmd="$libtool --mode=link --tag=CC $CC $CFLAGS foo.lo $LDFLAGS $LIBS -o libfoo.la"
ompi_check_linker_flags_work

#
# Now fake linking to it and capture the output from libtool
#

cmd="$libtool --dry-run --mode=link --tag=CC $CC bar.lo libfoo.la -o bar $extra_flags"
ompi_check_linker_flags_work yes

# eat any extra whitespace in CC, as libtool will do the same
tmpCC=`echo $CC | sed -e 's/\//\\\\\//g'`
output=`echo $output | sed -e "s/^$tmpCC//"`
eval "set $output"
extra_ldflags=
while test -n "[$]1"; do
    case "[$]1" in
    *.libs/bar*) ;;
    bar*) ;;
    -I*) ;;
    -L*) ;;
    -R*) ;;
    -lfoo) ;;
    *.libs/libfoo.*) ;;
    -o) ;;
    *.so) ;;
    *.a) ;;
    *)
	extra_ldflags="$extra_ldflags [$]1"
	;;
    esac
    shift
done

if test -n "$extra_ldflags"; then
    AC_MSG_RESULT([$extra_ldflags])
else
    AC_MSG_RESULT([no extra flags])
fi

cd ..
rm -rf conftest.$$])dnl
