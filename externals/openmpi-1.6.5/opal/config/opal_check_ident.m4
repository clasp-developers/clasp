dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2007 Sun Microsystems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl
dnl defines:
dnl   OPAL_$1_USE_PRAGMA_IDENT
dnl   OPAL_$1_USE_IDENT
dnl   OPAL_$1_USE_CONST_CHAR_IDENT
dnl

# OMPI_CHECK_IDENT(compiler-env, compiler-flags,
# file-suffix, lang) Try to compile a source file containing
# a #pragma ident, and determine whether the ident was
# inserted into the resulting object file
# -----------------------------------------------------------
AC_DEFUN([OMPI_CHECK_IDENT], [
    AC_MSG_CHECKING([for $4 ident string support])

    opal_pragma_ident_happy=0
    opal_ident_happy=0
    opal_static_const_char_happy=0
    _OMPI_CHECK_IDENT(
        [$1], [$2], [$3],
        [[#]pragma ident], [],
        [opal_pragma_ident_happy=1
         ompi_message="[#]pragma ident"],
        _OMPI_CHECK_IDENT(
            [$1], [$2], [$3],
            [[#]ident], [],
            [opal_ident_happy=1
             ompi_message="[#]ident"],
            _OMPI_CHECK_IDENT(
                [$1], [$2], [$3],
                [[#]pragma comment(exestr, ], [)],
                [opal_pragma_comment_happy=1
                 ompi_message="[#]pragma comment"],
                [opal_static_const_char_happy=1
                 ompi_message="static const char[[]]"])))

    AC_DEFINE_UNQUOTED([OPAL_$1_USE_PRAGMA_IDENT],
        [$opal_pragma_ident_happy], [Use #pragma ident strings for $4 files])
    AC_DEFINE_UNQUOTED([OPAL_$1_USE_IDENT],
        [$opal_ident_happy], [Use #ident strings for $4 files])
    AC_DEFINE_UNQUOTED([OPAL_$1_USE_PRAGMA_COMMENT],
        [$opal_pragma_comment_happy], [Use #pragma comment for $4 files])
    AC_DEFINE_UNQUOTED([OPAL_$1_USE_CONST_CHAR_IDENT],
        [$opal_static_const_char_happy], [Use static const char[] strings for $4 files])

    AC_MSG_RESULT([$ompi_message])

    unset opal_pragma_ident_happy opal_ident_happy opal_static_const_char_happy ompi_message
])

# _OMPI_CHECK_IDENT(compiler-env, compiler-flags,
# file-suffix, header_prefix, header_suffix, action-if-success, action-if-fail)
# Try to compile a source file containing a #-style ident,
# and determine whether the ident was inserted into the
# resulting object file
# -----------------------------------------------------------
AC_DEFUN([_OMPI_CHECK_IDENT], [
    eval ompi_compiler="\$$1"
    eval ompi_flags="\$$2"

    ompi_ident="string_not_coincidentally_inserted_by_the_compiler"
    cat > conftest.$3 <<EOF
$4 "$ompi_ident" $5
int main(int argc, char** argv);
int main(int argc, char** argv) { return 0; }
EOF

    # "strings" won't always return the ident string.  objdump isn't
    # universal (e.g., OS X doesn't have it), and ...other
    # complications.  So just try to "grep" for the string in the
    # resulting object file.  If the ident is found in "strings" or
    # the grep succeeds, rule that we have this flavor of ident.

    OMPI_LOG_COMMAND([$ompi_compiler $ompi_flags -c conftest.$3 -o conftest.${OBJEXT}],
                     [AS_IF([test -f conftest.${OBJEXT}],
                            [ompi_output="`strings -a conftest.${OBJEXT} | grep $ompi_ident`"
                             grep $ompi_ident conftest.${OBJEXT} 2>&1 1>/dev/null
                             ompi_status=$?
                             AS_IF([test "$ompi_output" != "" -o "$ompi_status" = "0"],
                                   [$6],
                                   [$7])],
                            [OMPI_LOG_MSG([the failed program was:])
                             OMPI_LOG_FILE([conftest.$3])
                             $7]
                            [$7])])

    unset ompi_compiler ompi_flags ompi_output ompi_status
    rm -rf conftest.* conftest${EXEEXT}
])dnl
