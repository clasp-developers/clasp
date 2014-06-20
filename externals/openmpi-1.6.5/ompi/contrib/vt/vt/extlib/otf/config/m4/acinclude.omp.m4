AC_DEFUN([CHECK_OMP],
[
    omp_error="no"
    check_omp="yes"
    force_omp="no"
    have_omp="no"

    AC_ARG_VAR(OPENMP_CXXFLAGS, [C++ compiler flags to enable support for OpenMP])

    AC_ARG_WITH([omp],
        AC_HELP_STRING([--with-omp],
            [use OpenMP for some OTF tools, default: yes if found by configure]),
        [if test "$withval" = "yes"; then force_omp="yes"; else check_omp="no"; fi])

    AS_IF([test "$check_omp" = "yes"],
    [
dnl     Disable OpenMP if the PGI compiler is used to work around the following errors:
dnl     compiler version  compiler error
dnl     < 9.0-3           PGCC-S-0000-Internal compiler error. calc_dw_tag:no tag
dnl     (see Technical Problem Report 4337 at http://www.pgroup.com/support/release_tprs_90.htm)
dnl     10.1 - 10.6       this kind of pragma may not be used here
dnl                       #pargma omp barrier
        case `$CC -V 2>&1` in
            *pgcc*)
                AS_IF([test "$force_omp" = "yes"],
                [
                    AC_MSG_NOTICE([error: OpenMP support cannot be enabled due to not yet resolved issues with the PGI compiler])
                ])
                omp_error="yes"
                ;;
        esac

        AS_IF([test "$omp_error" = "no"],
        [
            AC_LANG_SAVE
            AC_LANG_CPLUSPLUS
            AX_OPENMP([have_omp="yes"], [omp_error="yes"])
            AC_LANG_RESTORE
        ])
    ])
])
