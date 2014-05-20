dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_TEXT
dnl
dnl Determine how to set current mode as text.
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_TEXT],[
    AC_MSG_CHECKING([directive for setting text section])
    opal_cv_asm_text=""
    if test "$ompi_cv_c_compiler_vendor" = "microsoft" ; then
        # text section will be brought in with the rest of
        # header for MS - leave blank for now
        opal_cv_asm_text=""
    else
        case $host in
            *-aix*)
                opal_cv_asm_text=[".csect .text[PR]"]
            ;;
            *)
                opal_cv_asm_text=".text"
            ;;
        esac
    fi
    AC_MSG_RESULT([$opal_cv_asm_text])
    AC_DEFINE_UNQUOTED([OPAL_ASM_TEXT], ["$opal_cv_asm_text"],
                       [Assembly directive for setting text section])
    OPAL_ASM_TEXT="$opal_cv_asm_text"
    AC_SUBST(OPAL_ASM_TEXT)
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_GLOBAL
dnl
dnl Sets OPAL_ASM_GLOBAL to the value to prefix global values
dnl
dnl I'm sure if I don't have a test for this, there will be some
dnl dumb platform that uses something else
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_GLOBAL],[
    AC_MSG_CHECKING([directive for exporting symbols])
    opal_cv_asm_global=""
    if test "$ompi_cv_c_compiler_vendor" = "microsoft" ; then
        opal_cv_asm_global="PUBLIC"
    else
        case $host in
            *)
                opal_cv_asm_global=".globl"
            ;;
        esac
    fi
    AC_MSG_RESULT([$opal_cv_asm_global])
    AC_DEFINE_UNQUOTED([OPAL_ASM_GLOBAL], ["$opal_cv_asm_global"],
                       [Assembly directive for exporting symbols])
    OPAL_ASM_GLOBAL="$opal_cv_asm_global"
    AC_SUBST(OMPI_AS_GLOBAL)
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_LSYM
dnl
dnl Sets OPAL_ASM_LSYM to the prefix value on a symbol to make it
dnl an internal label (jump target and whatnot)
dnl
dnl We look for L .L $ L$ (in that order) for something that both
dnl assembles and does not leave a label in the output of nm.  Fall
dnl back to L if nothing else seems to work :/
dnl
dnl #################################################################

# _OMPI_CHECK_ASM_LSYM([variable-to-set])
# ---------------------------------------
AC_DEFUN([_OMPI_CHECK_ASM_LSYM],[
    AC_REQUIRE([AC_PROG_GREP])

    $1="L"

    for sym in L .L $ L$ ; do
        asm_result=0
        echo "configure: trying $sym" >&AC_FD_CC
        OMPI_TRY_ASSEMBLE([foobar$opal_cv_asm_label_suffix
${sym}mytestlabel$opal_cv_asm_label_suffix],
            [# ok, we succeeded at assembling.  see if we can nm, 
             # throwing the results in a file
            if $NM conftest.$OBJEXT > conftest.out 2>&AC_FD_CC ; then
                if test "`$GREP mytestlabel conftest.out`" = "" ; then
                    # there was no symbol...  looks promising to me
                    $1="$sym"
                    asm_result=1
                elif test ["`$GREP ' [Nt] .*mytestlabel' conftest.out`"] = "" ; then
                    # see if we have a non-global-ish symbol
                    # but we should see if we can do better.
                    $1="$sym"
                fi
            else
                # not so much on the NM goodness :/
                echo "$NM failed.  Output from NM was:" >&AC_FD_CC
                cat conftest.out >&AC_FD_CC
                AC_MSG_WARN([$NM could not read object file])
            fi
            ])
        if test "$asm_result" = "1" ; then
            break
        fi
    done
    rm -f conftest.out
    unset asm_result sym
])

# OMPI_CHECK_ASM_LSYM()
# ---------------------
AC_DEFUN([OMPI_CHECK_ASM_LSYM],[
    AC_REQUIRE([AC_PROG_NM])

    AC_CACHE_CHECK([prefix for lsym labels],
                   [opal_cv_asm_lsym],
                   [_OMPI_CHECK_ASM_LSYM([opal_cv_asm_lsym])])
    AC_DEFINE_UNQUOTED([OPAL_ASM_LSYM], ["$opal_cv_asm_lsym"],
                       [Assembly prefix for lsym labels])
    OPAL_ASM_LSYM="$opal_cv_asm_lsym"
    AC_SUBST(OPAL_ASM_LSYM)
])dnl

dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_PROC
dnl
dnl Sets a cv-flag, if the compiler needs a proc/endp-definition to
dnl link with C.
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_PROC],[
    AC_CACHE_CHECK([if .proc/endp is needed],
                   [ompi_cv_asm_need_proc],
                   [ompi_cv_asm_need_proc="no"
                    OMPI_TRY_ASSEMBLE([
     .proc mysym
mysym:
     .endp mysym],
                          [ompi_cv_asm_need_proc="yes"])
                    rm -f conftest.out])

    if test "$ompi_cv_asm_need_proc" = "yes" ; then
       ompi_cv_asm_proc=".proc"
       ompi_cv_asm_endproc=".endp"
    else
       ompi_cv_asm_proc="#"
       ompi_cv_asm_endproc="#"
    fi
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_GSYM
dnl
dnl Sets OPAL_ASM_GSYM to the prefix value on a symbol to make it
dnl a global linkable from C.  Basically, an _ or not.
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_GSYM],[
    AC_CACHE_CHECK([prefix for global symbol labels],
                   [opal_cv_asm_gsym],
                   [_OMPI_CHECK_ASM_GSYM])

    if test "$opal_cv_asm_gsym" = "none" ; then
       AC_MSG_ERROR([Could not determine global symbol label prefix])
    fi

    AC_DEFINE_UNQUOTED([OPAL_ASM_GSYM], ["$opal_cv_asm_gsym"],
                       [Assembly prefix for gsym labels])
    OPAL_ASM_GSYM="$opal_cv_asm_gsym"
    AC_SUBST(OPAL_ASM_GSYM)

])

AC_DEFUN([_OMPI_CHECK_ASM_GSYM],[
    opal_cv_asm_gsym="none"

    for sym in "_" "" "." ; do
        asm_result=0
        echo "configure: trying $sym" >&AC_FD_CC
cat > conftest_c.c <<EOF
#ifdef __cplusplus
extern "C" {
#endif
void gsym_test_func(void);
#ifdef __cplusplus
}
#endif
int
main()
{
    gsym_test_func();
    return 0;
}
EOF
        OMPI_TRY_ASSEMBLE([
$opal_cv_asm_text
$ompi_cv_asm_proc ${sym}gsym_test_func
$opal_cv_asm_global ${sym}gsym_test_func
${sym}gsym_test_func${opal_cv_asm_label_suffix}
$ompi_cv_asm_endproc ${sym}gsym_test_func
            ],
            [ompi_compile="$CC $CFLAGS -I. conftest_c.c -c > conftest.cmpl 2>&1"
             if AC_TRY_EVAL(ompi_compile) ; then
                # save the warnings
                 cat conftest.cmpl >&AC_FD_CC
                 ompi_link="$CC $CFLAGS conftest_c.$OBJEXT conftest.$OBJEXT -o conftest  $LDFLAGS $LIBS > conftest.link 2>&1"
                 if AC_TRY_EVAL(ompi_link) ; then
                     # save the warnings
                     cat conftest.link >&AC_FD_CC
                     asm_result=1
                 else
                     cat conftest.link >&AC_FD_CC
                     echo "configure: failed C program was: " >&AC_FD_CC
                     cat conftest_c.c >&AC_FD_CC
                     echo "configure: failed ASM program was: " >&AC_FD_CC
                     cat conftest.s >&AC_FD_CC
                     asm_result=0
                 fi
             else
                # save output and failed program
                 cat conftest.cmpl >&AC_FD_CC
                 echo "configure: failed C program was: " >&AC_FD_CC
                 cat conftest.c >&AC_FD_CC
                 asm_result=0
             fi], 
            [asm_result=0])
        if test "$asm_result" = "1" ; then
            opal_cv_asm_gsym="$sym"
            break
        fi
    done
    rm -rf conftest.*
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_LABEL_SUFFIX
dnl
dnl Sets OPAL_ASM_LABEL_SUFFIX to the value to suffix for labels
dnl
dnl I'm sure if I don't have a test for this, there will be some
dnl dumb platform that uses something else
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_LABEL_SUFFIX],[
    AC_MSG_CHECKING([suffix for labels])
    opal_cv_asm_label_suffix=""
    case $host in
        *)
                opal_cv_asm_label_suffix=":"
        ;;
    esac
    AC_MSG_RESULT([$opal_cv_asm_label_suffix])
    AC_DEFINE_UNQUOTED([OPAL_ASM_LABEL_SUFFIX], ["$opal_cv_asm_label_suffix"],
                       [Assembly suffix for labels])
    OPAL_ASM_LABEL_SUFFIX="$opal_cv_asm_label_suffix"
    AC_SUBST(OMPI_AS_LABEL_SUFFIX)
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_ALIGN_LOG
dnl
dnl Sets OPAL_ASM_ALIGN_LOG to 1 if align is specified 
dnl logarithmically, 0 otherwise
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_ALIGN_LOG],[
    AC_REQUIRE([AC_PROG_NM])
    AC_REQUIRE([AC_PROG_GREP])

    AC_CACHE_CHECK([if .align directive takes logarithmic value],
                   [ompi_cv_asm_align_log],
                   [ OMPI_TRY_ASSEMBLE([        $opal_cv_asm_text
        .align 4
        $opal_cv_asm_global foo
        .byte 1
        .align 4
foo$opal_cv_asm_label_suffix
        .byte 2], 
        [ompi_asm_addr=[`$NM conftest.$OBJEXT | $GREP foo | sed -e 's/.*\([0-9a-fA-F][0-9a-fA-F]\).*foo.*/\1/'`]],
        [ompi_asm_addr=""])
    # test for both 16 and 10 (decimal and hex notations)
    echo "configure: .align test address offset is $ompi_asm_addr" >&AC_FD_CC
    if test "$ompi_asm_addr" = "16" -o "$ompi_asm_addr" = "10" ; then
       ompi_cv_asm_align_log="yes"
    else
        ompi_cv_asm_align_log="no"
    fi])

    if test "$ompi_cv_asm_align_log" = "yes" -o "$ompi_cv_asm_align_log" = "1" ; then
        ompi_asm_align_log_result=1
    else
        ompi_asm_align_log_result=0
    fi

    AC_DEFINE_UNQUOTED([OPAL_ASM_ALIGN_LOG],
                       [$asm_align_log_result],
                       [Assembly align directive expects logarithmic value])

    unset omp_asm_addr asm_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_TYPE
dnl
dnl Sets OPAL_ASM_TYPE to the prefix for the function type to 
dnl set a symbol's type as function (needed on ELF for shared
dnl libaries).  If no .type directive is needed, sets OPAL_ASM_TYPE
dnl to an empty string
dnl
dnl We look for @ \# %
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_TYPE],[
        AC_CACHE_CHECK([prefix for function in .type],
                       [opal_cv_asm_type],
                       [_OMPI_CHECK_ASM_TYPE])

    AC_DEFINE_UNQUOTED([OPAL_ASM_TYPE], ["$opal_cv_asm_type"],
                       [How to set function type in .type directive])
    OPAL_ASM_TYPE="$opal_cv_asm_type"
    AC_SUBST(OPAL_ASM_TYPE)
])

AC_DEFUN([_OMPI_CHECK_ASM_TYPE],[
    opal_cv_asm_type=""

    case "${host}" in
    *-sun-solaris*)
        # GCC on solaris seems to accept just about anything, not
        # that what it defines actually works...  So just hardwire
        # to the right answer
        opal_cv_asm_type="#"
    ;;
    *)
        for type  in @ \# % ; do
            asm_result=0
            echo "configure: trying $type" >&AC_FD_CC
            OMPI_TRY_ASSEMBLE([     .type mysym, ${type}function
mysym:],
                 [opal_cv_asm_type="${type}"
                    asm_result=1])
            if test "$asm_result" = "1" ; then
                break
            fi
        done
    ;;
    esac
    rm -f conftest.out

    unset asm_result type
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_SIZE
dnl
dnl Sets OPAL_ASM_SIZE to 1 if we should set .size directives for
dnl each function, 0 otherwise.
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_SIZE],[
    AC_CACHE_CHECK([if .size is needed],
                   [ompi_cv_asm_need_size],
                   [ompi_cv_asm_need_size="no"
                    OMPI_TRY_ASSEMBLE([     .size mysym, 1],
                          [ompi_cv_asm_need_size="yes"])
                    rm -f conftest.out])

    if test "$ompi_cv_asm_need_size" = "yes" ; then
       opal_asm_size=1
    else
       opal_asm_size=0
    fi

    AC_DEFINE_UNQUOTED([OPAL_ASM_SIZE], ["$opal_asm_size"],
                       [Do we need to give a .size directive])
    OPAL_ASM_SIZE="$opal_asm_size"
    AC_SUBST(OPAL_ASM_TYPE)
    unset asm_result
])dnl


# OMPI_CHECK_ASM_GNU_STACKEXEC(var)
# ----------------------------------
# sets shell variable var to the things necessary to
# disable execable stacks with GAS
AC_DEFUN([OMPI_CHECK_ASM_GNU_STACKEXEC], [
    AC_REQUIRE([AC_PROG_GREP])

    AC_CHECK_PROG([OBJDUMP], [objdump], [objdump])
    AC_CACHE_CHECK([if .note.GNU-stack is needed],
        [ompi_cv_asm_gnu_stack_result],
        [AS_IF([test "$OBJDUMP" != ""],
            [ # first, see if a simple C program has it set
             cat >conftest.c <<EOF
int testfunc() {return 0; }
EOF
             OMPI_LOG_COMMAND([$CC $CFLAGS -c conftest.c -o conftest.$OBJEXT],
                 [$OBJDUMP -x conftest.$OBJEXT | $GREP '\.note\.GNU-stack' > /dev/null && ompi_cv_asm_gnu_stack_result=yes],
                 [OMPI_LOG_MSG([the failed program was:], 1)
                  OMPI_LOG_FILE([conftest.c])
                  ompi_cv_asm_gnu_stack_result=no])
             if test "$ompi_cv_asm_gnu_stack_result" != "yes" ; then
                 ompi_cv_asm_gnu_stack_result="no"
             fi
             rm -rf conftest.*],
            [ompi_cv_asm_gnu_stack_result="no"])])
    if test "$ompi_cv_asm_gnu_stack_result" = "yes" ; then
        ompi_cv_asm_gnu_stack=1
    else
        ompi_cv_asm_gnu_stack=0
    fi
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_POWERPC_REG
dnl
dnl See if the notation for specifying registers is X (most everyone)
dnl or rX (OS X)
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_POWERPC_REG],[
    AC_MSG_CHECKING([if PowerPC registers have r prefix])
    OMPI_TRY_ASSEMBLE([$opal_cv_asm_text
        addi 1,1,0],
        [opal_cv_asm_powerpc_r_reg=0],
        [OMPI_TRY_ASSEMBLE([$opal_cv_asm_text
        addi r1,r1,0],
            [opal_cv_asm_powerpc_r_reg=1],
            [AC_MSG_ERROR([Can not determine how to use PPC registers])])])
    if test "$opal_cv_asm_powerpc_r_reg" = "1" ; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    AC_DEFINE_UNQUOTED([OPAL_POWERPC_R_REGISTERS],
                       [$opal_cv_asm_powerpc_r_reg],
                       [Whether r notation is used for ppc registers])
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_POWERPC_64BIT
dnl
dnl On some powerpc chips (the PPC970 or G5), the OS usually runs in
dnl 32 bit mode, even though the hardware can do 64bit things.  If
dnl the compiler will let us, emit code for 64bit test and set type
dnl operations (on a long long).
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_POWERPC_64BIT],[
    if test "$ac_cv_sizeof_long" != "4" ; then
        # this function should only be called in the 32 bit case
        AC_MSG_ERROR([CHECK_POWERPC_64BIT called on 64 bit platform.  Internal error.])
    fi
    AC_MSG_CHECKING([for 64-bit PowerPC assembly support])
        case $host in
            *-darwin*)
                ppc64_result=0
                if test "$opal_cv_asm_powerpc_r_reg" = "1" ; then
                   ldarx_asm="        ldarx r1,r1,r1";
                else
                   ldarx_asm="        ldarx 1,1,1";
                fi
                OMPI_TRY_ASSEMBLE([$opal_cv_asm_text
        $ldarx_asm],
                    [ppc64_result=1],
                    [ppc64_result=0])
            ;;
            *)
                ppc64_result=0
            ;;
        esac

    if test "$ppc64_result" = "1" ; then
        AC_MSG_RESULT([yes])
        ifelse([$1],,:,[$1])
    else
        AC_MSG_RESULT([no])
        ifelse([$2],,:,[$2])
    fi

    unset ppc64_result ldarx_asm
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_SPARCV8PLUS
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_SPARCV8PLUS],[
    AC_MSG_CHECKING([if have Sparc v8+/v9 support])
    sparc_result=0
    OMPI_TRY_ASSEMBLE([$opal_cv_asm_text
	casa [%o0] 0x80, %o1, %o2],
                [sparc_result=1],
                [sparc_result=0])
    if test "$sparc_result" = "1" ; then
        AC_MSG_RESULT([yes])
        ifelse([$1],,:,[$1])
    else
        AC_MSG_RESULT([no])
        ifelse([$2],,:,[$2])
    fi

    unset sparc_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_INLINE_GCC
dnl
dnl Check if the compiler is capable of doing GCC-style inline
dnl assembly.  Some compilers emit a warning and ignore the inline
dnl assembly (xlc on OS X) and compile without error.  Therefore,
dnl the test attempts to run the emited code to check that the
dnl assembly is actually run.  To run this test, one argument to
dnl the macro must be an assembly instruction in gcc format to move 
dnl the value 0 into the register containing the variable ret.  
dnl For PowerPC, this would be:
dnl
dnl   "li %0,0" : "=&r"(ret)
dnl
dnl For testing ia32 assembly, the assembly instruction xaddl is
dnl tested.  The xaddl instruction is used by some of the atomic
dnl implementations so it makes sense to test for it.  In addition,
dnl some compilers (i.e. earlier versions of Sun Studio 12) do not
dnl necessarily handle xaddl properly, so that needs to be detected
dnl during configure time.
dnl
dnl DEFINE OMPI_GCC_INLINE_ASSEMBLY to 0 or 1 depending on GCC
dnl                support
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_INLINE_C_GCC],[
    assembly="$1"
    asm_result="unknown"

    AC_MSG_CHECKING([if $CC supports GCC inline assembly])

    if test "$ompi_cv_c_compiler_vendor" = "portland group" ; then
        # PGI seems to have some issues with our inline assembly.
        # Disable for now.
        asm_result="no (Portland Group)"
    else
        case $host in
            *-aix*)
                # the AIX compilers and linkers really don't do gcc 
                # inline assembly right - disable for now.
                asm_result="no (AIX)"
                ;;
            *)
                if test ! "$assembly" = "" ; then
                        AC_RUN_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT],
[[int ret = 1;
int negone = -1;
__asm__ __volatile__ ($assembly);
return ret;]])],
                    [asm_result="yes"], [asm_result="no"], 
                    [asm_result="unknown"])
                else
                    assembly="test skipped - assuming no"
                fi
                ;;
        esac

        # if we're cross compiling, just try to compile and figure good enough
        if test "$asm_result" = "unknown" ; then
            AC_LINK_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT],
[[int ret = 1;
int negone = -1;
__asm__ __volatile__ ($assembly);
return ret;]])],
            [asm_result="yes"], [asm_result="no"])
        fi
    fi

    AC_MSG_RESULT([$asm_result])

    if test "$asm_result" = "yes" ; then
        OPAL_C_GCC_INLINE_ASSEMBLY=1
    else
        OPAL_C_GCC_INLINE_ASSEMBLY=0
    fi

    AC_DEFINE_UNQUOTED([OPAL_C_GCC_INLINE_ASSEMBLY],
                       [$OPAL_C_GCC_INLINE_ASSEMBLY],
                       [Whether C compiler supports GCC style inline assembly])

    unset OPAL_C_GCC_INLINE_ASSEMBLY assembly asm_result
])dnl

AC_DEFUN([OMPI_CHECK_INLINE_CXX_GCC],[
    assembly="$1"
    asm_result="unknown"

    AC_LANG_PUSH([C++])
    AC_MSG_CHECKING([if $CXX supports GCC inline assembly])

    case $host in
        *-aix*)
            # the AIX compilers and linkers really don't do gcc 
            # inline assembly right - disable for now.
            asm_result="no (AIX)"
            ;;
        *)
            if test ! "$assembly" = "" ; then
            AC_RUN_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT],
[[int ret = 1;
int negone = -1;
__asm__ __volatile__ ($assembly);
return ret;]])],
                    [asm_result="yes"], [asm_result="no"], 
                    [asm_result="unknown"])
            else
                assembly="test skipped - assuming no"
            fi
            ;;
    esac
    # if we're cross compiling, just try to compile and figure good enough
    if test "$asm_result" = "unknown" ; then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT],
[[int ret = 1;
int negone = -1;
__asm__ __volatile__ ($assembly);
return ret;]])],
            [asm_result="yes"], [asm_result="no"])
    fi

    AC_MSG_RESULT([$asm_result])

    if test "$asm_result" = "yes" ; then
        OMPI_CXX_GCC_INLINE_ASSEMBLY=1
    else
        OMPI_CXX_GCC_INLINE_ASSEMBLY=0
    fi

    AC_DEFINE_UNQUOTED([OMPI_CXX_GCC_INLINE_ASSEMBLY],
                       [$OMPI_CXX_GCC_INLINE_ASSEMBLY],
                       [Whether C++ compiler supports GCC style inline assembly])
    AC_LANG_POP([C++])

    unset OMPI_CXX_GCC_INLINE_ASSEMBLY assembly asm_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_INLINE_DEC
dnl
dnl DEFINE OMPI_DEC to 0 or 1 depending on DEC
dnl                support
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_INLINE_C_DEC],[

    AC_MSG_CHECKING([if $CC supports DEC inline assembly])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT[
#include <c_asm.h>]],
[[asm("");
return 0;]])],
        [asm_result="yes"], [asm_result="no"])

    AC_MSG_RESULT([$asm_result])

    if test "$asm_result" = "yes" ; then
        OPAL_C_DEC_INLINE_ASSEMBLY=1
    else
        OPAL_C_DEC_INLINE_ASSEMBLY=0
    fi

    AC_DEFINE_UNQUOTED([OPAL_C_DEC_INLINE_ASSEMBLY],
                       [$OPAL_C_DEC_INLINE_ASSEMBLY],
                       [Whether C compiler supports DEC style inline assembly])

    unset OPAL_C_DEC_INLINE_ASSEMBLY asm_result
])dnl

AC_DEFUN([OMPI_CHECK_INLINE_CXX_DEC],[

    AC_LANG_PUSH([C++])
    AC_MSG_CHECKING([if $CXX supports DEC inline assembly])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([
AC_INCLUDES_DEFAULT[
#include <c_asm.h>]],
[[asm("");
return 0;]])],
        [asm_result="yes"], [asm_result="no"])

    AC_MSG_RESULT([$asm_result])

    if test "$asm_result" = "yes" ; then
        OMPI_CXX_DEC_INLINE_ASSEMBLY=1
    else
        OMPI_CXX_DEC_INLINE_ASSEMBLY=0
    fi

    AC_DEFINE_UNQUOTED([OMPI_CXX_DEC_INLINE_ASSEMBLY],
                       [$OMPI_CXX_DEC_INLINE_ASSEMBLY],
                       [Whether C++ compiler supports DEC style inline assembly])
    AC_LANG_POP([C++])

    unset OMPI_CXX_DEC_INLINE_ASSEMBLY asm_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_INLINE_XLC
dnl
dnl DEFINE OMPI_XLC to 0 or 1 depending on XLC
dnl                support
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_INLINE_C_XLC],[

    AC_MSG_CHECKING([if $CC supports XLC inline assembly])

    OPAL_C_XLC_INLINE_ASSEMBLY=0
    asm_result="no"
    if test "$CC" = "xlc" ; then
        OMPI_XLC_INLINE_ASSEMBLY=1
        asm_result="yes"
    fi

    AC_MSG_RESULT([$asm_result])
    AC_DEFINE_UNQUOTED([OPAL_C_XLC_INLINE_ASSEMBLY],
                       [$OPAL_C_XLC_INLINE_ASSEMBLY],
                       [Whether C compiler supports XLC style inline assembly])

    unset OPAL_C_XLC_INLINE_ASSEMBLY
])dnl

AC_DEFUN([OMPI_CHECK_INLINE_CXX_XLC],[

    AC_MSG_CHECKING([if $CXX supports XLC inline assembly])

    OMPI_CXX_XLC_INLINE_ASSEMBLY=0
    asm_result="no"
    if test "$CXX" = "xlC" -o "$CXX" = "xlc++" ; then
        OMPI_CXX_XLC_INLINE_ASSEMBLY=1
        asm_result="yes"
    fi

    AC_MSG_RESULT([$asm_result])
    AC_DEFINE_UNQUOTED([OMPI_CXX_XLC_INLINE_ASSEMBLY],
                       [$OMPI_CXX_XLC_INLINE_ASSEMBLY],
                       [Whether C++ compiler supports XLC style inline assembly])

    unset OMPI_CXX_XLC_INLINE_ASSEMBLY
])dnl


dnl #################################################################
dnl
dnl OMPI_CONFIG_ASM
dnl
dnl DEFINE OPAL_ASSEMBLY_ARCH to something in sys/architecture.h
dnl DEFINE OPAL_ASSEMBLY_FORMAT to string containing correct
dnl                             format for assembly (not user friendly)
dnl SUBST OPAL_ASSEMBLY_FORMAT to string containing correct
dnl                             format for assembly (not user friendly)
dnl
dnl #################################################################
AC_DEFUN([OMPI_CONFIG_ASM],[
    AC_REQUIRE([OMPI_SETUP_CC])
    # Only require C++ if we're building the OMPI project
    m4_ifdef([project_ompi], [AC_REQUIRE([OMPI_SETUP_CXX])])
    AC_REQUIRE([AM_PROG_AS])

    # OS X Leopard ld bus errors if you have "-g" or "-gX" in the link line
    # with our assembly (!).  So remove it from CCASFLAGS if it's
    # there (and we're on Leopard).
    OMPI_VAR_SCOPE_PUSH([ompi_config_asm_flags_new ompi_config_asm_flag])
    AC_MSG_CHECKING([if need to remove -g from CCASFLAGS])
    case "$host" in
        *-apple-darwin9.*)
            for ompi_config_asm_flag in $CCASFLAGS; do
                # See http://www.gnu.org/software/autoconf/manual/html_node/Quadrigraphs.html#Quadrigraphs
                # for an explanation of @<:@ and @:>@ -- they m4 expand 
                # to [ and ]
                case $ompi_config_asm_flag in
                -g)            ;;
                -g@<:@0-9@:>@) ;;
                *)
                    ompi_config_asm_flags_new="$ompi_config_asm_flags_new $ompi_config_asm_flag"
                    ;;
                esac
            done
            CCASFLAGS="$ompi_config_asm_flags_new"
            AC_MSG_RESULT([OS X Leopard - yes ($CCASFLAGS)])
            ;;
        *)
            AC_MSG_RESULT([no])
            ;;
    esac
    OMPI_VAR_SCOPE_POP

    AC_MSG_CHECKING([whether to enable smp locks])
    AC_ARG_ENABLE([smp-locks], 
        [AC_HELP_STRING([--enable-smp-locks],
            [enable smp locks in atomic ops.  Do not disable if code will ever run in SMP or multi-threaded environment. (default: enabled)])])
    if test "$enable_smp_locks" != "no"; then
        AC_MSG_RESULT([yes])
        want_smp_locks=1
    else
        AC_MSG_RESULT([no])
        want_smp_locks=0
    fi
    AC_DEFINE_UNQUOTED([OPAL_WANT_SMP_LOCKS], [$want_smp_locks],
                       [whether we want to have smp locks in atomic ops or not])

    if test "$ompi_cv_c_compiler_vendor" = "microsoft" ; then
        ompi_cv_asm_arch="WINDOWS"
    else
        OMPI_CHECK_ASM_PROC
        OMPI_CHECK_ASM_TEXT
        OMPI_CHECK_ASM_GLOBAL
        OMPI_CHECK_ASM_GNU_STACKEXEC
        OMPI_CHECK_ASM_LABEL_SUFFIX
        OMPI_CHECK_ASM_GSYM
        OMPI_CHECK_ASM_LSYM
        OMPI_CHECK_ASM_TYPE
        OMPI_CHECK_ASM_SIZE
        OMPI_CHECK_ASM_ALIGN_LOG

        # find our architecture for purposes of assembly stuff
        ompi_cv_asm_arch="UNSUPPORTED"
        OMPI_GCC_INLINE_ASSIGN=""
        OPAL_ASM_SUPPORT_64BIT=0
        case "${host}" in
        i?86-*|x86_64*)
            if test "$ac_cv_sizeof_long" = "4" ; then
                ompi_cv_asm_arch="IA32"
            else
                ompi_cv_asm_arch="AMD64"
            fi
            OPAL_ASM_SUPPORT_64BIT=1
            OMPI_GCC_INLINE_ASSIGN='"xaddl %1,%0" : "=m"(ret), "+r"(negone) : "m"(ret)'
            ;;

        ia64-*)
            ompi_cv_asm_arch="IA64"
            OPAL_ASM_SUPPORT_64BIT=1
            OMPI_GCC_INLINE_ASSIGN='"mov %0=r0\n;;\n" : "=&r"(ret)'
            ;;

        alpha-*|alphaev[[4-8]]-*|alphaev56-*|alphaev6[[78]]-*)
            ompi_cv_asm_arch="ALPHA"
            OPAL_ASM_SUPPORT_64BIT=1
            OMPI_GCC_INLINE_ASSIGN='"bis [$]31,[$]31,%0" : "=&r"(ret)'
            ;;

        armv7*)
            ompi_cv_asm_arch="ARM"
            OPAL_ASM_SUPPORT_64BIT=1
            OPAL_ASM_ARM_VERSION=7
            AC_DEFINE_UNQUOTED([OPAL_ASM_ARM_VERSION], [$OPAL_ASM_ARM_VERSION],
                               [What ARM assembly version to use])
            OMPI_GCC_INLINE_ASSIGN='"mov %0, #0" : "=&r"(ret)'
            ;;

        armv6*)
            ompi_cv_asm_arch="ARM"
            OPAL_ASM_SUPPORT_64BIT=0
            OPAL_ASM_ARM_VERSION=6
            CCASFLAGS="$CCASFLAGS -march=armv7-a"
            AC_DEFINE_UNQUOTED([OPAL_ASM_ARM_VERSION], [$OPAL_ASM_ARM_VERSION],
                               [What ARM assembly version to use])
            OMPI_GCC_INLINE_ASSIGN='"mov %0, #0" : "=&r"(ret)'
            ;;

        armv5*linux*|armv4*linux*)
            # uses Linux kernel helpers for some atomic operations
            ompi_cv_asm_arch="ARM"
            OPAL_ASM_SUPPORT_64BIT=0
            OPAL_ASM_ARM_VERSION=5
            CCASFLAGS="$CCASFLAGS -march=armv7-a"
            AC_DEFINE_UNQUOTED([OPAL_ASM_ARM_VERSION], [$OPAL_ASM_ARM_VERSION],
                               [What ARM assembly version to use])
            OMPI_GCC_INLINE_ASSIGN='"mov %0, #0" : "=&r"(ret)'
            ;;

        mips-*|mips64*)
            # Should really find some way to make sure that we are on
            # a MIPS III machine (r4000 and later)
            ompi_cv_asm_arch="MIPS"
            OPAL_ASM_SUPPORT_64BIT=1
            OMPI_GCC_INLINE_ASSIGN='"or %0,[$]0,[$]0" : "=&r"(ret)'
            ;;

        powerpc-*|powerpc64-*)
            OMPI_CHECK_POWERPC_REG
            if test "$ac_cv_sizeof_long" = "4" ; then
                ompi_cv_asm_arch="POWERPC32"

                # Note that on some platforms (Apple G5), even if we are
                # compiling in 32 bit mode (and therefore should assume
                # sizeof(long) == 4), we can use the 64 bit test and set
                # operations.
                OMPI_CHECK_POWERPC_64BIT(OPAL_ASM_SUPPORT_64BIT=1)
            elif test "$ac_cv_sizeof_long" = "8" ; then
                OPAL_ASM_SUPPORT_64BIT=1
                ompi_cv_asm_arch="POWERPC64"
            else
                AC_MSG_ERROR([Could not determine PowerPC word size: $ac_cv_sizeof_long])
            fi
            OMPI_GCC_INLINE_ASSIGN='"1: li %0,0" : "=&r"(ret)'
            ;;

        sparc*-*)
            # SPARC v9 (and above) are the only ones with 64bit support
            # if compiling 32 bit, see if we are v9 (aka v8plus) or
            # earlier (casa is v8+/v9). 
            if test "$ac_cv_sizeof_long" = "4" ; then
                have_v8plus=0
                OMPI_CHECK_SPARCV8PLUS([have_v8plus=1])
                if test "$have_v8plus" = "0" ; then
                    OPAL_ASM_SUPPORT_64BIT=0
                    ompi_cv_asm_arch="SPARC"
AC_MSG_WARN([Sparc v8 target is not supported in this release of Open MPI.])
AC_MSG_WARN([You must specify the target architecture v8plus])
AC_MSG_WARN([(cc: -xarch=v8plus, gcc: -mcpu=v9) for CFLAGS, CXXFLAGS,])
AC_MSG_WARN([FFLAGS, and FCFLAGS to compile Open MPI in 32 bit mode on])
AC_MSG_WARN([Sparc processors])
AC_MSG_ERROR([Can not continue.])
                else
                    OPAL_ASM_SUPPORT_64BIT=1
                    ompi_cv_asm_arch="SPARCV9_32"
                fi

            elif test "$ac_cv_sizeof_long" = "8" ; then
                OPAL_ASM_SUPPORT_64BIT=1
                ompi_cv_asm_arch="SPARCV9_64"
            else
                AC_MSG_ERROR([Could not determine Sparc word size: $ac_cv_sizeof_long])
            fi
            OMPI_GCC_INLINE_ASSIGN='"mov 0,%0" : "=&r"(ret)'
            ;;

        *)
            AC_MSG_ERROR([No atomic primitives available for $host])
            ;;
        esac

        AC_DEFINE_UNQUOTED([OPAL_ASM_SUPPORT_64BIT],
            [$OPAL_ASM_SUPPORT_64BIT],
            [Whether we can do 64bit assembly operations or not.  Should not be used outside of the assembly header files])
        AC_SUBST([OPAL_ASM_SUPPORT_64BIT])

        #
        # figure out if we need any special function start / stop code
        #
        case $host_os in
        aix*)
            ompi_asm_arch_config="aix"
            ;;
        *)
            ompi_asm_arch_config="default"
            ;;
         esac

         # now that we know our architecture, try to inline assemble
         OMPI_CHECK_INLINE_C_GCC([$OMPI_GCC_INLINE_ASSIGN])
         OMPI_CHECK_INLINE_C_DEC
         OMPI_CHECK_INLINE_C_XLC
         # Only check C++ if we're building the OMPI project and we
         # want the C++ bindings
         m4_ifdef([project_ompi],
                  [AS_IF([test "$WANT_MPI_CXX_SUPPORT" = "1"],
                         [OMPI_CHECK_INLINE_CXX_GCC([$OMPI_GCC_INLINE_ASSIGN])
                          OMPI_CHECK_INLINE_CXX_DEC
                          OMPI_CHECK_INLINE_CXX_XLC])])

         # format:
         #   config_file-text-global-label_suffix-gsym-lsym-type-size-align_log-ppc_r_reg-64_bit-gnu_stack
         asm_format="${ompi_asm_arch_config}"
         asm_format="${asm_format}-${opal_cv_asm_text}-${opal_cv_asm_global}"
         asm_format="${asm_format}-${opal_cv_asm_label_suffix}-${opal_cv_asm_gsym}"
         asm_format="${asm_format}-${opal_cv_asm_lsym}"
         asm_format="${asm_format}-${opal_cv_asm_type}-${opal_asm_size}"
         asm_format="${asm_format}-${ompi_asm_align_log_result}"
         if test "$ompi_cv_asm_arch" = "POWERPC32" -o "$ompi_cv_asm_arch" = "POWERPC64" ; then
             asm_format="${asm_format}-${opal_cv_asm_powerpc_r_reg}"
         else
             asm_format="${asm_format}-1"
         fi
         asm_format="${asm_format}-${OPAL_ASM_SUPPORT_64BIT}"
         ompi_cv_asm_format="${asm_format}-${ompi_cv_asm_gnu_stack}"
         # For the Makefile, need to escape the $ as $$.  Don't display
         # this version, but make sure the Makefile gives the right thing
         # when regenerating the files because the base has been touched.
         OPAL_ASSEMBLY_FORMAT=`echo "$ompi_cv_asm_format" | sed -e 's/\\\$/\\\$\\\$/'`

        AC_MSG_CHECKING([for assembly format])
        AC_MSG_RESULT([$ompi_cv_asm_format])
        AC_DEFINE_UNQUOTED([OPAL_ASSEMBLY_FORMAT], ["$OPAL_ASSEMBLY_FORMAT"],
                           [Format of assembly file])
        AC_SUBST([OPAL_ASSEMBLY_FORMAT])
    fi # if cv_c_compiler_vendor = microsoft

    result="OMPI_$ompi_cv_asm_arch"
    OPAL_ASSEMBLY_ARCH="$ompi_cv_asm_arch"
    AC_MSG_CHECKING([for asssembly architecture])
    AC_MSG_RESULT([$ompi_cv_asm_arch])
    AC_DEFINE_UNQUOTED([OPAL_ASSEMBLY_ARCH], [$result],
        [Architecture type of assembly to use for atomic operations])
    AC_SUBST([OPAL_ASSEMBLY_ARCH])

    OMPI_ASM_FIND_FILE

    unset result asm_format
])dnl


dnl #################################################################
dnl
dnl OMPI_ASM_FIND_FILE
dnl
dnl
dnl do all the evil mojo to provide a working assembly file
dnl
dnl #################################################################
AC_DEFUN([OMPI_ASM_FIND_FILE], [
    AC_REQUIRE([AC_PROG_GREP])
    AC_REQUIRE([AC_PROG_FGREP])

if test "$ompi_cv_asm_arch" != "WINDOWS" ; then
    AC_CHECK_PROG([PERL], [perl], [perl])

    # see if we have a pre-built one already
    AC_MSG_CHECKING([for pre-built assembly file])
    ompi_cv_asm_file=""
    if $GREP "$ompi_cv_asm_arch" "${top_ompi_srcdir}/opal/asm/asm-data.txt" | $FGREP "$ompi_cv_asm_format" >conftest.out 2>&1 ; then
        ompi_cv_asm_file="`cut -f3 conftest.out`"
        if test ! "$ompi_cv_asm_file" = "" ; then
            ompi_cv_asm_file="atomic-${ompi_cv_asm_file}.s"
            if test -f "${top_ompi_srcdir}/opal/asm/generated/${ompi_cv_asm_file}" ; then
                AC_MSG_RESULT([yes ($ompi_cv_asm_file)])
            else
                AC_MSG_RESULT([no ($ompi_cv_asm_file not found)])
                ompi_cv_asm_file=""
            fi
        fi
    else
        AC_MSG_RESULT([no (not in asm-data)])
    fi
    rm -rf conftest.*

    if test "$ompi_cv_asm_file" = "" ; then
        if test ! "$PERL" = "" ; then
            # we have perl...  Can we generate a file?
            AC_MSG_CHECKING([whether possible to generate assembly file])
            mkdir -p opal/asm/generated
            ompi_cv_asm_file="atomic-local.s"
            ompi_try='$PERL $top_ompi_srcdir/opal/asm/generate-asm.pl $ompi_cv_asm_arch "$ompi_cv_asm_format" $top_ompi_srcdir/opal/asm/base $top_ompi_builddir/opal/asm/generated/$ompi_cv_asm_file >conftest.out 2>&1'
            if AC_TRY_EVAL(ompi_try) ; then
                # save the warnings
                cat conftest.out >&AC_FD_CC
                AC_MSG_RESULT([yes])
            else
                # save output
                cat conftest.out >&AC_FD_CC
                ompi_cv_asm_file=""
                AC_MSG_RESULT([failed])
                AC_MSG_WARN([Could not build atomic operations assembly file.])
                AC_MSG_WARN([There will be no atomic operations for this build.])
            fi
        else
            AC_MSG_WARN([Could not find prebuilt atomic operations file and could not])
            AC_MSG_WARN([find perl to attempt to generate a custom assembly file.])
            AC_MSG_WARN([There will be no atomic operations for this build.])
        fi
    fi
    rm -rf conftest.*
else
    # On windows with VC++, atomics are done with compiler primitives
    ompi_cv_asm_file=""
fi

    AC_MSG_CHECKING([for atomic assembly filename])
    if test "$ompi_cv_asm_file" = "" ; then
        AC_MSG_RESULT([none])
        result=0
    else
        AC_MSG_RESULT([$ompi_cv_asm_file])
        result=1
    fi

    AC_DEFINE_UNQUOTED([OPAL_HAVE_ASM_FILE], [$result],
                       [Whether there is an atomic assembly file available])
    AM_CONDITIONAL([OPAL_HAVE_ASM_FILE], [test "$result" = "1"])

    OMPI_ASM_FILE=$ompi_cv_asm_file
    AC_SUBST(OMPI_ASM_FILE)
])dnl
