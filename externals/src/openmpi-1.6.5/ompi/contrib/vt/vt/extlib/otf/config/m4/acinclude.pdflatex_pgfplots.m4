AC_DEFUN([CHECK_PDFLATEX_PGFPLOTS],
[
    AC_ARG_VAR([PDFTEX], [pdfTeX typesetter command])

    AC_CHECK_PROGS([PDFTEX], [pdflatex pdftex])
    if test x"$PDFTEX" != x; then
        AC_DEFINE_UNQUOTED([PDFTEX], ["$PDFTEX"], [pdfTeX typesetter command.])

        AC_MSG_CHECKING([for PGFPLOTS version >= 1.4])

        cat << EOF >conftest.tex
\documentclass[[a4paper,10pt]]{article}
\nonstopmode
\usepackage{pgfplots}
\begin{document}
\pgfplotstableread{
col1 col2
1 2
}\testtable
test
\end{document}
EOF

        $PDFTEX conftest.tex >/dev/null 2>&1
        if test $? -eq 0; then
            AC_MSG_RESULT([yes])
            AC_DEFINE([HAVE_PGFPLOTS_1_4], [1], [Define to 1 if you have the TeX package PGFPLOTS version >=1.4.])
        else
            AC_MSG_RESULT([no])
        fi

        rm -f conftest.*

    fi
])
