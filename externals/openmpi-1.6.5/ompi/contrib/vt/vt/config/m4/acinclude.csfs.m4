AC_DEFUN([ACVT_CSFS],
[
	have_csfs="no"

	AC_MSG_CHECKING([if build filesystem is case sensitive])

	cat > conftest.out << EOF
lowercase
EOF

	cat > CONFTEST.OUT <<EOF
uppercase
EOF

	AS_IF([test "`cat conftest.out`" = "lowercase"],
	[have_csfs="yes"])

	AC_MSG_RESULT([$have_csfs])

	rm -f conftest.out CONFTEST.OUT
])

