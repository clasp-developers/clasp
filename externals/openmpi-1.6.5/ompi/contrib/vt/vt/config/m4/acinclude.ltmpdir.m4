AC_DEFUN([ACVT_LTMPDIR],
[
	local_tmp_dir=

	AC_ARG_WITH(local-tmp-dir,
		AC_HELP_STRING([--with-local-tmp-dir=DIR],
		[give the path for node-local temporary directory, default: /tmp]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-local-tmp-dir' not properly set])])
		local_tmp_dir=$withval
        ])

	AS_IF([test x"$local_tmp_dir" != x],
	[
		AC_DEFINE_UNQUOTED(DEFAULT_PFORM_LDIR,
		["$local_tmp_dir"], [Path for node-local temporary directory])
	])
])

