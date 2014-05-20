#!/bin/sh

srcfile=$1
option=$2

if test -z "$srcfile"; then
	option="--help"
else
	if ! test -f $srcfile; then
		echo "$0: $srcfile: No such file or directory"
		exit 1
	fi

	otf_vers=`cat $srcfile`
	eval "$otf_vers"
fi

if test -z "$option"; then
	option="--full"
fi

case "$option" in
	--major)
		echo $major
		;;
	--minor)
		echo $minor
		;;
	--sub)
		echo $sub
		;;
	--string)
		echo $string
		;;
	--full)
		out="$major.$minor"
		if test x"$sub" != "x0"; then
			out="$out.$sub"
		fi
		if test x"$string" != "x"; then
			out="$out$string"
		fi
		echo "$out"
		;;
	--library)
		echo $library
		;;
	-h|--help)
		cat <<EOF
$0 <srcfile> <option>

<srcfile> - Text version file
<option>  - One of:
    --full    - Full version number
    --major   - Major version number
    --minor   - Minor version number
    --string  - Version string (alpha, beta, etc)
    --library - Library version number
    --help    - This message
EOF
		;;
	*)
		echo "Unrecognized option $option. Run $0 --help for options"
		exit 1
		;;
esac

exit 0

