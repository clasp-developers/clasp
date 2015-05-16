#!/usr/bin/env sh

# Seems that the version of clang-format on some systems is outdated; use the externals-clasp one.

[ -e 'local.config' ] || echo "Please set up local.config before running clang-format.sh"

# Horrible hack to read local.config into bash
eval $(perl -ne 'if (/^export\s+[A-Z_]+ /) {s/\$\K\((.*?)\)/{$1}/g;s/\s+=\s+/=/;print "$_\n"}' local.config)

# Hackety hack hack
git ls-files src/ include/ \
	| perl -ne 'chomp;print "$_\n" if -f $_ and (/\.[hc][hcp]?p?$/) and not -l;' \
	| xargs -P$PJOBS -n1 ${EXTERNALS_SOURCE_DIR}/build/release/bin/clang-format -i
