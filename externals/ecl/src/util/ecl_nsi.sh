#!/bin/sh
source=$1
where=$2
cd $where
dest=ecl.nsi
where=`pwd -W`
echo $where
set -x
sed -e '/!@INSTALLFILES@/,$d' -e "s,@ECLDIR@,$where," $source > $dest
find . -type f -maxdepth 1 | sed -e '/^\.$/d;s,^./,,g;s,/,\\,g;'\
    -e 's,^\(.*\)$,  File "${ECLDIR}\\\1",g' >> $dest
find . -type d -maxdepth 1 | grep -v ecl.exe | grep -v ecl.nsi | \
  sed -e '/^\.$/d;s,^./,,g;s,/,\\,g;' \
    -e 's,^\(.*\)$,  File /r "${ECLDIR}\\\1",g' >> $dest
sed '1,/!@INSTALLFILES@/d;/!@DELETEFILES@/,$d' $source >> $dest
find . -type f -maxdepth 1 | sed -e '/^\.$/d;s,^./,,g;s,/,\\,g;'\
    -e 's,^\(.*\)$,  Delete "$INSTDIR\\\1",g' >> $dest
find . -type d -maxdepth 1 | grep -v ecl.exe | grep -v ecl.nsi | \
  sed -e '/^\.$/d;s,^./,,g;s,/,\\,g;' \
    -e 's,^\(.*\)$,  RMDir /r "$INSTDIR\\\1",g' >> $dest
sed '1,/!@DELETEFILES@/d' $source >> $dest
