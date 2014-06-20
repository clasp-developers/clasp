#!/bin/bash 
# change the following for install path, note 
# that VER is appended to the path.
VER="trunk"
SW_INSTALL_ROOT=/tmp/work/gshipman/ompi/install

PLATFORM=ornl/cray_xt_cnl_romio

ORTED_MAKEFILE=orte/tools/orted/Makefile

if test -z "`grep "orted_LDFLAGS =.*-all-static" ${ORTED_MAKEFILE}`"; then 
    echo "WARNING: patching ${ORTED_MAKEFILE} to build it static" 
    sed -i 's/orted_LDFLAGS =/orted_LDFLAGS = -all-static/g' ${ORTED_MAKEFILE}
fi



./configure \
 NM=/usr/bin/nm \
 CC=gcc \
 CXX=g++ \
 CFLAGS="-I/opt/xt-pe/default/include/  -I/opt/xt-catamount/default/catamount/linux/include/ " \
 CPPFLAGS=-I/opt/xt-pe/default/include/ \
 FCFLAGS=-I/opt/xt-pe/default/include/ \
 FFLAGS=-I/opt/xt-pe/default/include/ \
 LDFLAGS="-L/opt/xt-service/default/lib/snos64 -L/opt/xt-pe/default/cnos/linux/64/lib -L/opt/xt-mpt/default/lib/snos64" \
 LIBS="-lpct -lalpslli -lalpsutil -lportals -lpthread" \
  --with-wrapper-cflags="-Wmissing-prototypes -I/tmp/work/gshipman/ompi/install/trunk/include" \
  --with-wrapper-ldflags="-Wmissing-prototypes  -lnsl -lutil -lpct -lalpslli -lalpsutil -lportals -lpthread -lm -L/opt/xt-service/default/lib/snos64 -L/opt/xt-pe/default/cnos/linux/64/lib -L/opt/xt-mpt/default/lib/snos64"\
 --build=x86_64-unknown-linux-gnu  \
 --host=x86_64-cray-linux-gnu \
 --disable-mpi-f77\
 --disable-mpi-f90\
 --without-tm \
 --with-platform=./contrib/platform/${PLATFORM} \
  --with-io-romio-flags="build_alias=x86_64-unknown-linux-gnu \
  host_alias=x86_64-cray-linux-gnu \
  --enable-ltdl-convenience --no-recursion" \
  --with-alps=yes \
  --with-contrib-vt-flags="--with-platform=linux" \
--prefix="$SW_INSTALL_ROOT/$VER" | tee build.log

#gmake all install | tee -a build.log
#chmod -R go+rx $SW_INSTALL_ROOT/$VER-$CMP
