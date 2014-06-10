CC = mpicc
F77 = mpif77
INCLUDE_DIR = 
# because := is not universally avalible, we have to play games to use the
# user-specified LDFLAGS and OUR_LIBS env. variables (if set)
OUR_LIBS =   ${LDFLAGS} ${LIBS}
USER_CFLAGS =  -D_REENTRANT -I/Volumes/spin/Development/clasp-Lisp/externals/src/openmpi-1.6.5/opal/mca/hwloc/hwloc132/hwloc/include   -I/usr/include/infiniband -I/usr/include/infiniband  $(INCLUDE_DIR)
USER_FFLAGS =  -D_REENTRANT -I/Volumes/spin/Development/clasp-Lisp/externals/src/openmpi-1.6.5/opal/mca/hwloc/hwloc132/hwloc/include   -I/usr/include/infiniband -I/usr/include/infiniband  $(INCLUDE_DIR)
CTESTS = simple perf async coll_test coll_perf misc file_info excl large_array \
     atomicity noncontig i_noncontig noncontig_coll split_coll shared_fp \
     large_file psimple error status noncontig_coll2 aggregation1 aggregation2 \
     async-multiple ordered_fp hindexed
FTESTS = fcoll_test fperf fmisc pfcoll_test 
srcdir=/Volumes/spin/Development/clasp-Lisp/externals/src/openmpi-1.6.5/ompi/mca/io/romio/romio/test

VPATH=.:${srcdir}


all: c-test 

c-test: $(CTESTS)

default: all

.c.o:
	$(CC) $(USER_CFLAGS) -c $<

.f.o:
	$(F77) $(USER_FFLAGS) -c $<

.o:
	$(CC) $(USER_CFLAGS) -o  $@ $< $(OUR_LIBS)

clean: 
	rm -f .P* PI* *.o 
	rm -f $(CTESTS) $(FTESTS)
	@-rm -f work.pc work.pcl

#
# The Fortran tests must be built with the Fortran linker
fperf: fperf.o
	$(F77) $(USER_FFLAGS) -o  fperf fperf.o $(OUR_LIBS)

fcoll_test: fcoll_test.o
	$(F77) $(USER_FFLAGS) -o  fcoll_test fcoll_test.o $(OUR_LIBS)

fmisc: fmisc.o
	$(F77) $(USER_FFLAGS) -o  fmisc fmisc.o $(OUR_LIBS)

pfcoll_test: pfcoll_test.o	
	$(F77) $(USER_FFLAGS) -o  pfcoll_test pfcoll_test.o $(OUR_LIBS)

#testing: 
#	-./runtests $(TESTARGS)

cleanall:
	-@rm -f .P* PI* misc.c large_file.c fcoll_test.f fmisc.f fperf.f \
	mpif.h $(CTESTS) $(FTESTS) *.out runtests pfcoll_test.f
