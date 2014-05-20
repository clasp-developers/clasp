! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2010 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Do ***not*** copy this file to the directory where your Fortran
! fortran application is compiled unless it is absolutely necessary!  Most
! modern Fortran compilers now support the -I command line flag, which
! tells the compiler where to find .h files (specifically, this one).  For
! example:
!
!      shell$ mpif77 foo.f -o foo -I$OMPI_HOME/include
!
! will probably do the trick (assuming that you have set OMPI_HOME
! properly).
!
! That being said, OMPI's "mpif77" wrapper compiler should
! automatically include the -I option for you.  The following command
! should be equivalent to the command listed above:
!
!      shell$ mpif77 foo.f -o foo
!
! You should not copy this file to your local directory because it is
! possible that this file will be changed between versions of Open MPI.
! Indeed, this mpif.h is incompatible with the mpif.f of other
! implementations of MPI.  Using this mpif.h with other implementations
! of MPI, or with other versions of Open MPI will result in undefined
! behavior (to include incorrect results, segmentation faults,
! unexplainable "hanging" in your application, etc.).  Always use the
! -I command line option instead (or let mpif77 do it for you).
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!     This file contains the bulk of the Open MPI Fortran interface.  It
!     is included as a back-end file to both mpif.h (i.e., the
!     standardized MPI Fortran header file) and mpi.f90 (the MPI-2
!     Fortran module source file, found in ompi/mpi/f90).
!
!     This file is marginally different than mpif.h.  mpif.h includes
!     some "external" statements that are not suitable for use with the
!     MPI-2 F90 module, and therefore cannot be included in the mpi.f90
!     source file.  Hence, this file is essentially everything that
!     needs to be in the standardized mpif.h *except* the "external"
!     statements, and is therefore suitable to be included in mpi.f90.
!

!     First, however, include some output from configure.
!
      include 'mpif-config.h'

!
!     MPI version
!
      integer MPI_VERSION, MPI_SUBVERSION

      parameter (MPI_VERSION=2)
      parameter (MPI_SUBVERSION=1)
!
!     Miscellaneous constants
!
      integer MPI_ANY_SOURCE, MPI_ANY_TAG
      integer MPI_PROC_NULL
      integer MPI_ROOT
      integer MPI_UNDEFINED
      integer MPI_CART, MPI_GRAPH, MPI_KEYVAL_INVALID
      integer MPI_SOURCE, MPI_TAG, MPI_ERROR
      integer MPI_TAG_UB, MPI_HOST, MPI_IO, MPI_WTIME_IS_GLOBAL
      integer MPI_APPNUM, MPI_LASTUSEDCODE, MPI_UNIVERSE_SIZE
      integer IMPI_CLIENT_SIZE, IMPI_CLIENT_COLOR
      integer IMPI_HOST_SIZE, IMPI_HOST_COLOR
      integer MPI_BSEND_OVERHEAD
      integer MPI_ORDER_C, MPI_ORDER_FORTRAN
      integer MPI_DISTRIBUTE_BLOCK, MPI_DISTRIBUTE_CYCLIC
      integer MPI_DISTRIBUTE_NONE, MPI_DISTRIBUTE_DFLT_DARG
      integer MPI_TYPECLASS_INTEGER, MPI_TYPECLASS_REAL
      integer MPI_TYPECLASS_COMPLEX
      integer MPI_MODE_NOCHECK, MPI_MODE_NOPRECEDE, MPI_MODE_NOPUT
      integer MPI_MODE_NOSTORE, MPI_MODE_NOSUCCEED
      integer MPI_LOCK_EXCLUSIVE, MPI_LOCK_SHARED
      integer MPI_WIN_BASE, MPI_WIN_SIZE, MPI_WIN_DISP_UNIT

      parameter (MPI_ANY_SOURCE=-1)
      parameter (MPI_ANY_TAG=-1)
      parameter (MPI_PROC_NULL=-2)
      parameter (MPI_ROOT=-4)
      parameter (MPI_UNDEFINED=-32766)
      parameter (MPI_CART=1)
      parameter (MPI_GRAPH=2)
      parameter (MPI_KEYVAL_INVALID=-1)
      parameter (MPI_SOURCE=1)
      parameter (MPI_TAG=2)
      parameter (MPI_ERROR=3)
      parameter (MPI_TAG_UB=0)
      parameter (MPI_HOST=1)
      parameter (MPI_IO=2)
      parameter (MPI_WTIME_IS_GLOBAL=3)
      parameter (MPI_APPNUM=4)
      parameter (MPI_LASTUSEDCODE=5)
      parameter (MPI_UNIVERSE_SIZE=6)
      parameter (MPI_WIN_BASE=7)
      parameter (MPI_WIN_SIZE=8)
      parameter (MPI_WIN_DISP_UNIT=9)
      parameter (IMPI_CLIENT_SIZE=10)
      parameter (IMPI_CLIENT_COLOR=11)
      parameter (IMPI_HOST_SIZE=12)
      parameter (IMPI_HOST_COLOR=13)

      parameter (MPI_BSEND_OVERHEAD=128)
      parameter (MPI_ORDER_C=0)
      parameter (MPI_ORDER_FORTRAN=1)
      parameter (MPI_DISTRIBUTE_BLOCK=0)
      parameter (MPI_DISTRIBUTE_CYCLIC=1)
      parameter (MPI_DISTRIBUTE_NONE=2)
      parameter (MPI_DISTRIBUTE_DFLT_DARG=-1)
      parameter (MPI_TYPECLASS_INTEGER=1)
      parameter (MPI_TYPECLASS_REAL=2)
      parameter (MPI_TYPECLASS_COMPLEX=3)
      parameter (MPI_MODE_NOCHECK=1)
      parameter (MPI_MODE_NOPRECEDE=2)
      parameter (MPI_MODE_NOPUT=4)
      parameter (MPI_MODE_NOSTORE=8)
      parameter (MPI_MODE_NOSUCCEED=16)
      parameter (MPI_LOCK_EXCLUSIVE=1)
      parameter (MPI_LOCK_SHARED=2)

!
!     MPI sentinel values
!
!     Several of these types were chosen with care to match specific
!     overloaded functions in the F90 bindings.  They should also match
!     the types of their corresponding C variables.  Do not arbitrarily
!     change their types without also updating the F90 bindings and
!     their corresponding types in ompi/mpi/f77/constants.h and
!     ompi/mpi/runtime/ompi_init.c!
!
!     MPI_BOTTOM is only used where choice buffers can be used (meaning
!     that we already have overloaded F90 bindings for all available
!     types), so any type is fine.
      integer MPI_BOTTOM
!     MPI_IN_PLACE has the same rationale as MPI_BOTTOM.
      integer MPI_IN_PLACE
!     Making MPI_ARGV_NULL be the same type as the parameter that is
!     exepected in the F90 binding for MPI_COMM_SPAWN means that we
!     don't need another binding for MPI_COMM_SPAWN.
      character MPI_ARGV_NULL(1)
!     The array_of_argv parameter in the F90 bindings for
!     MPI_COMM_SPAWN_MULTIPLE takes a variable number of dimensions
!     (specified by the "count" parameter), so it's not possible to have
!     a single variable match all possible values.  Hence, make it an
!     entirely different type (one that would never likely be used by a
!     correct program, e.g., double) and have a separate F90 binding for
!     matching just this type.
      double precision MPI_ARGVS_NULL
!     MPI_ERRCODES_IGNORE has similar rationale to MPI_ARGV_NULL.  The
!     F77 functions are all smart enough to check that the errcodes
!     parameter is not ERRCODES_IGNORE before assigning values into it
!     (hence, the fact that this is an array of only 1 element does not
!     matter -- we'll never overrun it because we never assign values
!     into it).
      integer MPI_ERRCODES_IGNORE(1)
!     MPI_STATUS_IGNORE has similar rationale to MPI_ERRCODES_IGNORE.
      integer MPI_STATUS_IGNORE(MPI_STATUS_SIZE)
!     MPI_STATUSES_IGNORE has similar rationale to MPI_ARGVS_NULL.
      double precision MPI_STATUSES_IGNORE

      common/mpi_fortran_bottom/MPI_BOTTOM
      common/mpi_fortran_in_place/MPI_IN_PLACE
      common/mpi_fortran_argv_null/MPI_ARGV_NULL
      common/mpi_fortran_argvs_null/MPI_ARGVS_NULL
      common/mpi_fortran_errcodes_ignore/MPI_ERRCODES_IGNORE
      common/mpi_fortran_status_ignore/MPI_STATUS_IGNORE
      common/mpi_fortran_statuses_ignore/MPI_STATUSES_IGNORE
!
!     NULL "handles" (indices)
!
      integer MPI_GROUP_NULL, MPI_COMM_NULL, MPI_DATATYPE_NULL
      integer MPI_REQUEST_NULL, MPI_OP_NULL, MPI_ERRHANDLER_NULL
      integer MPI_INFO_NULL, MPI_WIN_NULL

      parameter (MPI_GROUP_NULL=0)
      parameter (MPI_COMM_NULL=2)
      parameter (MPI_DATATYPE_NULL=0)
      parameter (MPI_REQUEST_NULL=0)
      parameter (MPI_OP_NULL=0)
      parameter (MPI_ERRHANDLER_NULL=0)
      parameter (MPI_INFO_NULL=0)
      parameter (MPI_WIN_NULL=0)
!
!     MPI_Init_thread constants
!
      integer MPI_THREAD_SINGLE, MPI_THREAD_FUNNELED
      integer MPI_THREAD_SERIALIZED, MPI_THREAD_MULTIPLE

      parameter (MPI_THREAD_SINGLE=0)
      parameter (MPI_THREAD_FUNNELED=1)
      parameter (MPI_THREAD_SERIALIZED=2)
      parameter (MPI_THREAD_MULTIPLE=3)
!
!     error classes
!
      integer MPI_SUCCESS
      integer MPI_ERR_BUFFER
      integer MPI_ERR_COUNT
      integer MPI_ERR_TYPE
      integer MPI_ERR_TAG
      integer MPI_ERR_COMM
      integer MPI_ERR_RANK
      integer MPI_ERR_REQUEST
      integer MPI_ERR_ROOT
      integer MPI_ERR_GROUP
      integer MPI_ERR_OP
      integer MPI_ERR_TOPOLOGY
      integer MPI_ERR_DIMS
      integer MPI_ERR_ARG
      integer MPI_ERR_UNKNOWN
      integer MPI_ERR_TRUNCATE
      integer MPI_ERR_OTHER
      integer MPI_ERR_INTERN
      integer MPI_ERR_IN_STATUS
      integer MPI_ERR_PENDING
      integer MPI_ERR_ACCESS
      integer MPI_ERR_AMODE
      integer MPI_ERR_ASSERT
      integer MPI_ERR_BAD_FILE
      integer MPI_ERR_BASE
      integer MPI_ERR_CONVERSION
      integer MPI_ERR_DISP
      integer MPI_ERR_DUP_DATAREP
      integer MPI_ERR_FILE_EXISTS
      integer MPI_ERR_FILE_IN_USE
      integer MPI_ERR_FILE
      integer MPI_ERR_INFO_KEY
      integer MPI_ERR_INFO_NOKEY
      integer MPI_ERR_INFO_VALUE
      integer MPI_ERR_INFO
      integer MPI_ERR_IO
      integer MPI_ERR_KEYVAL
      integer MPI_ERR_LOCKTYPE
      integer MPI_ERR_NAME
      integer MPI_ERR_NO_MEM
      integer MPI_ERR_NOT_SAME
      integer MPI_ERR_NO_SPACE
      integer MPI_ERR_NO_SUCH_FILE
      integer MPI_ERR_PORT
      integer MPI_ERR_QUOTA
      integer MPI_ERR_READ_ONLY
      integer MPI_ERR_RMA_CONFLICT
      integer MPI_ERR_RMA_SYNC
      integer MPI_ERR_SERVICE
      integer MPI_ERR_SIZE
      integer MPI_ERR_SPAWN
      integer MPI_ERR_UNSUPPORTED_DATAREP
      integer MPI_ERR_UNSUPPORTED_OPERATION
      integer MPI_ERR_WIN

      integer MPI_ERR_SYSRESOURCE
      integer MPI_ERR_LASTCODE

      parameter( MPI_SUCCESS                  = 0)
      parameter( MPI_ERR_BUFFER               = 1)
      parameter( MPI_ERR_COUNT                = 2)
      parameter( MPI_ERR_TYPE                 = 3)
      parameter( MPI_ERR_TAG                  = 4)
      parameter( MPI_ERR_COMM                 = 5)
      parameter( MPI_ERR_RANK                 = 6)
      parameter( MPI_ERR_REQUEST              = 7)
      parameter( MPI_ERR_ROOT                 = 8)
      parameter( MPI_ERR_GROUP                = 9)
      parameter( MPI_ERR_OP                   = 10)
      parameter( MPI_ERR_TOPOLOGY             = 11)
      parameter( MPI_ERR_DIMS                 = 12)
      parameter( MPI_ERR_ARG                  = 13)
      parameter( MPI_ERR_UNKNOWN              = 14)
      parameter( MPI_ERR_TRUNCATE             = 15)
      parameter( MPI_ERR_OTHER                = 16)
      parameter( MPI_ERR_INTERN               = 17)
      parameter( MPI_ERR_IN_STATUS            = 18)
      parameter( MPI_ERR_PENDING              = 19)
      parameter( MPI_ERR_ACCESS               = 20)
      parameter( MPI_ERR_AMODE                = 21)
      parameter( MPI_ERR_ASSERT               = 22)
      parameter( MPI_ERR_BAD_FILE             = 23)
      parameter( MPI_ERR_BASE                 = 24)
      parameter( MPI_ERR_CONVERSION           = 25)
      parameter( MPI_ERR_DISP                 = 26)
      parameter( MPI_ERR_DUP_DATAREP          = 27)
      parameter( MPI_ERR_FILE_EXISTS          = 28)
      parameter( MPI_ERR_FILE_IN_USE          = 29)
      parameter( MPI_ERR_FILE                 = 30)
      parameter( MPI_ERR_INFO_KEY             = 31)
      parameter( MPI_ERR_INFO_NOKEY           = 32)
      parameter( MPI_ERR_INFO_VALUE           = 33)
      parameter( MPI_ERR_INFO                 = 34)
      parameter( MPI_ERR_IO                   = 35)
      parameter( MPI_ERR_KEYVAL               = 36)
      parameter( MPI_ERR_LOCKTYPE             = 37)
      parameter( MPI_ERR_NAME                 = 38)
      parameter( MPI_ERR_NO_MEM               = 39)
      parameter( MPI_ERR_NOT_SAME             = 40)
      parameter( MPI_ERR_NO_SPACE             = 41)
      parameter( MPI_ERR_NO_SUCH_FILE         = 42)
      parameter( MPI_ERR_PORT                 = 43)
      parameter( MPI_ERR_QUOTA                = 44)
      parameter( MPI_ERR_READ_ONLY            = 45)
      parameter( MPI_ERR_RMA_CONFLICT         = 46)
      parameter( MPI_ERR_RMA_SYNC             = 47)
      parameter( MPI_ERR_SERVICE              = 48)
      parameter( MPI_ERR_SIZE                 = 49)
      parameter( MPI_ERR_SPAWN                = 50)
      parameter( MPI_ERR_UNSUPPORTED_DATAREP  = 51)
      parameter( MPI_ERR_UNSUPPORTED_OPERATION= 52)
      parameter( MPI_ERR_WIN                  = 53)

      parameter( MPI_ERR_SYSRESOURCE          = -2)
      parameter( MPI_ERR_LASTCODE             = 54)

!
!     comparison results
!
      integer MPI_IDENT, MPI_CONGRUENT, MPI_SIMILAR, MPI_UNEQUAL

      parameter (MPI_IDENT=0)
      parameter (MPI_CONGRUENT=1)
      parameter (MPI_SIMILAR=2)
      parameter (MPI_UNEQUAL=3)
!
!     datatype combiners
!
      integer MPI_COMBINER_NAMED
      integer MPI_COMBINER_DUP
      integer MPI_COMBINER_CONTIGUOUS
      integer MPI_COMBINER_VECTOR
      integer MPI_COMBINER_HVECTOR_INTEGER
      integer MPI_COMBINER_HVECTOR
      integer MPI_COMBINER_INDEXED
      integer MPI_COMBINER_HINDEXED_INTEGER
      integer MPI_COMBINER_HINDEXED
      integer MPI_COMBINER_INDEXED_BLOCK
      integer MPI_COMBINER_STRUCT_INTEGER
      integer MPI_COMBINER_STRUCT
      integer MPI_COMBINER_SUBARRAY
      integer MPI_COMBINER_DARRAY
      integer MPI_COMBINER_F90_REAL
      integer MPI_COMBINER_F90_COMPLEX
      integer MPI_COMBINER_F90_INTEGER
      integer MPI_COMBINER_RESIZED

      parameter (MPI_COMBINER_NAMED=0)
      parameter (MPI_COMBINER_DUP=1)
      parameter (MPI_COMBINER_CONTIGUOUS=2)
      parameter (MPI_COMBINER_VECTOR=3)
      parameter (MPI_COMBINER_HVECTOR_INTEGER=4)
      parameter (MPI_COMBINER_HVECTOR=5)
      parameter (MPI_COMBINER_INDEXED=6)
      parameter (MPI_COMBINER_HINDEXED_INTEGER=7)
      parameter (MPI_COMBINER_HINDEXED=8)
      parameter (MPI_COMBINER_INDEXED_BLOCK=9)
      parameter (MPI_COMBINER_STRUCT_INTEGER=10)
      parameter (MPI_COMBINER_STRUCT=11)
      parameter (MPI_COMBINER_SUBARRAY=12)
      parameter (MPI_COMBINER_DARRAY=13)
      parameter (MPI_COMBINER_F90_REAL=14)
      parameter (MPI_COMBINER_F90_COMPLEX=15)
      parameter (MPI_COMBINER_F90_INTEGER=16)
      parameter (MPI_COMBINER_RESIZED=17)
!
!     lookup table indices
!
      integer MPI_COMM_WORLD, MPI_COMM_SELF
      integer MPI_GROUP_EMPTY
      integer MPI_ERRORS_ARE_FATAL, MPI_ERRORS_RETURN

      parameter (MPI_COMM_WORLD=0)
      parameter (MPI_COMM_SELF=1)
      parameter (MPI_GROUP_EMPTY=1)
      parameter (MPI_ERRORS_ARE_FATAL=1)
      parameter (MPI_ERRORS_RETURN=2)

      integer MPI_BYTE, MPI_PACKED, MPI_UB, MPI_LB
      integer MPI_CHARACTER, MPI_LOGICAL
      integer MPI_INTEGER, MPI_INTEGER1, MPI_INTEGER2, MPI_INTEGER4
      integer MPI_INTEGER8, MPI_INTEGER16
      integer MPI_REAL, MPI_REAL2, MPI_REAL4, MPI_REAL8, MPI_REAL16
      integer MPI_DOUBLE_PRECISION
      integer MPI_COMPLEX, MPI_COMPLEX8, MPI_COMPLEX16, MPI_COMPLEX32
      integer MPI_DOUBLE_COMPLEX
      integer MPI_2REAL, MPI_2DOUBLE_PRECISION, MPI_2INTEGER
      integer MPI_2COMPLEX, MPI_2DOUBLE_COMPLEX
! Note that MPI_LOGICALx are not defined by the MPI spec, but there are
! other MPI implementations that have them, so it's good for us to have
! as well.
      integer MPI_LOGICAL1, MPI_LOGICAL2, MPI_LOGICAL4, MPI_LOGICAL8
! All other MPI types including the C and C++, as well as the
! ones defined in the MPI 2.2
      integer MPI_WCHAR, MPI_CHAR
      integer MPI_SIGNED_CHAR, MPI_UNSIGNED_CHAR
      integer MPI_SHORT, MPI_UNSIGNED_SHORT
      integer MPI_INT, MPI_UNSIGNED, MPI_LONG
      integer MPI_UNSIGNED_LONG, MPI_LONG_LONG_INT
      integer MPI_UNSIGNED_LONG_LONG
      integer MPI_FLOAT, MPI_DOUBLE, MPI_LONG_DOUBLE
      integer MPI_FLOAT_INT, MPI_DOUBLE_INT
      integer MPI_LONGDBL_INT, MPI_LONG_INT
      integer MPI_2INT, MPI_SHORT_INT
      integer MPI_CXX_BOOL, MPI_CXX_CPLEX
      integer MPI_CXX_DBLCPLEX, MPI_CXX_LDBLCPLEX
      integer MPI_INT8_T, MPI_UINT8_T
      integer MPI_INT16_T, MPI_UINT16_T
      integer MPI_INT32_T, MPI_UINT32_T
      integer MPI_INT64_T, MPI_UINT64_T
      integer MPI_AINT, MPI_OFFSET
!
!     Do NOT change the order of these parameters
!
      parameter (MPI_BYTE               =  1)
      parameter (MPI_PACKED             =  2)
      parameter (MPI_UB                 =  3)
      parameter (MPI_LB                 =  4)
      parameter (MPI_CHARACTER          =  5)
      parameter (MPI_LOGICAL            =  6)
      parameter (MPI_INTEGER            =  7)
      parameter (MPI_INTEGER1           =  8)
      parameter (MPI_INTEGER2           =  9)
      parameter (MPI_INTEGER4           = 10)
      parameter (MPI_INTEGER8           = 11)
      parameter (MPI_INTEGER16          = 12)
      parameter (MPI_REAL               = 13)
      parameter (MPI_REAL4              = 14)
      parameter (MPI_REAL8              = 15)
      parameter (MPI_REAL16             = 16)
      parameter (MPI_DOUBLE_PRECISION   = 17)
      parameter (MPI_COMPLEX            = 18)
      parameter (MPI_COMPLEX8           = 19)
      parameter (MPI_COMPLEX16          = 20)
      parameter (MPI_COMPLEX32          = 21)
      parameter (MPI_DOUBLE_COMPLEX     = 22)
      parameter (MPI_2REAL              = 23)
      parameter (MPI_2DOUBLE_PRECISION  = 24)
      parameter (MPI_2INTEGER           = 25)
      parameter (MPI_2COMPLEX           = 26)
      parameter (MPI_2DOUBLE_COMPLEX    = 27)
      parameter (MPI_REAL2              = 28)
      parameter (MPI_LOGICAL1           = 29)
      parameter (MPI_LOGICAL2           = 30)
      parameter (MPI_LOGICAL4           = 31)
      parameter (MPI_LOGICAL8           = 32)
      parameter (MPI_WCHAR              = 33)
      parameter (MPI_CHAR               = 34)
      parameter (MPI_UNSIGNED_CHAR      = 35)
      parameter (MPI_SIGNED_CHAR        = 36)
      parameter (MPI_SHORT              = 37)
      parameter (MPI_UNSIGNED_SHORT     = 38)
      parameter (MPI_INT                = 39)
      parameter (MPI_UNSIGNED           = 40)
      parameter (MPI_LONG               = 41)
      parameter (MPI_UNSIGNED_LONG      = 42)
      parameter (MPI_LONG_LONG_INT      = 43)
      parameter (MPI_UNSIGNED_LONG_LONG = 44)
      parameter (MPI_FLOAT              = 45)
      parameter (MPI_DOUBLE             = 46)
      parameter (MPI_LONG_DOUBLE        = 47)
      parameter (MPI_FLOAT_INT          = 48)
      parameter (MPI_DOUBLE_INT         = 49)
      parameter (MPI_LONGDBL_INT        = 50)
      parameter (MPI_LONG_INT           = 51)
      parameter (MPI_2INT               = 52)
      parameter (MPI_SHORT_INT          = 53)
      parameter (MPI_CXX_BOOL           = 54)
      parameter (MPI_CXX_CPLEX          = 55)
      parameter (MPI_CXX_DBLCPLEX       = 56)
      parameter (MPI_CXX_LDBLCPLEX      = 57)
      parameter (MPI_INT8_T             = 58)
      parameter (MPI_UINT8_T            = 59)
      parameter (MPI_INT16_T            = 60)
      parameter (MPI_UINT16_T           = 61)
      parameter (MPI_INT32_T            = 62)
      parameter (MPI_UINT32_T           = 63)
      parameter (MPI_INT64_T            = 64)
      parameter (MPI_UINT64_T           = 65)
      parameter (MPI_AINT               = 66)
      parameter (MPI_OFFSET             = 67)

      integer MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_LAND
      integer MPI_BAND, MPI_LOR, MPI_BOR, MPI_LXOR, MPI_BXOR
      integer MPI_MAXLOC, MPI_MINLOC, MPI_REPLACE

      parameter (MPI_MAX=1)
      parameter (MPI_MIN=2)
      parameter (MPI_SUM=3)
      parameter (MPI_PROD=4)
      parameter (MPI_LAND=5)
      parameter (MPI_BAND=6)
      parameter (MPI_LOR=7)
      parameter (MPI_BOR=8)
      parameter (MPI_LXOR=9)
      parameter (MPI_BXOR=10)
      parameter (MPI_MAXLOC=11)
      parameter (MPI_MINLOC=12)
      parameter (MPI_REPLACE=13)
