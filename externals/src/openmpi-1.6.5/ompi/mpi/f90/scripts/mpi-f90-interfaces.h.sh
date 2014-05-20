#! /bin/sh
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Do a little error checking

if test ! -f "$1/fortran_kinds.sh"; then
    echo "ERROR: Cannot find fortran_kinds.sh" >&2
    exit 1
elif test -z "$1/fortran_kinds.sh"; then
    echo "ERROR: fortran_kinds.sh appears to be empty!" >&2
    exit 1
fi

# Read in the KIND information

. "$1/fortran_kinds.sh"

# Setup

output=1
allranks="0 $ranks"

#------------------------------------------------------------------------

# Helper functions

start() {
    check_size $2
    if test "$output" = "1"; then        
        echo "interface $1"
    fi
}

end() {
    if test "$output" = "1"; then
        cat <<EOF
end interface


EOF
    fi
}

# A few hard-coded functions that cannot pass through to the F77
# equivalents

start MPI_Wtick small
if test "$output" = "1"; then
    cat <<EOF

function MPI_Wtick()
    double precision MPI_Wtick
end function MPI_Wtick

EOF
fi
end MPI_Wtick

start MPI_Wtime small
if test "$output" = "1"; then
    cat <<EOF

function MPI_Wtime()
    double precision MPI_Wtime
end function MPI_Wtime

EOF
fi
end MPI_Wtime

#------------------------------------------------------------------------

output_1() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errorcode, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Abort small
output_1 MPI_Abort
end MPI_Abort

#------------------------------------------------------------------------

output_2() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, op, win, ierr)
  include 'mpif-config.h'
  ${type}, intent(in) :: origin_addr
  integer, intent(in) :: origin_count
  integer, intent(in) :: origin_datatype
  integer, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
  integer, intent(in) :: target_count
  integer, intent(in) :: target_datatype
  integer, intent(in) :: op
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Accumulate medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_2 MPI_Accumulate ${rank} CH "character${dim}"
  output_2 MPI_Accumulate ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_2 MPI_Accumulate ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_2 MPI_Accumulate ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_2 MPI_Accumulate ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Accumulate

#------------------------------------------------------------------------

output_3() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorclass, ierr)
  integer, intent(in) :: errorclass
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_class small
output_3 MPI_Add_error_class
end MPI_Add_error_class

#------------------------------------------------------------------------

output_4() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorclass, errorcode, ierr)
  integer, intent(in) :: errorclass
  integer, intent(out) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_code small
output_4 MPI_Add_error_code
end MPI_Add_error_code

#------------------------------------------------------------------------

output_5() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, string, ierr)
  integer, intent(in) :: errorcode
  character(len=*), intent(in) :: string
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Add_error_string small
output_5 MPI_Add_error_string
end MPI_Add_error_string

#------------------------------------------------------------------------

output_6() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(location, address, ierr)
  ${type}, intent(in) :: location
  integer, intent(out) :: address
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Address medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_6 MPI_Address ${rank} CH "character${dim}"
  output_6 MPI_Address ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_6 MPI_Address ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_6 MPI_Address ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_6 MPI_Address ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Address

#------------------------------------------------------------------------

output_7() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Allgather large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_7 MPI_Allgather ${rank} CH "character${dim}"
  output_7 MPI_Allgather ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_7 MPI_Allgather ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_7 MPI_Allgather ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_7 MPI_Allgather ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Allgather

#------------------------------------------------------------------------

output_8() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, dimension(*), intent(in) :: recvcounts
  integer, dimension(*), intent(in) :: displs
  integer, intent(in) :: recvtype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Allgatherv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_8 MPI_Allgatherv ${rank} CH "character${dim}"
  output_8 MPI_Allgatherv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_8 MPI_Allgatherv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_8 MPI_Allgatherv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_8 MPI_Allgatherv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Allgatherv

#------------------------------------------------------------------------

output_9() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(size, info, baseptr, ierr)
  include 'mpif-config.h'
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size
  integer, intent(in) :: info
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: baseptr
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Alloc_mem medium
output_9 MPI_Alloc_mem
end MPI_Alloc_mem

#------------------------------------------------------------------------

output_10() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Allreduce large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_10 MPI_Allreduce ${rank} CH "character${dim}"
  output_10 MPI_Allreduce ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_10 MPI_Allreduce ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_10 MPI_Allreduce ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_10 MPI_Allreduce ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Allreduce

#------------------------------------------------------------------------

output_11() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Alltoall large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_11 MPI_Alltoall ${rank} CH "character${dim}"
  output_11 MPI_Alltoall ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_11 MPI_Alltoall ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_11 MPI_Alltoall ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_11 MPI_Alltoall ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Alltoall

#------------------------------------------------------------------------

output_12() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtype, recvbuf, &
        recvcounts, rdispls, recvtype, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, dimension(*), intent(in) :: sendcounts
  integer, dimension(*), intent(in) :: sdispls
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, dimension(*), intent(in) :: recvcounts
  integer, dimension(*), intent(in) :: rdispls
  integer, intent(in) :: recvtype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Alltoallv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_12 MPI_Alltoallv ${rank} CH "character${dim}"
  output_12 MPI_Alltoallv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_12 MPI_Alltoallv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_12 MPI_Alltoallv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_12 MPI_Alltoallv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Alltoallv

#------------------------------------------------------------------------

output_13() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, &
        recvcounts, rdispls, recvtypes, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, dimension(*), intent(in) :: sendcounts
  integer, dimension(*), intent(in) :: sdispls
  integer, dimension(*), intent(in) :: sendtypes
  ${type}, intent(out) :: recvbuf
  integer, dimension(*), intent(in) :: recvcounts
  integer, dimension(*), intent(in) :: rdispls
  integer, dimension(*), intent(in) :: recvtypes
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Alltoallw large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_13 MPI_Alltoallw ${rank} CH "character${dim}"
  output_13 MPI_Alltoallw ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_13 MPI_Alltoallw ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_13 MPI_Alltoallw ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_13 MPI_Alltoallw ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Alltoallw

#------------------------------------------------------------------------

output_14() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_delete small
output_14 MPI_Attr_delete
end MPI_Attr_delete

#------------------------------------------------------------------------

output_15() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, attribute_val, flag, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_get small
output_15 MPI_Attr_get
end MPI_Attr_get

#------------------------------------------------------------------------

output_16() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, keyval, attribute_val, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Attr_put small
output_16 MPI_Attr_put
end MPI_Attr_put

#------------------------------------------------------------------------

output_17() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Barrier small
output_17 MPI_Barrier
end MPI_Barrier

#------------------------------------------------------------------------

output_18() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buffer, count, datatype, root, comm&
        , ierr)
  ${type}, intent(in) :: buffer
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Bcast medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_18 MPI_Bcast ${rank} CH "character${dim}"
  output_18 MPI_Bcast ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_18 MPI_Bcast ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_18 MPI_Bcast ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_18 MPI_Bcast ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Bcast

#------------------------------------------------------------------------

output_19() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Bsend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_19 MPI_Bsend ${rank} CH "character${dim}"
  output_19 MPI_Bsend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_19 MPI_Bsend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_19 MPI_Bsend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_19 MPI_Bsend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Bsend

#------------------------------------------------------------------------

output_20() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Bsend_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_20 MPI_Bsend_init ${rank} CH "character${dim}"
  output_20 MPI_Bsend_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_20 MPI_Bsend_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_20 MPI_Bsend_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_20 MPI_Bsend_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Bsend_init

#------------------------------------------------------------------------

output_21() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buffer, size, ierr)
  ${type}, intent(in) :: buffer
  integer, intent(in) :: size
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Buffer_attach medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_21 MPI_Buffer_attach ${rank} CH "character${dim}"
  output_21 MPI_Buffer_attach ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_21 MPI_Buffer_attach ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_21 MPI_Buffer_attach ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_21 MPI_Buffer_attach ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Buffer_attach

#------------------------------------------------------------------------

output_22() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buffer, size, ierr)
  ${type}, intent(out) :: buffer
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Buffer_detach medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_22 MPI_Buffer_detach ${rank} CH "character${dim}"
  output_22 MPI_Buffer_detach ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_22 MPI_Buffer_detach ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_22 MPI_Buffer_detach ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_22 MPI_Buffer_detach ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Buffer_detach

#------------------------------------------------------------------------

output_23() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  integer, intent(in) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cancel small
output_23 MPI_Cancel
end MPI_Cancel

#------------------------------------------------------------------------

output_24() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, maxdims, coords, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_coords small
output_24 MPI_Cart_coords
end MPI_Cart_coords

#------------------------------------------------------------------------

output_25() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(old_comm, ndims, dims, periods, reorder, &
        comm_cart, ierr)
  integer, intent(in) :: old_comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  logical, dimension(*), intent(in) :: periods
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_cart
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_create small
output_25 MPI_Cart_create
end MPI_Cart_create

#------------------------------------------------------------------------

output_26() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, maxdims, dims, periods, coords&
        , ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: maxdims
  integer, dimension(*), intent(out) :: dims
  logical, dimension(*), intent(out) :: periods
  integer, dimension(*), intent(out) :: coords
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_get small
output_26 MPI_Cart_get
end MPI_Cart_get

#------------------------------------------------------------------------

output_27() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ndims, dims, periods, newrank&
        , ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: dims
  integer, dimension(*), intent(in) :: periods
  integer, intent(out) :: newrank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_map small
output_27 MPI_Cart_map
end MPI_Cart_map

#------------------------------------------------------------------------

output_28() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, coords, rank, ierr)
  integer, intent(in) :: comm
  integer, dimension(*), intent(in) :: coords
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_rank small
output_28 MPI_Cart_rank
end MPI_Cart_rank

#------------------------------------------------------------------------

output_29() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, direction, disp, rank_source, rank_dest&
        , ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: direction
  integer, intent(in) :: disp
  integer, intent(out) :: rank_source
  integer, intent(out) :: rank_dest
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_shift small
output_29 MPI_Cart_shift
end MPI_Cart_shift

#------------------------------------------------------------------------

output_30() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, remain_dims, new_comm, ierr)
  integer, intent(in) :: comm
  logical, dimension(*), intent(in) :: remain_dims
  integer, intent(out) :: new_comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cart_sub small
output_30 MPI_Cart_sub
end MPI_Cart_sub

#------------------------------------------------------------------------

output_31() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ndims, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: ndims
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Cartdim_get small
output_31 MPI_Cartdim_get
end MPI_Cartdim_get

#------------------------------------------------------------------------

output_32() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errorcode, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_call_errhandler small
output_32 MPI_Comm_call_errhandler
end MPI_Comm_call_errhandler

#------------------------------------------------------------------------

output_33() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm1, comm2, result, ierr)
  integer, intent(in) :: comm1
  integer, intent(in) :: comm2
  integer, intent(out) :: result
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_compare small
output_33 MPI_Comm_compare
end MPI_Comm_compare

#------------------------------------------------------------------------

output_34() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, newcomm, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: group
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create small
output_34 MPI_Comm_create
end MPI_Comm_create

#------------------------------------------------------------------------

output_35() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create_errhandler small
output_35 MPI_Comm_create_errhandler
end MPI_Comm_create_errhandler

#------------------------------------------------------------------------

output_36() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierr)
  include 'mpif-config.h'
  external :: comm_copy_attr_fn
  external :: comm_delete_attr_fn
  integer, intent(out) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_create_keyval small
output_36 MPI_Comm_create_keyval
end MPI_Comm_create_keyval

#------------------------------------------------------------------------

output_37() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, ierr)
  integer, intent(inout) :: comm
  integer, intent(in) :: comm_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_delete_attr small
output_37 MPI_Comm_delete_attr
end MPI_Comm_delete_attr

#------------------------------------------------------------------------

output_38() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, newcomm, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_dup small
output_38 MPI_Comm_dup
end MPI_Comm_dup

#------------------------------------------------------------------------

output_39() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  integer, intent(inout) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_free small
output_39 MPI_Comm_free
end MPI_Comm_free

#------------------------------------------------------------------------

output_40() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_keyval, ierr)
  integer, intent(inout) :: comm_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_free_keyval small
output_40 MPI_Comm_free_keyval
end MPI_Comm_free_keyval

#------------------------------------------------------------------------

output_41() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, attribute_val, flag, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_attr small
output_41 MPI_Comm_get_attr
end MPI_Comm_get_attr

#------------------------------------------------------------------------

output_42() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, erhandler, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: erhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_errhandler small
output_42 MPI_Comm_get_errhandler
end MPI_Comm_get_errhandler

#------------------------------------------------------------------------

output_43() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_name, resultlen, ierr)
  integer, intent(in) :: comm
  character(len=*), intent(out) :: comm_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_name small
output_43 MPI_Comm_get_name
end MPI_Comm_get_name

#------------------------------------------------------------------------

output_44() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_group small
output_44 MPI_Comm_group
end MPI_Comm_group

#------------------------------------------------------------------------

output_45() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_rank small
output_45 MPI_Comm_rank
end MPI_Comm_rank

#------------------------------------------------------------------------

output_46() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, group, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_remote_group small
output_46 MPI_Comm_remote_group
end MPI_Comm_remote_group

#------------------------------------------------------------------------

output_47() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, size, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_remote_size small
output_47 MPI_Comm_remote_size
end MPI_Comm_remote_size

#------------------------------------------------------------------------

output_48() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_keyval, attribute_val, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: comm
  integer, intent(in) :: comm_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_attr small
output_48 MPI_Comm_set_attr
end MPI_Comm_set_attr

#------------------------------------------------------------------------

output_49() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_errhandler small
output_49 MPI_Comm_set_errhandler
end MPI_Comm_set_errhandler

#------------------------------------------------------------------------

output_50() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, comm_name, ierr)
  integer, intent(inout) :: comm
  character(len=*), intent(in) :: comm_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_set_name small
output_50 MPI_Comm_set_name
end MPI_Comm_set_name

#------------------------------------------------------------------------

output_51() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, size, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_size small
output_51 MPI_Comm_size
end MPI_Comm_size

#------------------------------------------------------------------------

output_52() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, color, key, newcomm, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: color
  integer, intent(in) :: key
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_split small
output_52 MPI_Comm_split
end MPI_Comm_split

#------------------------------------------------------------------------

output_53() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, flag, ierr)
  integer, intent(in) :: comm
  logical, intent(in) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_test_inter small
output_53 MPI_Comm_test_inter
end MPI_Comm_test_inter

#------------------------------------------------------------------------

output_54() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(nnodes, ndims, dims, ierr)
  integer, intent(in) :: nnodes
  integer, intent(in) :: ndims
  integer, dimension(*), intent(inout) :: dims
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Dims_create small
output_54 MPI_Dims_create
end MPI_Dims_create

#------------------------------------------------------------------------

output_55() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_create small
output_55 MPI_Errhandler_create
end MPI_Errhandler_create

#------------------------------------------------------------------------

output_56() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errhandler, ierr)
  integer, intent(inout) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_free small
output_56 MPI_Errhandler_free
end MPI_Errhandler_free

#------------------------------------------------------------------------

output_57() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_get small
output_57 MPI_Errhandler_get
end MPI_Errhandler_get

#------------------------------------------------------------------------

output_58() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, errhandler, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Errhandler_set small
output_58 MPI_Errhandler_set
end MPI_Errhandler_set

#------------------------------------------------------------------------

output_59() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, errorclass, ierr)
  integer, intent(in) :: errorcode
  integer, intent(out) :: errorclass
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Error_class small
output_59 MPI_Error_class
end MPI_Error_class

#------------------------------------------------------------------------

output_60() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(errorcode, string, resultlen, ierr)
  integer, intent(in) :: errorcode
  character(len=*), intent(out) :: string
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Error_string small
output_60 MPI_Error_string
end MPI_Error_string

#------------------------------------------------------------------------

output_61() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Exscan large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_61 MPI_Exscan ${rank} CH "character${dim}"
  output_61 MPI_Exscan ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_61 MPI_Exscan ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_61 MPI_Exscan ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_61 MPI_Exscan ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Exscan

#------------------------------------------------------------------------

output_62() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, errorcode, ierr)
  integer, intent(in) :: fh
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_call_errhandler small
output_62 MPI_File_call_errhandler
end MPI_File_call_errhandler

#------------------------------------------------------------------------

output_63() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, ierr)
  integer, intent(inout) :: fh
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_close small
output_63 MPI_File_close
end MPI_File_close

#------------------------------------------------------------------------

output_64() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_create_errhandler small
output_64 MPI_File_create_errhandler
end MPI_File_create_errhandler

#------------------------------------------------------------------------

output_65() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(filename, info, ierr)
  character(len=*), intent(in) :: filename
  integer, intent(in) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_delete small
output_65 MPI_File_delete
end MPI_File_delete

#------------------------------------------------------------------------

output_66() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, amode, ierr)
  integer, intent(in) :: fh
  integer, intent(out) :: amode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_amode small
output_66 MPI_File_get_amode
end MPI_File_get_amode

#------------------------------------------------------------------------

output_67() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, flag, ierr)
  integer, intent(in) :: fh
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_atomicity small
output_67 MPI_File_get_atomicity
end MPI_File_get_atomicity

#------------------------------------------------------------------------

output_68() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, disp, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer(kind=MPI_OFFSET_KIND), intent(out) :: disp
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_byte_offset small
output_68 MPI_File_get_byte_offset
end MPI_File_get_byte_offset

#------------------------------------------------------------------------

output_69() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(file, errhandler, ierr)
  integer, intent(in) :: file
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_errhandler small
output_69 MPI_File_get_errhandler
end MPI_File_get_errhandler

#------------------------------------------------------------------------

output_70() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, group, ierr)
  integer, intent(in) :: fh
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_group small
output_70 MPI_File_get_group
end MPI_File_get_group

#------------------------------------------------------------------------

output_71() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, info_used, ierr)
  integer, intent(in) :: fh
  integer, intent(out) :: info_used
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_info small
output_71 MPI_File_get_info
end MPI_File_get_info

#------------------------------------------------------------------------

output_72() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: offset
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_position small
output_72 MPI_File_get_position
end MPI_File_get_position

#------------------------------------------------------------------------

output_73() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: offset
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_position_shared small
output_73 MPI_File_get_position_shared
end MPI_File_get_position_shared

#------------------------------------------------------------------------

output_74() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_size small
output_74 MPI_File_get_size
end MPI_File_get_size

#------------------------------------------------------------------------

output_75() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, datatype, extent, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_type_extent small
output_75 MPI_File_get_type_extent
end MPI_File_get_type_extent

#------------------------------------------------------------------------

output_76() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, disp, etype, filetype, datarep&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(out) :: disp
  integer, intent(out) :: etype
  integer, intent(out) :: filetype
  character(len=*), intent(out) :: datarep
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_get_view small
output_76 MPI_File_get_view
end MPI_File_get_view

#------------------------------------------------------------------------

output_77() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iread medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_77 MPI_File_iread ${rank} CH "character${dim}"
  output_77 MPI_File_iread ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_77 MPI_File_iread ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_77 MPI_File_iread ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_77 MPI_File_iread ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iread

#------------------------------------------------------------------------

output_78() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iread_at medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_78 MPI_File_iread_at ${rank} CH "character${dim}"
  output_78 MPI_File_iread_at ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_78 MPI_File_iread_at ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_78 MPI_File_iread_at ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_78 MPI_File_iread_at ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iread_at

#------------------------------------------------------------------------

output_79() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iread_shared medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_79 MPI_File_iread_shared ${rank} CH "character${dim}"
  output_79 MPI_File_iread_shared ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_79 MPI_File_iread_shared ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_79 MPI_File_iread_shared ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_79 MPI_File_iread_shared ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iread_shared

#------------------------------------------------------------------------

output_80() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iwrite medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_80 MPI_File_iwrite ${rank} CH "character${dim}"
  output_80 MPI_File_iwrite ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_80 MPI_File_iwrite ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_80 MPI_File_iwrite ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_80 MPI_File_iwrite ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iwrite

#------------------------------------------------------------------------

output_81() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        request, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iwrite_at medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_81 MPI_File_iwrite_at ${rank} CH "character${dim}"
  output_81 MPI_File_iwrite_at ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_81 MPI_File_iwrite_at ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_81 MPI_File_iwrite_at ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_81 MPI_File_iwrite_at ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iwrite_at

#------------------------------------------------------------------------

output_82() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, request&
        , ierr)
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_iwrite_shared medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_82 MPI_File_iwrite_shared ${rank} CH "character${dim}"
  output_82 MPI_File_iwrite_shared ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_82 MPI_File_iwrite_shared ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_82 MPI_File_iwrite_shared ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_82 MPI_File_iwrite_shared ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_iwrite_shared

#------------------------------------------------------------------------

output_83() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, filename, amode, info, fh&
        , ierr)
  integer, intent(in) :: comm
  character(len=*), intent(in) :: filename
  integer, intent(in) :: amode
  integer, intent(in) :: info
  integer, intent(out) :: fh
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_open small
output_83 MPI_File_open
end MPI_File_open

#------------------------------------------------------------------------

output_84() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_preallocate small
output_84 MPI_File_preallocate
end MPI_File_preallocate

#------------------------------------------------------------------------

output_85() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_85 MPI_File_read ${rank} CH "character${dim}"
  output_85 MPI_File_read ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_85 MPI_File_read ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_85 MPI_File_read ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_85 MPI_File_read ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read

#------------------------------------------------------------------------

output_86() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_all medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_86 MPI_File_read_all ${rank} CH "character${dim}"
  output_86 MPI_File_read_all ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_86 MPI_File_read_all ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_86 MPI_File_read_all ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_86 MPI_File_read_all ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_all

#------------------------------------------------------------------------

output_87() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, ierr)
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_all_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_87 MPI_File_read_all_begin ${rank} CH "character${dim}"
  output_87 MPI_File_read_all_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_87 MPI_File_read_all_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_87 MPI_File_read_all_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_87 MPI_File_read_all_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_all_begin

#------------------------------------------------------------------------

output_88() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_all_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_88 MPI_File_read_all_end ${rank} CH "character${dim}"
  output_88 MPI_File_read_all_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_88 MPI_File_read_all_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_88 MPI_File_read_all_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_88 MPI_File_read_all_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_all_end

#------------------------------------------------------------------------

output_89() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_at medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_89 MPI_File_read_at ${rank} CH "character${dim}"
  output_89 MPI_File_read_at ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_89 MPI_File_read_at ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_89 MPI_File_read_at ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_89 MPI_File_read_at ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_at

#------------------------------------------------------------------------

output_90() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_at_all medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_90 MPI_File_read_at_all ${rank} CH "character${dim}"
  output_90 MPI_File_read_at_all ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_90 MPI_File_read_at_all ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_90 MPI_File_read_at_all ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_90 MPI_File_read_at_all ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_at_all

#------------------------------------------------------------------------

output_91() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_at_all_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_91 MPI_File_read_at_all_begin ${rank} CH "character${dim}"
  output_91 MPI_File_read_at_all_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_91 MPI_File_read_at_all_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_91 MPI_File_read_at_all_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_91 MPI_File_read_at_all_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_at_all_begin

#------------------------------------------------------------------------

output_92() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  ${type}, intent(out) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_at_all_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_92 MPI_File_read_at_all_end ${rank} CH "character${dim}"
  output_92 MPI_File_read_at_all_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_92 MPI_File_read_at_all_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_92 MPI_File_read_at_all_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_92 MPI_File_read_at_all_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_at_all_end

#------------------------------------------------------------------------

output_93() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_ordered medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_93 MPI_File_read_ordered ${rank} CH "character${dim}"
  output_93 MPI_File_read_ordered ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_93 MPI_File_read_ordered ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_93 MPI_File_read_ordered ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_93 MPI_File_read_ordered ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_ordered

#------------------------------------------------------------------------

output_94() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, ierr)
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_ordered_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_94 MPI_File_read_ord_begin ${rank} CH "character${dim}"
  output_94 MPI_File_read_ord_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_94 MPI_File_read_ord_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_94 MPI_File_read_ord_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_94 MPI_File_read_ord_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_ordered_begin

#------------------------------------------------------------------------

output_95() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_ordered_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_95 MPI_File_read_ordered_end ${rank} CH "character${dim}"
  output_95 MPI_File_read_ordered_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_95 MPI_File_read_ordered_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_95 MPI_File_read_ordered_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_95 MPI_File_read_ordered_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_ordered_end

#------------------------------------------------------------------------

output_96() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_read_shared medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_96 MPI_File_read_shared ${rank} CH "character${dim}"
  output_96 MPI_File_read_shared ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_96 MPI_File_read_shared ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_96 MPI_File_read_shared ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_96 MPI_File_read_shared ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_read_shared

#------------------------------------------------------------------------

output_97() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, whence, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer, intent(in) :: whence
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_seek small
output_97 MPI_File_seek
end MPI_File_seek

#------------------------------------------------------------------------

output_98() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, offset, whence, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  integer, intent(in) :: whence
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_seek_shared small
output_98 MPI_File_seek_shared
end MPI_File_seek_shared

#------------------------------------------------------------------------

output_99() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, flag, ierr)
  integer, intent(inout) :: fh
  logical, intent(in) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_atomicity small
output_99 MPI_File_set_atomicity
end MPI_File_set_atomicity

#------------------------------------------------------------------------

output_100() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(file, errhandler, ierr)
  integer, intent(in) :: file
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_errhandler small
output_100 MPI_File_set_errhandler
end MPI_File_set_errhandler

#------------------------------------------------------------------------

output_101() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, info, ierr)
  integer, intent(inout) :: fh
  integer, intent(in) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_info small
output_101 MPI_File_set_info
end MPI_File_set_info

#------------------------------------------------------------------------

output_102() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, size, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_size small
output_102 MPI_File_set_size
end MPI_File_set_size

#------------------------------------------------------------------------

output_103() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, disp, etype, filetype, datarep, &
        info, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: disp
  integer, intent(in) :: etype
  integer, intent(in) :: filetype
  character(len=*), intent(in) :: datarep
  integer, intent(in) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_set_view small
output_103 MPI_File_set_view
end MPI_File_set_view

#------------------------------------------------------------------------

output_104() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fh, ierr)
  integer, intent(inout) :: fh
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_File_sync small
output_104 MPI_File_sync
end MPI_File_sync

#------------------------------------------------------------------------

output_105() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_105 MPI_File_write ${rank} CH "character${dim}"
  output_105 MPI_File_write ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_105 MPI_File_write ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_105 MPI_File_write ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_105 MPI_File_write ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write

#------------------------------------------------------------------------

output_106() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_all medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_106 MPI_File_write_all ${rank} CH "character${dim}"
  output_106 MPI_File_write_all ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_106 MPI_File_write_all ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_106 MPI_File_write_all ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_106 MPI_File_write_all ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_all

#------------------------------------------------------------------------

output_107() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, ierr)
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_all_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_107 MPI_File_write_all_begin ${rank} CH "character${dim}"
  output_107 MPI_File_write_all_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_107 MPI_File_write_all_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_107 MPI_File_write_all_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_107 MPI_File_write_all_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_all_begin

#------------------------------------------------------------------------

output_108() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_all_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_108 MPI_File_write_all_end ${rank} CH "character${dim}"
  output_108 MPI_File_write_all_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_108 MPI_File_write_all_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_108 MPI_File_write_all_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_108 MPI_File_write_all_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_all_end

#------------------------------------------------------------------------

output_109() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_at medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_109 MPI_File_write_at ${rank} CH "character${dim}"
  output_109 MPI_File_write_at ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_109 MPI_File_write_at ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_109 MPI_File_write_at ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_109 MPI_File_write_at ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_at

#------------------------------------------------------------------------

output_110() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype, &
        status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_at_all medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_110 MPI_File_write_at_all ${rank} CH "character${dim}"
  output_110 MPI_File_write_at_all ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_110 MPI_File_write_at_all ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_110 MPI_File_write_at_all ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_110 MPI_File_write_at_all ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_at_all

#------------------------------------------------------------------------

output_111() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, offset, buf, count, datatype&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  integer(kind=MPI_OFFSET_KIND), intent(in) :: offset
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_at_all_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_111 MPI_File_wr_at_all_begin ${rank} CH "character${dim}"
  output_111 MPI_File_wr_at_all_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_111 MPI_File_wr_at_all_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_111 MPI_File_wr_at_all_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_111 MPI_File_wr_at_all_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_at_all_begin

#------------------------------------------------------------------------

output_112() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_at_all_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_112 MPI_File_write_at_all_end ${rank} CH "character${dim}"
  output_112 MPI_File_write_at_all_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_112 MPI_File_write_at_all_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_112 MPI_File_write_at_all_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_112 MPI_File_write_at_all_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_at_all_end

#------------------------------------------------------------------------

output_113() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_ordered medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_113 MPI_File_write_ordered ${rank} CH "character${dim}"
  output_113 MPI_File_write_ordered ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_113 MPI_File_write_ordered ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_113 MPI_File_write_ordered ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_113 MPI_File_write_ordered ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_ordered

#------------------------------------------------------------------------

output_114() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, ierr)
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_ordered_begin medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_114 MPI_File_write_ordered_begin ${rank} CH "character${dim}"
  output_114 MPI_File_write_ordered_begin ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_114 MPI_File_write_ordered_begin ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_114 MPI_File_write_ordered_begin ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_114 MPI_File_write_ordered_begin ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_ordered_begin

#------------------------------------------------------------------------

output_115() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_ordered_end medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_115 MPI_File_write_ordered_end ${rank} CH "character${dim}"
  output_115 MPI_File_write_ordered_end ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_115 MPI_File_write_ordered_end ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_115 MPI_File_write_ordered_end ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_115 MPI_File_write_ordered_end ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_ordered_end

#------------------------------------------------------------------------

output_116() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(fh, buf, count, datatype, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: fh
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_File_write_shared medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_116 MPI_File_write_shared ${rank} CH "character${dim}"
  output_116 MPI_File_write_shared ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_116 MPI_File_write_shared ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_116 MPI_File_write_shared ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_116 MPI_File_write_shared ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_File_write_shared

#------------------------------------------------------------------------

output_117() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ierr)
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Finalize small
output_117 MPI_Finalize
end MPI_Finalize

#------------------------------------------------------------------------

output_118() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Finalized small
output_118 MPI_Finalized
end MPI_Finalized

#------------------------------------------------------------------------

output_119() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(base, ierr)
  ${type}, intent(in) :: base
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Free_mem medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_119 MPI_Free_mem ${rank} CH "character${dim}"
  output_119 MPI_Free_mem ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_119 MPI_Free_mem ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_119 MPI_Free_mem ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_119 MPI_Free_mem ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Free_mem

#------------------------------------------------------------------------

output_120() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Gather large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_120 MPI_Gather ${rank} CH "character${dim}"
  output_120 MPI_Gather ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_120 MPI_Gather ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_120 MPI_Gather ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_120 MPI_Gather ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Gather

#------------------------------------------------------------------------

output_121() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcounts, &
        displs, recvtype, root, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, dimension(*), intent(in) :: recvcounts
  integer, dimension(*), intent(in) :: displs
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Gatherv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_121 MPI_Gatherv ${rank} CH "character${dim}"
  output_121 MPI_Gatherv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_121 MPI_Gatherv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_121 MPI_Gatherv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_121 MPI_Gatherv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Gatherv

#------------------------------------------------------------------------

output_122() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)
  include 'mpif-config.h'
  ${type}, intent(in) :: origin_addr
  integer, intent(in) :: origin_count
  integer, intent(in) :: origin_datatype
  integer, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
  integer, intent(in) :: target_count
  integer, intent(in) :: target_datatype
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Get medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_122 MPI_Get ${rank} CH "character${dim}"
  output_122 MPI_Get ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_122 MPI_Get ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_122 MPI_Get ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_122 MPI_Get ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Get

#------------------------------------------------------------------------

output_123() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(location, address, ierr)
  include 'mpif-config.h'
  ${type}, intent(in) :: location
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: address
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Get_address medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_123 MPI_Get_address ${rank} CH "character${dim}"
  output_123 MPI_Get_address ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_123 MPI_Get_address ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_123 MPI_Get_address ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_123 MPI_Get_address ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Get_address

#------------------------------------------------------------------------

output_124() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_count small
output_124 MPI_Get_count
end MPI_Get_count

#------------------------------------------------------------------------

output_125() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  integer, intent(in) :: datatype
  integer, intent(out) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_elements small
output_125 MPI_Get_elements
end MPI_Get_elements

#------------------------------------------------------------------------

output_126() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(name, resultlen, ierr)
  character(len=*), intent(out) :: name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_processor_name small
output_126 MPI_Get_processor_name
end MPI_Get_processor_name

#------------------------------------------------------------------------

output_127() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(version, subversion, ierr)
  integer, intent(out) :: version
  integer, intent(out) :: subversion
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Get_version small
output_127 MPI_Get_version
end MPI_Get_version

#------------------------------------------------------------------------

output_128() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm_old, nnodes, index, edges, reorder, &
        comm_graph, ierr)
  integer, intent(in) :: comm_old
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  logical, intent(in) :: reorder
  integer, intent(out) :: comm_graph
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_create small
output_128 MPI_Graph_create
end MPI_Graph_create

#------------------------------------------------------------------------

output_129() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, maxindex, maxedges, index, edges&
        , ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: maxindex
  integer, intent(in) :: maxedges
  integer, dimension(*), intent(out) :: index
  integer, dimension(*), intent(out) :: edges
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_get small
output_129 MPI_Graph_get
end MPI_Graph_get

#------------------------------------------------------------------------

output_130() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, nnodes, index, edges, newrank&
        , ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: nnodes
  integer, dimension(*), intent(in) :: index
  integer, dimension(*), intent(in) :: edges
  integer, intent(out) :: newrank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_map small
output_130 MPI_Graph_map
end MPI_Graph_map

#------------------------------------------------------------------------

output_131() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, maxneighbors, neighbors, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(in) :: maxneighbors
  integer, dimension(*), intent(out) :: neighbors
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_neighbors small
output_131 MPI_Graph_neighbors
end MPI_Graph_neighbors

#------------------------------------------------------------------------

output_132() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, rank, nneighbors, ierr)
  integer, intent(in) :: comm
  integer, intent(in) :: rank
  integer, intent(out) :: nneighbors
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graph_neighbors_count small
output_132 MPI_Graph_neighbors_count
end MPI_Graph_neighbors_count

#------------------------------------------------------------------------

output_133() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, nnodes, nedges, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: nnodes
  integer, intent(out) :: nedges
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Graphdims_get small
output_133 MPI_Graphdims_get
end MPI_Graphdims_get

#------------------------------------------------------------------------

output_134() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  integer, intent(inout) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Grequest_complete small
output_134 MPI_Grequest_complete
end MPI_Grequest_complete

#------------------------------------------------------------------------

output_135() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(query_fn, free_fn, cancel_fn, extra_state, request&
        , ierr)
  include 'mpif-config.h'
  external :: query_fn
  external :: free_fn
  external :: cancel_fn
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Grequest_start small
output_135 MPI_Grequest_start
end MPI_Grequest_start

#------------------------------------------------------------------------

output_136() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, result, ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: result
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_compare small
output_136 MPI_Group_compare
end MPI_Group_compare

#------------------------------------------------------------------------

output_137() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_difference small
output_137 MPI_Group_difference
end MPI_Group_difference

#------------------------------------------------------------------------

output_138() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranks, newgroup, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_excl small
output_138 MPI_Group_excl
end MPI_Group_excl

#------------------------------------------------------------------------

output_139() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, ierr)
  integer, intent(inout) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_free small
output_139 MPI_Group_free
end MPI_Group_free

#------------------------------------------------------------------------

output_140() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranks, newgroup, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_incl small
output_140 MPI_Group_incl
end MPI_Group_incl

#------------------------------------------------------------------------

output_141() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_intersection small
output_141 MPI_Group_intersection
end MPI_Group_intersection

#------------------------------------------------------------------------

output_142() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranges, newgroup, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_range_excl small
output_142 MPI_Group_range_excl
end MPI_Group_range_excl

#------------------------------------------------------------------------

output_143() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, n, ranges, newgroup, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: n
  integer, dimension(3, *), intent(in) :: ranges
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_range_incl small
output_143 MPI_Group_range_incl
end MPI_Group_range_incl

#------------------------------------------------------------------------

output_144() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, rank, ierr)
  integer, intent(in) :: group
  integer, intent(out) :: rank
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_rank small
output_144 MPI_Group_rank
end MPI_Group_rank

#------------------------------------------------------------------------

output_145() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, size, ierr)
  integer, intent(in) :: group
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_size small
output_145 MPI_Group_size
end MPI_Group_size

#------------------------------------------------------------------------

output_146() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, n, ranks1, group2, ranks2&
        , ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: n
  integer, dimension(*), intent(in) :: ranks1
  integer, intent(in) :: group2
  integer, dimension(*), intent(out) :: ranks2
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_translate_ranks small
output_146 MPI_Group_translate_ranks
end MPI_Group_translate_ranks

#------------------------------------------------------------------------

output_147() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group1, group2, newgroup, ierr)
  integer, intent(in) :: group1
  integer, intent(in) :: group2
  integer, intent(out) :: newgroup
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Group_union small
output_147 MPI_Group_union
end MPI_Group_union

#------------------------------------------------------------------------

output_148() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Ibsend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_148 MPI_Ibsend ${rank} CH "character${dim}"
  output_148 MPI_Ibsend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_148 MPI_Ibsend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_148 MPI_Ibsend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_148 MPI_Ibsend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Ibsend

#------------------------------------------------------------------------

output_149() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, ierr)
  integer, intent(out) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_create small
output_149 MPI_Info_create
end MPI_Info_create

#------------------------------------------------------------------------

output_150() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, ierr)
  integer, intent(out) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_delete small
output_150 MPI_Info_delete
end MPI_Info_delete

#------------------------------------------------------------------------

output_151() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, newinfo, ierr)
  integer, intent(in) :: info
  integer, intent(out) :: newinfo
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_dup small
output_151 MPI_Info_dup
end MPI_Info_dup

#------------------------------------------------------------------------

output_152() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, ierr)
  integer, intent(inout) :: info
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_free small
output_152 MPI_Info_free
end MPI_Info_free

#------------------------------------------------------------------------

output_153() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, valuelen, value, flag&
        , ierr)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(in) :: valuelen
  character(len=*), intent(out) :: value
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get small
output_153 MPI_Info_get
end MPI_Info_get

#------------------------------------------------------------------------

output_154() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, nkeys, ierr)
  integer, intent(in) :: info
  integer, intent(out) :: nkeys
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_nkeys small
output_154 MPI_Info_get_nkeys
end MPI_Info_get_nkeys

#------------------------------------------------------------------------

output_155() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, n, key, ierr)
  integer, intent(in) :: info
  integer, intent(in) :: n
  character(len=*), intent(out) :: key
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_nthkey small
output_155 MPI_Info_get_nthkey
end MPI_Info_get_nthkey

#------------------------------------------------------------------------

output_156() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, valuelen, flag, ierr)
  integer, intent(in) :: info
  character(len=*), intent(in) :: key
  integer, intent(out) :: valuelen
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_get_valuelen small
output_156 MPI_Info_get_valuelen
end MPI_Info_get_valuelen

#------------------------------------------------------------------------

output_157() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, key, value, ierr)
  integer, intent(inout) :: info
  character(len=*), intent(in) :: key
  character(len=*), intent(in) :: value
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Info_set small
output_157 MPI_Info_set
end MPI_Info_set

#------------------------------------------------------------------------

output_158() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ierr)
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Init small
output_158 MPI_Init
end MPI_Init

#------------------------------------------------------------------------

output_159() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(required, provided, ierr)
  integer, intent(in) :: required
  integer, intent(out) :: provided
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Init_thread small
output_159 MPI_Init_thread
end MPI_Init_thread

#------------------------------------------------------------------------

output_160() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Initialized small
output_160 MPI_Initialized
end MPI_Initialized

#------------------------------------------------------------------------

output_161() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(local_comm, local_leader, bridge_comm, remote_leader, tag, &
        newintercomm, ierr)
  integer, intent(in) :: local_comm
  integer, intent(in) :: local_leader
  integer, intent(in) :: bridge_comm
  integer, intent(in) :: remote_leader
  integer, intent(in) :: tag
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Intercomm_create small
output_161 MPI_Intercomm_create
end MPI_Intercomm_create

#------------------------------------------------------------------------

output_162() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(intercomm, high, newintercomm, ierr)
  integer, intent(in) :: intercomm
  logical, intent(in) :: high
  integer, intent(out) :: newintercomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Intercomm_merge small
output_162 MPI_Intercomm_merge
end MPI_Intercomm_merge

#------------------------------------------------------------------------

output_163() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(source, tag, comm, flag, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Iprobe small
output_163 MPI_Iprobe
end MPI_Iprobe

#------------------------------------------------------------------------

output_164() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Irecv medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_164 MPI_Irecv ${rank} CH "character${dim}"
  output_164 MPI_Irecv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_164 MPI_Irecv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_164 MPI_Irecv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_164 MPI_Irecv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Irecv

#------------------------------------------------------------------------

output_165() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Irsend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_165 MPI_Irsend ${rank} CH "character${dim}"
  output_165 MPI_Irsend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_165 MPI_Irsend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_165 MPI_Irsend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_165 MPI_Irsend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Irsend

#------------------------------------------------------------------------

output_166() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(flag, ierr)
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Is_thread_main small
output_166 MPI_Is_thread_main
end MPI_Is_thread_main

#------------------------------------------------------------------------

output_167() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Isend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_167 MPI_Isend ${rank} CH "character${dim}"
  output_167 MPI_Isend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_167 MPI_Isend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_167 MPI_Isend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_167 MPI_Isend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Isend

#------------------------------------------------------------------------

output_168() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Issend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_168 MPI_Issend ${rank} CH "character${dim}"
  output_168 MPI_Issend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_168 MPI_Issend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_168 MPI_Issend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_168 MPI_Issend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Issend

#------------------------------------------------------------------------

output_169() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(copy_fn, delete_fn, keyval, extra_state, ierr)
  external :: copy_fn
  external :: delete_fn
  integer, intent(out) :: keyval
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Keyval_create small
output_169 MPI_Keyval_create
end MPI_Keyval_create

#------------------------------------------------------------------------

output_170() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(keyval, ierr)
  integer, intent(inout) :: keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Keyval_free small
output_170 MPI_Keyval_free
end MPI_Keyval_free

#------------------------------------------------------------------------

output_171_commutative() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(op, commute, ierr)
  integer, intent(in) :: op
  logical, intent(out) :: commute
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Op_commutative small
output_171_commutative MPI_Op_commutative
end MPI_Op_commutative

#------------------------------------------------------------------------

output_171() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, commute, op, ierr)
  external :: function
  logical, intent(in) :: commute
  integer, intent(out) :: op
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Op_create small
output_171 MPI_Op_create
end MPI_Op_create

#------------------------------------------------------------------------

output_172() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(op, ierr)
  integer, intent(inout) :: op
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Op_free small
output_172 MPI_Op_free
end MPI_Op_free

#------------------------------------------------------------------------

output_173() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(inbuf, incount, datatype, outbuf, outsize, &
        position, comm, ierr)
  ${type}, intent(in) :: inbuf
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  ${type}, intent(out) :: outbuf
  integer, intent(out) :: outsize
  integer, intent(inout) :: position
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Pack large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_173 MPI_Pack ${rank} CH "character${dim}"
  output_173 MPI_Pack ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_173 MPI_Pack ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_173 MPI_Pack ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_173 MPI_Pack ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Pack

#------------------------------------------------------------------------

output_174() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(datarep, inbuf, incount, datatype, outbuf, &
        outsize, position, ierr)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  ${type}, intent(in) :: inbuf
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  ${type}, intent(out) :: outbuf
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: outsize
  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Pack_external large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_174 MPI_Pack_external ${rank} CH "character${dim}"
  output_174 MPI_Pack_external ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_174 MPI_Pack_external ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_174 MPI_Pack_external ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_174 MPI_Pack_external ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Pack_external

#------------------------------------------------------------------------

output_175() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datarep, incount, datatype, size, ierr)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Pack_external_size small
output_175 MPI_Pack_external_size
end MPI_Pack_external_size

#------------------------------------------------------------------------

output_176() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(incount, datatype, comm, size, ierr)
  integer, intent(in) :: incount
  integer, intent(in) :: datatype
  integer, intent(in) :: comm
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Pack_size small
output_176 MPI_Pack_size
end MPI_Pack_size

#------------------------------------------------------------------------

output_177() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(level)
  integer, intent(in) :: level

end subroutine ${procedure}

EOF
}

start MPI_Pcontrol small
output_177 MPI_Pcontrol
end MPI_Pcontrol

#------------------------------------------------------------------------

output_178() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(source, tag, comm, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Probe small
output_178 MPI_Probe
end MPI_Probe

#------------------------------------------------------------------------

output_179() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
        target_count, target_datatype, win, ierr)
  include 'mpif-config.h'
  ${type}, intent(in) :: origin_addr
  integer, intent(in) :: origin_count
  integer, intent(in) :: origin_datatype
  integer, intent(in) :: target_rank
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: target_disp
  integer, intent(in) :: target_count
  integer, intent(in) :: target_datatype
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Put medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_179 MPI_Put ${rank} CH "character${dim}"
  output_179 MPI_Put ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_179 MPI_Put ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_179 MPI_Put ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_179 MPI_Put ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Put

#------------------------------------------------------------------------

output_180() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(provided, ierr)
  integer, intent(out) :: provided
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Query_thread small
output_180 MPI_Query_thread
end MPI_Query_thread

#------------------------------------------------------------------------

output_181() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, status, ierr)
  include 'mpif-config.h'
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Recv medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_181 MPI_Recv ${rank} CH "character${dim}"
  output_181 MPI_Recv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_181 MPI_Recv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_181 MPI_Recv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_181 MPI_Recv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Recv

#------------------------------------------------------------------------

output_182() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, source, tag, &
        comm, request, ierr)
  ${type}, intent(out) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: source
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Recv_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_182 MPI_Recv_init ${rank} CH "character${dim}"
  output_182 MPI_Recv_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_182 MPI_Recv_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_182 MPI_Recv_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_182 MPI_Recv_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Recv_init

#------------------------------------------------------------------------

output_183() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        root, comm, ierr)
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Reduce large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_183 MPI_Reduce ${rank} CH "character${dim}"
  output_183 MPI_Reduce ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_183 MPI_Reduce ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_183 MPI_Reduce ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_183 MPI_Reduce ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Reduce

#------------------------------------------------------------------------

output_183_local() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(inbuf, inout, count, datatype, op, &
        ierr)
  ${type}, intent(in) :: inbuf
  ${type}, intent(out) :: inout
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Reduce_local large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_183_local MPI_Reduce_local ${rank} CH "character${dim}"
  output_183_local MPI_Reduce_local ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_183_local MPI_Reduce_local ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_183_local MPI_Reduce_local ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_183_local MPI_Reduce_local ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Reduce_local

#------------------------------------------------------------------------

output_184() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, recvcounts, datatype, op, &
        comm, ierr)
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcounts
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Reduce_scatter large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_184 MPI_Reduce_scatter ${rank} CH "character${dim}"
  output_184 MPI_Reduce_scatter ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_184 MPI_Reduce_scatter ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_184 MPI_Reduce_scatter ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_184 MPI_Reduce_scatter ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Reduce_scatter

#------------------------------------------------------------------------

output_185() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state&
        , ierr)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  external :: read_conversion_fn
  external :: write_conversion_fn
  external :: dtype_file_extent_fn
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Register_datarep small
output_185 MPI_Register_datarep
end MPI_Register_datarep

#------------------------------------------------------------------------

output_186() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  integer, intent(inout) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Request_free small
output_186 MPI_Request_free
end MPI_Request_free

#------------------------------------------------------------------------

output_187() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, flag, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: request
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Request_get_status small
output_187 MPI_Request_get_status
end MPI_Request_get_status

#------------------------------------------------------------------------

output_188() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(ibuf, count, datatype, dest, tag, &
        comm, ierr)
  ${type}, intent(in) :: ibuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Rsend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_188 MPI_Rsend ${rank} CH "character${dim}"
  output_188 MPI_Rsend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_188 MPI_Rsend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_188 MPI_Rsend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_188 MPI_Rsend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Rsend

#------------------------------------------------------------------------

output_189() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Rsend_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_189 MPI_Rsend_init ${rank} CH "character${dim}"
  output_189 MPI_Rsend_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_189 MPI_Rsend_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_189 MPI_Rsend_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_189 MPI_Rsend_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Rsend_init

#------------------------------------------------------------------------

output_190() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, recvbuf, count, datatype, op, &
        comm, ierr)
  ${type}, intent(in) :: sendbuf
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: op
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Scan large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_190 MPI_Scan ${rank} CH "character${dim}"
  output_190 MPI_Scan ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_190 MPI_Scan ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_190 MPI_Scan ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_190 MPI_Scan ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Scan

#------------------------------------------------------------------------

output_191() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, recvbuf, recvcount, &
        recvtype, root, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Scatter large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_191 MPI_Scatter ${rank} CH "character${dim}"
  output_191 MPI_Scatter ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_191 MPI_Scatter ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_191 MPI_Scatter ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_191 MPI_Scatter ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Scatter

#------------------------------------------------------------------------

output_192() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

! Because we can't break ABI in the middle of the 1.4 series, also
! provide the old/bad/incorrect MPI_Scatterv binding
subroutine ${proc}(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcounts
  integer, intent(in) :: displs
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

subroutine ${proc}_correct(sendbuf, sendcounts, displs, sendtype, recvbuf, &
        recvcount, recvtype, root, comm, ierr)
  ${type}, intent(in) :: sendbuf
  integer, dimension(*), intent(in) :: sendcounts
  integer, dimension(*), intent(in) :: displs
  integer, intent(in) :: sendtype
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}_correct

EOF
}

start MPI_Scatterv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_192 MPI_Scatterv ${rank} CH "character${dim}"
  output_192 MPI_Scatterv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_192 MPI_Scatterv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_192 MPI_Scatterv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_192 MPI_Scatterv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Scatterv

#------------------------------------------------------------------------

output_193() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Send medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_193 MPI_Send ${rank} CH "character${dim}"
  output_193 MPI_Send ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_193 MPI_Send ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_193 MPI_Send ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_193 MPI_Send ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Send

#------------------------------------------------------------------------

output_194() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Send_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_194 MPI_Send_init ${rank} CH "character${dim}"
  output_194 MPI_Send_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_194 MPI_Send_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_194 MPI_Send_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_194 MPI_Send_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Send_init

#------------------------------------------------------------------------

output_195() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(sendbuf, sendcount, sendtype, dest, sendtag, &
        recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr)
  include 'mpif-config.h'
  ${type}, intent(in) :: sendbuf
  integer, intent(in) :: sendcount
  integer, intent(in) :: sendtype
  integer, intent(in) :: dest
  integer, intent(in) :: sendtag
  ${type}, intent(out) :: recvbuf
  integer, intent(in) :: recvcount
  integer, intent(in) :: recvtype
  integer, intent(in) :: source
  integer, intent(in) :: recvtag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Sendrecv large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_195 MPI_Sendrecv ${rank} CH "character${dim}"
  output_195 MPI_Sendrecv ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_195 MPI_Sendrecv ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_195 MPI_Sendrecv ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_195 MPI_Sendrecv ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Sendrecv

#------------------------------------------------------------------------

output_196() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, sendtag, &
        source, recvtag, comm, status, ierr)
  include 'mpif-config.h'
  ${type}, intent(inout) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: sendtag
  integer, intent(in) :: source
  integer, intent(in) :: recvtag
  integer, intent(in) :: comm
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Sendrecv_replace medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_196 MPI_Sendrecv_replace ${rank} CH "character${dim}"
  output_196 MPI_Sendrecv_replace ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_196 MPI_Sendrecv_replace ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_196 MPI_Sendrecv_replace ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_196 MPI_Sendrecv_replace ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Sendrecv_replace

#------------------------------------------------------------------------

output_197() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(x, size, ierr)
  ${type}, intent(in) :: x
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Sizeof trivial

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_197 MPI_Sizeof ${rank} CH "character${dim}"
  output_197 MPI_Sizeof ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_197 MPI_Sizeof ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_197 MPI_Sizeof ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_197 MPI_Sizeof ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Sizeof

#------------------------------------------------------------------------

output_198() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Ssend medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_198 MPI_Ssend ${rank} CH "character${dim}"
  output_198 MPI_Ssend ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_198 MPI_Ssend ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_198 MPI_Ssend ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_198 MPI_Ssend ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Ssend

#------------------------------------------------------------------------

output_199() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(buf, count, datatype, dest, tag, &
        comm, request, ierr)
  ${type}, intent(in) :: buf
  integer, intent(in) :: count
  integer, intent(in) :: datatype
  integer, intent(in) :: dest
  integer, intent(in) :: tag
  integer, intent(in) :: comm
  integer, intent(out) :: request
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Ssend_init medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_199 MPI_Ssend_init ${rank} CH "character${dim}"
  output_199 MPI_Ssend_init ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_199 MPI_Ssend_init ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_199 MPI_Ssend_init ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_199 MPI_Ssend_init ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Ssend_init

#------------------------------------------------------------------------

output_200() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, ierr)
  integer, intent(inout) :: request
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Start small
output_200 MPI_Start
end MPI_Start

#------------------------------------------------------------------------

output_201() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, ierr)
  integer, intent(in) :: count
  integer, dimension(*), intent(inout) :: array_of_requests
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Startall small
output_201 MPI_Startall
end MPI_Startall

#------------------------------------------------------------------------

output_202() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, flag, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  logical, intent(in) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Status_set_cancelled small
output_202 MPI_Status_set_cancelled
end MPI_Status_set_cancelled

#------------------------------------------------------------------------

output_203() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, datatype, count, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
  integer, intent(in) :: datatype
  integer, intent(in) :: count
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Status_set_elements small
output_203 MPI_Status_set_elements
end MPI_Status_set_elements

#------------------------------------------------------------------------

output_204() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, flag, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: request
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Test small
output_204 MPI_Test
end MPI_Test

#------------------------------------------------------------------------

output_205() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(status, flag, ierr)
  include 'mpif-config.h'
  integer, dimension(MPI_STATUS_SIZE), intent(in) :: status
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Test_cancelled small
output_205 MPI_Test_cancelled
end MPI_Test_cancelled

#------------------------------------------------------------------------

output_206() {
    if test "$output" = "0"; then
        return 0
    fi

    suffix=$1
    status_type=$2
    cat <<EOF

subroutine MPI_Testall${suffix}(count, array_of_requests, flag, array_of_statuses, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  logical, intent(out) :: flag
  $status_type, intent(out) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine MPI_Testall${suffix}

EOF
}

start MPI_Testall small
output_206 S "integer, dimension(MPI_STATUS_SIZE, count)"
output_206 I "double precision"
end MPI_Testall

#------------------------------------------------------------------------

output_207() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, index, flag, status&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  logical, intent(out) :: flag
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Testany small
output_207 MPI_Testany
end MPI_Testany

#------------------------------------------------------------------------

output_208() {
    if test "$output" = "0"; then
        return 0
    fi

    suffix=$1
    status_type=$2
    cat <<EOF

subroutine MPI_Testsome${suffix}(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  $status_type, intent(out) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine MPI_Testsome${suffix}

EOF
}

start MPI_Testsome small
output_208 S "integer, dimension(MPI_STATUS_SIZE, incount)"
output_208 I "double precision"
end MPI_Testsome

#------------------------------------------------------------------------

output_209() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, status, ierr)
  integer, intent(in) :: comm
  integer, intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Topo_test small
output_209 MPI_Topo_test
end MPI_Topo_test

#------------------------------------------------------------------------

output_210() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, ierr)
  integer, intent(inout) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_commit small
output_210 MPI_Type_commit
end MPI_Type_commit

#------------------------------------------------------------------------

output_211() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, oldtype, newtype, ierr)
  integer, intent(in) :: count
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_contiguous small
output_211 MPI_Type_contiguous
end MPI_Type_contiguous

#------------------------------------------------------------------------

output_212() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(size, rank, ndims, gsize_array, distrib_array, &
        darg_array, psize_array, order, oldtype, newtype, ierr)
  integer, intent(in) :: size
  integer, intent(in) :: rank
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: gsize_array
  integer, dimension(*), intent(in) :: distrib_array
  integer, dimension(*), intent(in) :: darg_array
  integer, dimension(*), intent(in) :: psize_array
  integer, intent(in) :: order
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_darray small
output_212 MPI_Type_create_darray
end MPI_Type_create_darray

#------------------------------------------------------------------------

output_213() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(p, r, newtype, ierr)
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_complex small
output_213 MPI_Type_create_f90_complex
end MPI_Type_create_f90_complex

#------------------------------------------------------------------------

output_214() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(r, newtype, ierr)
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_integer small
output_214 MPI_Type_create_f90_integer
end MPI_Type_create_f90_integer

#------------------------------------------------------------------------

output_215() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(p, r, newtype, ierr)
  integer, intent(in) :: p
  integer, intent(in) :: r
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_f90_real small
output_215 MPI_Type_create_f90_real
end MPI_Type_create_f90_real

#------------------------------------------------------------------------

output_216() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_hindexed small
output_216 MPI_Type_create_hindexed
end MPI_Type_create_hindexed

#------------------------------------------------------------------------

output_217() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_hvector small
output_217 MPI_Type_create_hvector
end MPI_Type_create_hvector

#------------------------------------------------------------------------

output_218() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, array_of_displacements, oldtype, newtype&
        , ierr)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_indexed_block small
output_218 MPI_Type_create_indexed_block
end MPI_Type_create_indexed_block

#------------------------------------------------------------------------

output_219() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierr)
  include 'mpif-config.h'
  external :: type_copy_attr_fn
  external :: type_delete_attr_fn
  integer, intent(out) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_keyval small
output_219 MPI_Type_create_keyval
end MPI_Type_create_keyval

#------------------------------------------------------------------------

output_220() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(oldtype, lb, extent, newtype, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: oldtype
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extent
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_resized small
output_220 MPI_Type_create_resized
end MPI_Type_create_resized

#------------------------------------------------------------------------

output_221() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_block_lengths, array_of_displacements, array_of_types, newtype&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_block_lengths
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(in) :: array_of_displacements
  integer, dimension(*), intent(in) :: array_of_types
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_struct small
output_221 MPI_Type_create_struct
end MPI_Type_create_struct

#------------------------------------------------------------------------

output_222() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(ndims, size_array, subsize_array, start_array, order, &
        oldtype, newtype, ierr)
  integer, intent(in) :: ndims
  integer, dimension(*), intent(in) :: size_array
  integer, dimension(*), intent(in) :: subsize_array
  integer, dimension(*), intent(in) :: start_array
  integer, intent(in) :: order
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_create_subarray small
output_222 MPI_Type_create_subarray
end MPI_Type_create_subarray

#------------------------------------------------------------------------

output_223() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, ierr)
  integer, intent(inout) :: type
  integer, intent(in) :: type_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_delete_attr small
output_223 MPI_Type_delete_attr
end MPI_Type_delete_attr

#------------------------------------------------------------------------

output_224() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, newtype, ierr)
  integer, intent(in) :: type
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_dup small
output_224 MPI_Type_dup
end MPI_Type_dup

#------------------------------------------------------------------------

output_225() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, extent, ierr)
  integer, intent(in) :: type
  integer, intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_extent small
output_225 MPI_Type_extent
end MPI_Type_extent

#------------------------------------------------------------------------

output_226() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, ierr)
  integer, intent(inout) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_free small
output_226 MPI_Type_free
end MPI_Type_free

#------------------------------------------------------------------------

output_227() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type_keyval, ierr)
  integer, intent(inout) :: type_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_free_keyval small
output_227 MPI_Type_free_keyval
end MPI_Type_free_keyval

#------------------------------------------------------------------------

output_228() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, attribute_val, flag, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_attr small
output_228 MPI_Type_get_attr
end MPI_Type_get_attr

#------------------------------------------------------------------------

output_229() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(mtype, max_integers, max_addresses, max_datatypes, array_of_integers, &
        array_of_addresses, array_of_datatypes, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: mtype
  integer, intent(in) :: max_integers
  integer, intent(in) :: max_addresses
  integer, intent(in) :: max_datatypes
  integer, dimension(*), intent(out) :: array_of_integers
  integer(kind=MPI_ADDRESS_KIND), dimension(*), intent(out) :: array_of_addresses
  integer, dimension(*), intent(out) :: array_of_datatypes
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_contents small
output_229 MPI_Type_get_contents
end MPI_Type_get_contents

#------------------------------------------------------------------------

output_230() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, num_integers, num_addresses, num_datatypes, combiner&
        , ierr)
  integer, intent(in) :: type
  integer, intent(out) :: num_integers
  integer, intent(out) :: num_addresses
  integer, intent(out) :: num_datatypes
  integer, intent(out) :: combiner
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_envelope small
output_230 MPI_Type_get_envelope
end MPI_Type_get_envelope

#------------------------------------------------------------------------

output_231() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, lb, extent, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_extent small
output_231 MPI_Type_get_extent
end MPI_Type_get_extent

#------------------------------------------------------------------------

output_232() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_name, resultlen, ierr)
  integer, intent(in) :: type
  character(len=*), intent(out) :: type_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_name small
output_232 MPI_Type_get_name
end MPI_Type_get_name

#------------------------------------------------------------------------

output_233() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(datatype, true_lb, true_extent, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: datatype
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_lb
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: true_extent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_get_true_extent small
output_233 MPI_Type_get_true_extent
end MPI_Type_get_true_extent

#------------------------------------------------------------------------

output_234() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_hindexed small
output_234 MPI_Type_hindexed
end MPI_Type_hindexed

#------------------------------------------------------------------------

output_235() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_hvector small
output_235 MPI_Type_hvector
end MPI_Type_hvector

#------------------------------------------------------------------------

output_236() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierr)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_indexed small
output_236 MPI_Type_indexed
end MPI_Type_indexed

#------------------------------------------------------------------------

output_237() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, lb, ierr)
  integer, intent(in) :: type
  integer, intent(out) :: lb
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_lb small
output_237 MPI_Type_lb
end MPI_Type_lb

#------------------------------------------------------------------------

output_238() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(typeclass, size, type, ierr)
  integer, intent(in) :: typeclass
  integer, intent(in) :: size
  integer, intent(out) :: type
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_match_size small
output_238 MPI_Type_match_size
end MPI_Type_match_size

#------------------------------------------------------------------------

output_239() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_keyval, attr_val, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: type
  integer, intent(in) :: type_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attr_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_set_attr small
output_239 MPI_Type_set_attr
end MPI_Type_set_attr

#------------------------------------------------------------------------

output_240() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, type_name, ierr)
  integer, intent(inout) :: type
  character(len=*), intent(in) :: type_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_set_name small
output_240 MPI_Type_set_name
end MPI_Type_set_name

#------------------------------------------------------------------------

output_241() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(type, size, ierr)
  integer, intent(in) :: type
  integer, intent(out) :: size
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_size small
output_241 MPI_Type_size
end MPI_Type_size

#------------------------------------------------------------------------

output_242() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype&
        , ierr)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, dimension(*), intent(in) :: array_of_types
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_struct small
output_242 MPI_Type_struct
end MPI_Type_struct

#------------------------------------------------------------------------

output_243() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(mtype, ub, ierr)
  integer, intent(in) :: mtype
  integer, intent(out) :: ub
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_ub small
output_243 MPI_Type_ub
end MPI_Type_ub

#------------------------------------------------------------------------

output_244() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, blocklength, stride, oldtype, newtype&
        , ierr)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Type_vector small
output_244 MPI_Type_vector
end MPI_Type_vector

#------------------------------------------------------------------------

output_245() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(inbuf, insize, position, outbuf, outcount, &
        datatype, comm, ierr)
  ${type}, intent(in) :: inbuf
  integer, intent(in) :: insize
  integer, intent(inout) :: position
  ${type}, intent(out) :: outbuf
  integer, intent(in) :: outcount
  integer, intent(in) :: datatype
  integer, intent(in) :: comm
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Unpack large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_245 MPI_Unpack ${rank} CH "character${dim}"
  output_245 MPI_Unpack ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_245 MPI_Unpack ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_245 MPI_Unpack ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_245 MPI_Unpack ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Unpack

#------------------------------------------------------------------------

output_246() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(datarep, inbuf, insize, position, outbuf, &
        outcount, datatype, ierr)
  include 'mpif-config.h'
  character(len=*), intent(in) :: datarep
  ${type}, intent(in) :: inbuf
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: insize
  integer(kind=MPI_ADDRESS_KIND), intent(inout) :: position
  ${type}, intent(out) :: outbuf
  integer, intent(in) :: outcount
  integer, intent(in) :: datatype
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Unpack_external large

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_246 MPI_Unpack_external ${rank} CH "character${dim}"
  output_246 MPI_Unpack_external ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_246 MPI_Unpack_external ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_246 MPI_Unpack_external ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_246 MPI_Unpack_external ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Unpack_external

#------------------------------------------------------------------------

output_247() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(request, status, ierr)
  include 'mpif-config.h'
  integer, intent(inout) :: request
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Wait small
output_247 MPI_Wait
end MPI_Wait

#------------------------------------------------------------------------

output_248() {
    if test "$output" = "0"; then
        return 0
    fi

    suffix=$1
    status_type=$2
    cat <<EOF

subroutine MPI_Waitall${suffix}(count, array_of_requests, array_of_statuses, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  $status_type, intent(out) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine MPI_Waitall${suffix}

EOF
}

start MPI_Waitall small
output_248 S "integer, dimension(MPI_STATUS_SIZE, count)"
output_248 I "double precision"
end MPI_Waitall

#------------------------------------------------------------------------

output_249() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(count, array_of_requests, index, status, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: count
  integer, dimension(count), intent(inout) :: array_of_requests
  integer, intent(out) :: index
  integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Waitany small
output_249 MPI_Waitany
end MPI_Waitany

#------------------------------------------------------------------------

output_250() {
    if test "$output" = "0"; then
        return 0
    fi

    suffix=$1
    status_type=$2
    cat <<EOF

subroutine MPI_Waitsome${suffix}(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
        , ierr)
  include 'mpif-config.h'
  integer, intent(in) :: incount
  integer, dimension(incount), intent(inout) :: array_of_requests
  integer, intent(out) :: outcount
  integer, dimension(*), intent(out) :: array_of_indices
  $status_type, intent(out) :: array_of_statuses
  integer, intent(out) :: ierr
end subroutine MPI_Waitsome${suffix}

EOF
}

start MPI_Waitsome small
output_250 S "integer, dimension(MPI_STATUS_SIZE, incount)"
output_250 I "double precision"
end MPI_Waitsome

#------------------------------------------------------------------------

output_251() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errorcode, ierr)
  integer, intent(in) :: win
  integer, intent(in) :: errorcode
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_call_errhandler small
output_251 MPI_Win_call_errhandler
end MPI_Win_call_errhandler

#------------------------------------------------------------------------

output_252() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_complete small
output_252 MPI_Win_complete
end MPI_Win_complete

#------------------------------------------------------------------------

output_253() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    rank=$2
    type=$4
    proc="$1$2D$3"
    cat <<EOF

subroutine ${proc}(base, size, disp_unit, info, comm, &
        win, ierr)
  include 'mpif-config.h'
  ${type}, intent(in) :: base
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: size
  integer, intent(in) :: disp_unit
  integer, intent(in) :: info
  integer, intent(in) :: comm
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${proc}

EOF
}

start MPI_Win_create medium

for rank in $allranks
do
  case "$rank" in  0)  dim=''  ;  esac
  case "$rank" in  1)  dim=', dimension(*)'  ;  esac
  case "$rank" in  2)  dim=', dimension(1,*)'  ;  esac
  case "$rank" in  3)  dim=', dimension(1,1,*)'  ;  esac
  case "$rank" in  4)  dim=', dimension(1,1,1,*)'  ;  esac
  case "$rank" in  5)  dim=', dimension(1,1,1,1,*)'  ;  esac
  case "$rank" in  6)  dim=', dimension(1,1,1,1,1,*)'  ;  esac
  case "$rank" in  7)  dim=', dimension(1,1,1,1,1,1,*)'  ;  esac

  output_253 MPI_Win_create ${rank} CH "character${dim}"
  output_253 MPI_Win_create ${rank} L "logical${dim}"
  for kind in $ikinds
  do
    output_253 MPI_Win_create ${rank} I${kind} "integer*${kind}${dim}"
  done
  for kind in $rkinds
  do
    output_253 MPI_Win_create ${rank} R${kind} "real*${kind}${dim}"
  done
  for kind in $ckinds
  do
    output_253 MPI_Win_create ${rank} C${kind} "complex*${kind}${dim}"
  done
done
end MPI_Win_create

#------------------------------------------------------------------------

output_254() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(function, errhandler, ierr)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_create_errhandler small
output_254 MPI_Win_create_errhandler
end MPI_Win_create_errhandler

#------------------------------------------------------------------------

output_255() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierr)
  include 'mpif-config.h'
  external :: win_copy_attr_fn
  external :: win_delete_attr_fn
  integer, intent(out) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: extra_state
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_create_keyval small
output_255 MPI_Win_create_keyval
end MPI_Win_create_keyval

#------------------------------------------------------------------------

output_256() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, ierr)
  integer, intent(inout) :: win
  integer, intent(in) :: win_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_delete_attr small
output_256 MPI_Win_delete_attr
end MPI_Win_delete_attr

#------------------------------------------------------------------------

output_257() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(assert, win, ierr)
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_fence small
output_257 MPI_Win_fence
end MPI_Win_fence

#------------------------------------------------------------------------

output_258() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  integer, intent(inout) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_free small
output_258 MPI_Win_free
end MPI_Win_free

#------------------------------------------------------------------------

output_259() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win_keyval, ierr)
  integer, intent(inout) :: win_keyval
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_free_keyval small
output_259 MPI_Win_free_keyval
end MPI_Win_free_keyval

#------------------------------------------------------------------------

output_260() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, attribute_val, flag, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_attr small
output_260 MPI_Win_get_attr
end MPI_Win_get_attr

#------------------------------------------------------------------------

output_261() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errhandler, ierr)
  integer, intent(in) :: win
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_errhandler small
output_261 MPI_Win_get_errhandler
end MPI_Win_get_errhandler

#------------------------------------------------------------------------

output_262() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, group, ierr)
  integer, intent(in) :: win
  integer, intent(out) :: group
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_group small
output_262 MPI_Win_get_group
end MPI_Win_get_group

#------------------------------------------------------------------------

output_263() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_name, resultlen, ierr)
  integer, intent(in) :: win
  character(len=*), intent(out) :: win_name
  integer, intent(out) :: resultlen
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_get_name small
output_263 MPI_Win_get_name
end MPI_Win_get_name

#------------------------------------------------------------------------

output_264() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(lock_type, rank, assert, win, ierr)
  integer, intent(in) :: lock_type
  integer, intent(in) :: rank
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_lock small
output_264 MPI_Win_lock
end MPI_Win_lock

#------------------------------------------------------------------------

output_265() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, assert, win, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_post small
output_265 MPI_Win_post
end MPI_Win_post

#------------------------------------------------------------------------

output_266() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_keyval, attribute_val, ierr)
  include 'mpif-config.h'
  integer, intent(in) :: win
  integer, intent(in) :: win_keyval
  integer(kind=MPI_ADDRESS_KIND), intent(in) :: attribute_val
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_attr small
output_266 MPI_Win_set_attr
end MPI_Win_set_attr

#------------------------------------------------------------------------

output_267() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, errhandler, ierr)
  integer, intent(in) :: win
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_errhandler small
output_267 MPI_Win_set_errhandler
end MPI_Win_set_errhandler

#------------------------------------------------------------------------

output_268() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, win_name, ierr)
  integer, intent(inout) :: win
  character(len=*), intent(in) :: win_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_set_name small
output_268 MPI_Win_set_name
end MPI_Win_set_name

#------------------------------------------------------------------------

output_269() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(group, assert, win, ierr)
  integer, intent(in) :: group
  integer, intent(in) :: assert
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_start small
output_269 MPI_Win_start
end MPI_Win_start

#------------------------------------------------------------------------

output_270() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, flag, ierr)
  integer, intent(in) :: win
  logical, intent(out) :: flag
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_test small
output_270 MPI_Win_test
end MPI_Win_test

#------------------------------------------------------------------------

output_271() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(rank, win, ierr)
  integer, intent(in) :: rank
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_unlock small
output_271 MPI_Win_unlock
end MPI_Win_unlock

#------------------------------------------------------------------------

output_272() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(win, ierr)
  integer, intent(in) :: win
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Win_wait small
output_272 MPI_Win_wait
end MPI_Win_wait

#------------------------------------------------------------------------

output_273() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, ierr)
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Close_port small
output_273 MPI_Close_port
end MPI_Close_port

#------------------------------------------------------------------------

output_274() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Lookup_name small
output_274 MPI_Lookup_name
end MPI_Lookup_name

#------------------------------------------------------------------------

output_275() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(info, port_name, ierr)
  integer, intent(in) :: info
  character(len=*), intent(out) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Open_port small
output_275 MPI_Open_port
end MPI_Open_port

#------------------------------------------------------------------------

output_276() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Publish_name small
output_276 MPI_Publish_name
end MPI_Publish_name

#------------------------------------------------------------------------

output_277() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(service_name, info, port_name, ierr)
  character(len=*), intent(in) :: service_name
  integer, intent(in) :: info
  character(len=*), intent(in) :: port_name
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Unpublish_name small
output_277 MPI_Unpublish_name
end MPI_Unpublish_name

#------------------------------------------------------------------------

output_278() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(comm, ierr)
  integer, intent(inout) :: comm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_disconnect small
output_278 MPI_Comm_disconnect
end MPI_Comm_disconnect

#------------------------------------------------------------------------

output_279() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(parent, ierr)
  integer, intent(out) :: parent
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_get_parent small
output_279 MPI_Comm_get_parent
end MPI_Comm_get_parent

#------------------------------------------------------------------------

output_280() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(fd, intercomm, ierr)
  integer, intent(in) :: fd
  integer, intent(out) :: intercomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_join small
output_280 MPI_Comm_join
end MPI_Comm_join

#------------------------------------------------------------------------

output_281() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, info, root, comm, newcomm&
        , ierr)
  character(len=*), intent(in) :: port_name
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_accept small
output_281 MPI_Comm_accept
end MPI_Comm_accept

#------------------------------------------------------------------------

output_282() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(port_name, info, root, comm, newcomm&
        , ierr)
  character(len=*), intent(in) :: port_name
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: newcomm
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_connect small
output_282 MPI_Comm_connect
end MPI_Comm_connect

#------------------------------------------------------------------------

output_283() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    cat <<EOF

subroutine ${procedure}(command, argv, maxprocs, info, root, &
        comm, intercomm, array_of_errcodes, ierr)
  character(len=*), intent(in) :: command
  character(len=*), dimension(*), intent(in) :: argv
  integer, intent(in) :: maxprocs
  integer, intent(in) :: info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_spawn small
output_283 MPI_Comm_spawn
end MPI_Comm_spawn

#------------------------------------------------------------------------

output_284() {
    if test "$output" = "0"; then
        return 0
    fi

    procedure=$1
    argv_type=$2
    cat <<EOF

subroutine ${procedure}(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
        root, comm, intercomm, array_of_errcodes, ierr)
  integer, intent(in) :: count
  character(len=*), dimension(*), intent(in) :: array_of_commands
  $argv_type, intent(in) :: array_of_argv
  integer, dimension(*), intent(in) :: array_of_maxprocs
  integer, dimension(*), intent(in) :: array_of_info
  integer, intent(in) :: root
  integer, intent(in) :: comm
  integer, intent(out) :: intercomm
  integer, dimension(*), intent(out) :: array_of_errcodes
  integer, intent(out) :: ierr
end subroutine ${procedure}

EOF
}

start MPI_Comm_spawn_multiple small
output_284 MPI_Comm_spawn_multipleA "character(len=*), dimension(count,*)"
output_284 MPI_Comm_spawn_multipleN "double precision"
end MPI_Comm_spawn_multiple
