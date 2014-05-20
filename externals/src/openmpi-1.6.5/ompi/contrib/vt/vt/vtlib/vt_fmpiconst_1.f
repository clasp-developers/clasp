c     **
c     * VampirTrace
c     * http://www.tu-dresden.de/zih/vampirtrace
c     *
c     * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
c     *
c     * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
c     *                          Centre, Federal Republic of Germany
c     *
c     * See the file COPYING in the package base directory for details
c     **

c     MPI_BOTTOM

      SUBROUTINE vt_get_mpi_f_bottom___
      CALL vt_get_mpi_f_bottom
      END
      SUBROUTINE vt_get_mpi_f_bottom__
      CALL vt_get_mpi_f_bottom
      END
      SUBROUTINE vt_get_mpi_f_bottom_
      CALL vt_get_mpi_f_bottom
      END
      SUBROUTINE vt_get_mpi_f_bottom
      INCLUDE 'mpif.h'
      CALL vt_get_mpi_f_bottom_cb(MPI_BOTTOM)
      END

c     MPI_STATUS[ES]_IGNORE

      SUBROUTINE vt_get_mpi_f_statuses_ignore___
      CALL vt_get_mpi_f_statuses_ignore
      END
      SUBROUTINE vt_get_mpi_f_statuses_ignore__
      CALL vt_get_mpi_f_statuses_ignore
      END
      SUBROUTINE vt_get_mpi_f_statuses_ignore_
      CALL vt_get_mpi_f_statuses_ignore
      END
      SUBROUTINE vt_get_mpi_f_statuses_ignore
      INCLUDE 'mpif.h'
      CALL vt_get_mpi_f_statuses_ignore_cb(MPI_STATUS_IGNORE,
     +MPI_STATUSES_IGNORE)
      END

c     MPI_STATUS_SIZE

      SUBROUTINE vt_get_mpi_status_size___ (f_mpi_status_size)
      INTEGER f_mpi_status_size
      CALL vt_get_mpi_status_size (f_mpi_status_size)
      END
      SUBROUTINE vt_get_mpi_status_size__ (f_mpi_status_size)
      INTEGER f_mpi_status_size
      CALL vt_get_mpi_status_size (f_mpi_status_size)
      END
      SUBROUTINE vt_get_mpi_status_size_ (f_mpi_status_size)
      INTEGER f_mpi_status_size
      CALL vt_get_mpi_status_size (f_mpi_status_size)
      END
      SUBROUTINE vt_get_mpi_status_size (f_mpi_status_size)
      INCLUDE 'mpif.h'
      INTEGER f_mpi_status_size
      f_mpi_status_size = MPI_STATUS_SIZE
      END
