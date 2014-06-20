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

c     MPI_IN_PLACE

      SUBROUTINE vt_get_mpi_f_in_place___
      CALL vt_get_mpi_f_in_place
      END
      SUBROUTINE vt_get_mpi_f_in_place__
      CALL vt_get_mpi_f_in_place
      END
      SUBROUTINE vt_get_mpi_f_in_place_
      CALL vt_get_mpi_f_in_place
      END
      SUBROUTINE vt_get_mpi_f_in_place
      INCLUDE 'mpif.h'
      CALL vt_get_mpi_f_in_place_cb(MPI_IN_PLACE)
      END
