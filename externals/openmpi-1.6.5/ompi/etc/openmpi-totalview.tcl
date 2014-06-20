#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Check if the newly loaded image is one of the MPI starter programs
# and start it immediately if it is.

proc mpi_auto_run_starter {loaded_id} {
    set starter_programs {mpirun mpiexec orterun}
    set executable_name [TV::symbol get $loaded_id full_pathname]
    set file_component [file tail $executable_name]

    if {[lsearch -exact $starter_programs $file_component] != -1} {
        puts "**************************************"
        puts "Automatically starting $file_component"
        puts "**************************************"
        dgo
    }
}

# Append this function to TotalView's image load callbacks so that
# TotalView run this program automatically.

dlappend TV::image_load_callbacks mpi_auto_run_starter
