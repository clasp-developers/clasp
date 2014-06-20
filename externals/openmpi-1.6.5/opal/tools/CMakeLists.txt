# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

IF(OPAL_ENABLE_FT)
  ADD_SUBDIRECTORY(opal-restart)
ENDIF(OPAL_ENABLE_FT)
ADD_SUBDIRECTORY(wrappers)