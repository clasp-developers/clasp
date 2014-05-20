# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#



IF(WIN32)
  SET(COMPAT_SYS_FILES
    ${COMPAT_SYS_FILES}
    queue.h
    _timeradd.h  
  )
ELSE(WIN32)
ENDIF(WIN32)