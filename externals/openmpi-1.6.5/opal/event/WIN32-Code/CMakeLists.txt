# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


IF(WIN32)
  SET (EVENT_WIN32_FILES
    ${EVENT_WIN32_FILES}
    config.h
    tree.h
    win32.c
  )
ELSE(WIN32)
ENDIF(WIN32)