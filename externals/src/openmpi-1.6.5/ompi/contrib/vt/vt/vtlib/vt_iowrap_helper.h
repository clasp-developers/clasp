/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef VT_IOWRAP_HELPER_H_
#define VT_IOWRAP_HELPER_H_

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

#include "config.h"

#include "vt_inttypes.h"

typedef struct {
        uint32_t vampir_file_id;
        uint32_t vampir_file_group_id;
        uint64_t matchingid;
        uint64_t handle;
} vampir_file_t;

EXTERN int max_open_files;
EXTERN uint32_t file_group_id_stdio;
EXTERN uint32_t file_group_id_rest;
EXTERN vampir_file_t *fd_to_vampirid;
EXTERN vampir_file_t unknown_fd_vampir_file;

EXTERN vampir_file_t *get_vampir_file(int fd);
EXTERN int get_max_open_files(void);
EXTERN void vt_iofile_open(const char* fname, int fd);
EXTERN void vt_iofile_close(int fd);
EXTERN uint32_t vt_iofile_id(const char* fname);
EXTERN void vt_iofile_dupfd(int oldfd, int newfd);

#endif /*VT_IOWRAP_HELPER_H_*/
