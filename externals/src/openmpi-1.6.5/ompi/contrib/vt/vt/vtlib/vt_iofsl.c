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

#include "config.h"

#include "vt_env.h"
#include "vt_error.h"
#include "vt_iofsl.h"
#include "vt_trc.h"

#include "otf.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* indicator for enabled/disabled IOFSL mode */
uint8_t vt_iofsl_enabled = 0;

/* IOFSL mode
   (either VT_IOFSL_MODE_MULTIFILE or VT_IOFSL_MODE_MULTIFILE_SPLIT) */
uint32_t vt_iofsl_mode = 0;

/* IOFSL flags bitmask */
uint32_t vt_iofsl_flags = 0;

/* number of IOFSL servers */
uint32_t vt_iofsl_servers_num = 0;

/* IOFSL server addresses */
char** vt_iofsl_servers_list = NULL;

/* indicator for IOFSL initialization */
static uint8_t iofsl_initialized = 0;

/* remove previous created output files to prevent appending data to it */
static void remove_previous_output( void )
{
  char filename_prefix[VT_PATH_MAX];
  char filename[VT_PATH_MAX];
  int server_idx, file_type_idx, compress_type_idx, iofsl_type_idx;

  /* compose common file prefix */
  if (vt_my_funique > 0)
    snprintf(filename_prefix, sizeof(filename_prefix) - 1, "%s/%s_%u",
      vt_env_gdir(), vt_env_fprefix(), vt_my_funique);
  else
    snprintf(filename_prefix, sizeof(filename_prefix) - 1, "%s/%s",
      vt_env_gdir(), vt_env_fprefix());

  /* iterate over IOFSL server indices */
  for(server_idx = vt_my_trace; server_idx < (int)vt_iofsl_servers_num;
      server_idx+=vt_num_traces)
  {
    /* iterate over file types (defs, marker, events, stats.) */
    for(file_type_idx = 0; file_type_idx < 4; file_type_idx++)
    {
      OTF_FileType file_type;

      /* set file type */
      switch(file_type_idx)
      {
        case 0:
          file_type = OTF_FILETYPE_DEF;
          break;
        case 1:
          file_type = OTF_FILETYPE_MARKER;
          break;
        case 2:
          file_type = OTF_FILETYPE_EVENT;
          break;
        case 3:
        default:
          file_type = OTF_FILETYPE_STATS;
          break;
      }

      /* iterate over compression types (compressed, uncompressed) */
      for(compress_type_idx = 0; compress_type_idx < 2; compress_type_idx++)
      {
        /* set compression bits of file type */
        switch(compress_type_idx)
        {
          case 0:
            file_type &= ~OTF_FILECOMPRESSION_COMPRESSED;
            file_type |= OTF_FILECOMPRESSION_UNCOMPRESSED;
            break;
          case 1:
          default:
            file_type &= ~OTF_FILECOMPRESSION_UNCOMPRESSED;
            file_type |= OTF_FILECOMPRESSION_COMPRESSED;
            break;
        }

        /* iterate over IOFSL file types (all, idx) */
        for(iofsl_type_idx = 0; iofsl_type_idx < 2; iofsl_type_idx++)
        {
          switch(iofsl_type_idx)
          {
            case 0:
              file_type &= ~OTF_FILETYPE_IOFSL_IDX;
              file_type |= OTF_FILETYPE_IOFSL_ALL;
              break;
            case 1:
            default:
              file_type &= ~OTF_FILETYPE_IOFSL_ALL;
              file_type |= OTF_FILETYPE_IOFSL_IDX;
            break;
          }

          /* get file name */
          OTF_getFilename(filename_prefix, server_idx, file_type, VT_PATH_MAX,
            filename);
          /* remove file */
          if (remove(filename) == 0)
            vt_cntl_msg(1, "Removed %s", filename);
        }
      }
    }
  }
}

void vt_iofsl_init()
{
  if (!iofsl_initialized)
  {
    char* iofsl_servers = vt_env_iofsl_servers();

    iofsl_initialized = 1;

    /* extract IOFSL server addresses from env. VT_IOFSL_SERVERS */
    if (iofsl_servers)
    {
      char* tmpbuf;
      char* p;
      char* tk;
      uint32_t n;

      /* get maximum number of IOFSL servers by counting separators */

      p = iofsl_servers;
      n = 0;
      while(*p)
      {
        if (*p == ',') n++;
        p++;
      }
      n++;

      /* allocate list of IOFSL servers */

      vt_iofsl_servers_list = (char**)malloc(n * sizeof(char*));
      if (vt_iofsl_servers_list == NULL)
        vt_error();

      /* get a copy of environment variable content to disassemble it
         with strtok() */
      tmpbuf = strdup(iofsl_servers);
      if (tmpbuf == NULL)
        vt_error();

      /* extract IOFSL server addresses */

      tk = strtok(tmpbuf, ",");
      do
      {
        if (tk && strlen(tk) > 0)
        {
          vt_iofsl_servers_list[vt_iofsl_servers_num] = strdup(tk);
          if (vt_iofsl_servers_list[vt_iofsl_servers_num] == NULL)
            vt_error();

          vt_iofsl_servers_num++;
        }
      }
      while((tk = strtok(0, ",")));

      /* reallocate list of IOFSL servers to the actual size needed */
      if (n > vt_iofsl_servers_num)
      {
        vt_iofsl_servers_list =
          (char**)realloc(vt_iofsl_servers_list,
            vt_iofsl_servers_num * sizeof(char*));
      }

      if (vt_iofsl_servers_num > 0)
      {
        /* set IOFSL flags */
        vt_iofsl_flags |=
          (vt_env_iofsl_async_io() ? VT_IOFSL_FLAG_ASYNC_IO : 0);

        /* set IOFSL mode */
        vt_iofsl_mode = vt_env_iofsl_mode();

        /* set indicator for IOFSL mode enabled */
        vt_iofsl_enabled = 1;

        vt_cntl_msg(2, "IOFSL mode enabled using %u server(s)",
                    vt_iofsl_servers_num);

        /* remove previous created output files to prevent appending
           data to it */
        remove_previous_output();
      }

      free(tmpbuf);
    }
  }
}

void vt_iofsl_finalize()
{
  if (iofsl_initialized)
  {
    uint32_t i;

    for(i = 0; i < vt_iofsl_servers_num; i++)
      free(vt_iofsl_servers_list[i]);
    free(vt_iofsl_servers_list);
    vt_iofsl_servers_list = NULL;
    vt_iofsl_servers_num = 0;
    vt_iofsl_enabled = 0;
    vt_iofsl_mode = 0;
    vt_iofsl_flags = 0;

    iofsl_initialized = 0;
  }
}
