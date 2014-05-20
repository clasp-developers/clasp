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

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#include "vt_mpicom.h"
#include "vt_error.h"

#include <stdlib.h>
#include <string.h>

/*
 *-----------------------------------------------------------------------------
 * Macros
 *-----------------------------------------------------------------------------
 */

/* Raise the maximum number of MPI handles (groups, comms., or windows)
   defined per process */
#define RAISE_MAX(handles, max, type)                                       \
{                                                                           \
  uint32_t newmax =                                                         \
    (max) + (uint32_t)((double)(max) * 1.5/*raise factor*/ + 0.5/*round*/); \
  /* reallocate handle-array */                                             \
  handles = (type*)realloc(handles, newmax * sizeof(type));                 \
  if ( !handles )                                                           \
    vt_error();                                                             \
  /* initialize new memory */                                               \
  memset(handles + max, 0, (newmax - max) * sizeof(type));                  \
  max = newmax;                                                             \
}

/*
 *-----------------------------------------------------------------------------
 * Data structures
 *-----------------------------------------------------------------------------
 */

/* Structure to hold the MPI_COMM_WORLD definition */

struct VTWorld
{
  MPI_Group   group;
  VT_MPI_INT  size;
  VT_MPI_INT  size_grpv;
  VT_MPI_INT* ranks;
};

/* Structure for group tracking */

struct VTGroup
{
  MPI_Group   group;
  uint32_t    gid;
  uint32_t    refcnt;
};

/* Structure for communicator tracking */

struct VTComm
{
  MPI_Comm    comm;
  uint32_t    cid;
};

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED

/* Structure for window tracking */

struct VTWin
{
  MPI_Win     win;
  MPI_Comm    comm;
  uint32_t    gid;
  uint32_t    wid;
};

#endif /* HAVE_MPI2_1SIDED */

/*
 *-----------------------------------------------------------------------------
 * Global variables
 *-----------------------------------------------------------------------------
 */

/* group of MPI_COMM_WORLD */
MPI_Group vt_mpi_comm_world_group;

/* group of MPI_COMM_SELF */
MPI_Group vt_mpi_comm_self_group;

/* process group id of MPI_COMM_WORLD */
uint32_t vt_mpi_comm_world_cid = (uint32_t)-1;

/* process group id of MPI_COMM_SELF */
uint32_t vt_mpi_comm_self_cid = (uint32_t)-1;

/*
 *-----------------------------------------------------------------------------
 * Local variables
 *-----------------------------------------------------------------------------
 */

/* MPI_COMM_WORLD definition */
static struct VTWorld world;

/* index to group array */
static uint32_t last_group = 0;

/* index to comm array */
static uint32_t last_comm = 0;

/* maximum number of groups defined per process */
static uint32_t max_groups = VT_MAX_MPI_GROUPS_INIT;

/* maximum number of communicators defined per process */
static uint32_t max_comms = VT_MAX_MPI_COMMS_INIT;

/* group tracking data structure */
static struct VTGroup* groups;

/* communicator tracking data structure */
static struct VTComm* comms;

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED

  static uint32_t free_win = (uint32_t)-1;

  /* index to win array */
  static uint32_t last_win = 0;

  /* maximum number of windows defined per process */
  static uint32_t max_wins = VT_MAX_MPI_WINS_INIT;

  /* window tracking data structure */
  static struct VTWin* wins;

#endif /* HAVE_MPI2_1SIDED */

/* array of ranks used for rank translation */
static VT_MPI_INT* ranks;

/* bitvector used for bitvector creation of new communicators/groups */
static uint8_t* grpv;

/* flag to indicate communicator initialization */
static uint8_t comm_initialized = 0;

/*
 *-----------------------------------------------------------------------------
 * Local functions
 *-----------------------------------------------------------------------------
 */

static uint32_t comm_search(MPI_Comm comm)
{
  uint32_t i = 0;

  while ((i < last_comm) && (comms[i].comm != comm))
    i++;

  if (i != last_comm)
    return i;
  else
    return (uint32_t)-1;
}

static uint32_t group_search(MPI_Group group)
{
  uint32_t i = 0;

  while ((i < last_group) && (groups[i].group != group))
    i++;

  if (i != last_group)
    return i;
  else
    return (uint32_t)-1; 
}

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
static uint32_t win_search(MPI_Win win)
{
  uint32_t i = 0;

  free_win = (uint32_t)-1;
  while ((i < last_win)&& (wins[i].win != win))
    {
      if( (free_win == (uint32_t)-1) && (wins[i].win == MPI_WIN_NULL) &&
          (wins[i].comm == MPI_COMM_NULL) )
        free_win = i;
      i++;
    }

  if (i != last_win)
    return i;
  else
    return (uint32_t)-1;
}
#endif /* HAVE_MPI2_1SIDED */

static void group_to_bitvector(MPI_Group group)
{
  int i;
  int size;

  /* determine the world rank of each process in group
     (Parameter #3 is world.ranks here, as we need an array of integers
      initialized with 0 to n-1, which world.ranks happens to be. */
  PMPI_Group_size(group, &size);
  PMPI_Group_translate_ranks(group, size, world.ranks, world.group, ranks);

  /* initialize grpv */
  memset(grpv, 0, world.size_grpv);

  /* set corresponding bit for each process in group */
  for (i = 0; i < size; i++)
    grpv[ranks[i] / 8] |= (1 << (ranks[i] % 8));
}

/*
 *-----------------------------------------------------------------------------
 * Global functions
 *-----------------------------------------------------------------------------
 */

void vt_comm_init()
{
  VT_MPI_INT i;

  if ( !comm_initialized )
  {
    comm_initialized = 1;

    groups = (struct VTGroup*)calloc(max_groups, sizeof(struct VTGroup));
    if ( !groups )
      vt_error();

    comms = (struct VTComm*)calloc(max_comms, sizeof(struct VTComm));
    if ( !comms )
      vt_error();

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
    wins = (struct VTWin*)calloc(max_wins, sizeof(struct VTWin));
    if ( !wins )
      vt_error();
#endif /* HAVE_MPI2_1SIDED */

    PMPI_Comm_group(MPI_COMM_WORLD, &vt_mpi_comm_world_group);
    PMPI_Comm_group(MPI_COMM_SELF, &vt_mpi_comm_self_group);

    world.group = vt_mpi_comm_world_group;
    PMPI_Group_size(world.group, &world.size);
    world.size_grpv = world.size / 8 + (world.size % 8 ? 1 : 0);

    world.ranks  = (VT_MPI_INT*)calloc(world.size, sizeof(VT_MPI_INT));
    if ( !world.ranks )
      vt_error();

    for (i = 0; i < world.size; i++)
      world.ranks[i] = i;

    ranks  = (VT_MPI_INT*)calloc(world.size, sizeof(VT_MPI_INT));
    grpv = (uint8_t*)calloc(world.size_grpv, sizeof(uint8_t));

    vt_comm_create(MPI_COMM_WORLD);
    vt_comm_create(MPI_COMM_SELF);
  }
}

void vt_comm_finalize()
{
  PMPI_Group_free(&world.group);

  free(groups);
  free(comms);
#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
  free(wins);
#endif /* HAVE_MPI2_1SIDED */

  free(world.ranks);
  free(ranks);
  free(grpv);
}

uint32_t vt_rank_to_pe(VT_MPI_INT rank, MPI_Comm comm)
{
  MPI_Group group;
  VT_MPI_INT global_rank;
  VT_MPI_INT inter;

#if defined(HAVE_DECL_MPI_ROOT) && HAVE_DECL_MPI_ROOT
  if ( rank == MPI_ROOT )
    {
      global_rank = (VT_MPI_INT)vt_my_trace;
    }
  else
#endif /* HAVE_DECL_MPI_ROOT */
    {
      PMPI_Comm_test_inter(comm, &inter);
      if ( inter )
        PMPI_Comm_remote_group(comm, &group);
      else
        PMPI_Comm_group(comm, &group);

      PMPI_Group_translate_ranks(group, 1, &rank, world.group, &global_rank);
      PMPI_Group_free(&group);
    }

  return (uint32_t)global_rank;
}

uint32_t vt_rank_to_pe_by_group(VT_MPI_INT rank, MPI_Group group)
{
  VT_MPI_INT global_rank;

#if defined(HAVE_DECL_MPI_ROOT) && HAVE_DECL_MPI_ROOT
  if ( rank == MPI_ROOT )
    {
      global_rank = (VT_MPI_INT)vt_my_trace;
    }
  else
#endif /* HAVE_DECL_MPI_ROOT */
    {
      PMPI_Group_translate_ranks(group, 1, &rank, world.group, &global_rank);
    }

  return (uint32_t)global_rank;
}

/*
 *-----------------------------------------------------------------------------
 * Communicator management
 *-----------------------------------------------------------------------------
 */

void vt_comm_create(MPI_Comm comm)
{
  MPI_Group group, lgroup, rgroup;
  VT_MPI_INT inter;
  VT_MPI_INT size_grpv = 0;
  uint32_t cid;

  /* raise maximum number of communicators, if necessary */
  if (last_comm == max_comms)
    RAISE_MAX(comms, max_comms, struct VTComm);

  /* ask for group of comm */

  PMPI_Comm_test_inter(comm, &inter);

  if (inter)
    {
      PMPI_Comm_remote_group(comm, &rgroup);
      PMPI_Comm_group(comm, &lgroup);

      PMPI_Group_union(lgroup, rgroup, &group);

      PMPI_Group_free(&lgroup);
      PMPI_Group_free(&rgroup);
    }
  else
    {
      PMPI_Comm_group(comm, &group);
    }

  /* create group entry in grpv except for
     MPI_COMM_SELF and
     MPI_COMM_WORLD (if the current rank isn't the first available one) */
  if ((comm != MPI_COMM_SELF && comm != MPI_COMM_WORLD) ||
      (comm == MPI_COMM_WORLD && vt_my_trace_is_first_avail))
    {
      group_to_bitvector( group );
      size_grpv = world.size_grpv;
    }

  /* register mpi communicator definition */
#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_LOCK_IDS();
#endif /* VT_MT || VT_HYB */
  cid = vt_def_mpi_comm(VT_CURRENT_THREAD,
          comm == MPI_COMM_WORLD ? VT_MPI_COMM_WORLD :
          comm == MPI_COMM_SELF ? VT_MPI_COMM_SELF : VT_MPI_COMM_OTHER,
          size_grpv, grpv);
#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB */

  /* save communicator id for fast access in VT_COMM_ID */
  if (comm == MPI_COMM_WORLD) vt_mpi_comm_world_cid = cid;
  else if (comm == MPI_COMM_SELF) vt_mpi_comm_self_cid = cid;

  /* enter comm in comms[] array */
  comms[last_comm].comm = comm;
  comms[last_comm].cid  = cid;
  last_comm++;

  /* clean up */
  PMPI_Group_free(&group);
}

void vt_comm_free(MPI_Comm comm)
{
  /* if only one communicator exists, we just need to decrease last_comm */
  if (last_comm == 1 && comms[0].comm == comm)
    {
      last_comm = 0;
    }
  /* if more than one communicator exists, we need to search for the entry */
  else if (last_comm > 1)
    {
      uint32_t i;

      if ((i = comm_search(comm)) != (uint32_t)-1)
        {
          /* swap deletion candidate with last entry in the list */
          comms[i] = comms[--last_comm];
        }
      else
        {
          vt_error_msg("vt_comm_free1: Cannot find communicator");
        }
    }
  else
    {
      vt_error_msg("vt_comm_free2: Cannot find communicator");
    }
}

uint32_t vt_comm_id(MPI_Comm comm)
{
  uint32_t i;

  if ((i = comm_search(comm)) != (uint32_t)-1)
    {
      return comms[i].cid;
    }
  else
    {
      vt_error_msg("Cannot find communicator");
      return (uint32_t)-1;
    }
}

/*
 *-----------------------------------------------------------------------------
 * Group management
 *-----------------------------------------------------------------------------
 */

void vt_group_create(MPI_Group group)
{
  uint32_t i;

  /* check if group already exists */
  if ((i = group_search( group )) == (uint32_t)-1)
    {
      uint32_t gid;

      /* raise maximum number of groups, if necessary */
      if (last_group == max_groups)
        RAISE_MAX(groups, max_groups, struct VTGroup);

      /* create group entry in grpv */
      group_to_bitvector( group );

      /* register mpi group definition (as communicator) */
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif /* VT_MT || VT_HYB */
      gid = vt_def_mpi_comm(VT_CURRENT_THREAD, VT_MPI_GROUP,
                            world.size_grpv, grpv);
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB */

      /* enter group in groups[] array */
      groups[last_group].group  = group;
      groups[last_group].gid    = gid;
      groups[last_group].refcnt = 1;
      last_group++;
    }
  else
    {
      /* count additional reference on group */
      groups[i].refcnt++;
    }
}

void vt_group_free(MPI_Group group)
{
  if (last_group == 1 && groups[0].group == group)
    {
      groups[0].refcnt--;

      if (groups[0].refcnt == 0)
        last_group--;
    }
  else if (last_group > 1)
    {
      uint32_t i;

      if ((i = group_search(group)) != (uint32_t)-1)
        {
          /* decrease reference count on entry */
          groups[i].refcnt--;

          /* check if entry can be deleted */
          if (groups[i].refcnt == 0)
            groups[i] = groups[--last_group];
        }
      else
        {
          vt_error_msg("vt_group_free1: Cannot find group");
        }
    }
  else
    {
      vt_error_msg("vt_group_free2: Cannot find group");
    }
}

uint32_t vt_group_id(MPI_Group group)
{
  uint32_t i;

  if ((i = group_search(group)) != (uint32_t)-1)
    {
      return groups[i].gid;
    }
  else
    {
      vt_error_msg("Cannot find group");
      return (uint32_t)-1;
    }
}

/*
 *-----------------------------------------------------------------------------
 * Window management
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED

void vt_win_create( MPI_Win win, MPI_Comm comm )
{
  MPI_Group group;

  /* check if window already exists */
  if (win_search( win ) == (uint32_t)-1)
    {
      uint32_t wid;

#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif /* VT_MT || VT_HYB */
      wid = vt_get_curid();
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB */

      /* ask for group of win and register them */

      PMPI_Win_get_group(win, &group);
      vt_libassert(group != MPI_GROUP_NULL);
      vt_group_create(group);

      /* enter win in wins[] array */

      if( free_win != (uint32_t)-1 )
        {
          wins[free_win].win  = win;
          wins[free_win].comm = comm;
          wins[free_win].gid  = vt_group_id(group);
          wins[free_win].wid  = wid;
        }
      else
        {
          /* raise maximum number of windows, if necessary */
          if (last_win == max_wins)
            RAISE_MAX(wins, max_wins, struct VTWin);

          wins[last_win].win  = win;
          wins[last_win].comm = comm;
          wins[last_win].gid  = vt_group_id(group);
          wins[last_win].wid  = wid;
          last_win++;
        }
    }
}

void vt_win_free( MPI_Win win )
{
  uint32_t i = win_search(win);
  if ( i != (uint32_t)-1 )
    {
      wins[i].win  = MPI_WIN_NULL;
      wins[i].comm = MPI_COMM_NULL;
      wins[i].gid  = 0;
      wins[i].wid  = 0;
    }
  else
    {
      vt_error_msg("Cannot find window");  
    }
}

void vt_win_id( MPI_Win win, MPI_Comm* comm, uint32_t* gid, uint32_t* wid )
{
  uint32_t i = win_search(win);
  
  if ( i != (uint32_t)-1 )
    {
      *comm = wins[i].comm;
      *gid  = wins[i].gid;
      *wid  = wins[i].wid;
    }
  else
    {
      vt_error_msg("Cannot find window");  
    }
}

void vt_win_set_gid( MPI_Win win, uint32_t gid )
{
  uint32_t i = win_search(win);

  if ( i != (uint32_t)-1 )
    {
      wins[i].gid = gid;
    }
  else
    {
      vt_error_msg("Cannot find window");
    }
}

#endif /* HAVE_MPI2_1SIDED */
