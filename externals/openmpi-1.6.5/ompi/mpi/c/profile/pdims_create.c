/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Dims_create = PMPI_Dims_create
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Dims_create";

/* static functions */
static int assignnodes(int ndim, int nfactor, int *pfacts, int *counts, int **pdims);
static int getfactors(int num, int nprime, int *primes, int **pcounts);
static int getprimes(int num, int *pnprime, int **pprimes);


/*
 * This is a utility function, no need to have anything in the lower
 * layer for this at all
 */ 
int MPI_Dims_create(int nnodes, int ndims, int *dims) 
{
    int i;
    int freeprocs;
    int freedims;
    int nprimes;
    int *primes;
    int *factors;
    int *procs;
    int *p;
    int err;

    OPAL_CR_NOOP_PROGRESS();

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (NULL == dims) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD,
                                           MPI_ERR_ARG, FUNC_NAME);
        }
        
        if (1 > ndims) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, 
                                           MPI_ERR_DIMS, FUNC_NAME);
        }
    }

    /* Get # of free-to-be-assigned processes and # of free dimensions */
    freeprocs = nnodes;
    freedims = 0;
    for (i = 0, p = dims; i < ndims; ++i,++p) {
        if (*p == 0) {
            ++freedims;
        } else if ((*p < 0) || ((nnodes % *p) != 0)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_DIMS,
                                           FUNC_NAME);
        } else {
            freeprocs /= *p;
        }
    }

    if (freedims == 0) {
       if (freeprocs == 1) {
          return MPI_SUCCESS;
       }
       return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_DIMS,
                                     FUNC_NAME);
    }

    if (freeprocs < 1) {
       return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_DIMS,
                                     FUNC_NAME);
    }
    else if (freeprocs == 1) {
        for (i = 0; i < ndims; ++i, ++dims) {
            if (*dims == 0) {
               *dims = 1;
            }
        }
        return MPI_SUCCESS;
    }

    /* Compute the relevant prime numbers for factoring */
    if (MPI_SUCCESS != (err = getprimes(freeprocs, &nprimes, &primes))) {
       return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err,
                                     FUNC_NAME);
    }
    
    /* Factor the number of free processes */
    if (MPI_SUCCESS != (err = getfactors(freeprocs, nprimes, primes, &factors))) {
       return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err,
                                     FUNC_NAME);
    }

    /* Assign free processes to free dimensions */
    if (MPI_SUCCESS != (err = assignnodes(freedims, nprimes, primes, factors, &procs))) {
       return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, err,
                                     FUNC_NAME);
    }

    /* Return assignment results */
    p = procs;
    for (i = 0; i < ndims; ++i, ++dims) {
        if (*dims == 0) {
           *dims = *p++;
        }
    }

    free((char *) primes);
    free((char *) factors);
    free((char *) procs);

    /* all done */
    return MPI_SUCCESS;
}

/*
 *  assignnodes
 *
 *  Function:   - assign processes to dimensions
 *          - get "best-balanced" grid
 *          - greedy bin-packing algorithm used
 *          - sort dimensions in decreasing order
 *          - dimensions array dynamically allocated
 *  Accepts:    - # of dimensions
 *          - # of prime factors
 *          - array of prime factors
 *          - array of factor counts
 *          - ptr to array of dimensions (returned value)
 *  Returns:    - 0 or ERROR
 */
static int
assignnodes(int ndim, int nfactor, int *pfacts, int *counts, int **pdims)
{
    int *bins;
    int i, j;
    int n;
    int f;
    int *p;
    int *pmin;
          
    if (0 >= ndim) {
       return MPI_ERR_DIMS;
    }

    /* Allocate and initialize the bins */
    bins = (int *) malloc((unsigned) ndim * sizeof(int));
    if (NULL == bins) {
       return MPI_ERR_NO_MEM;
    }
    *pdims = bins;

    for (i = 0, p = bins; i < ndim; ++i, ++p) {
        *p = 1;
     }
    
    /* Loop assigning factors from the highest to the lowest */
    for (j = nfactor - 1; j >= 0; --j) {
       f = pfacts[j];
       for (n = counts[j]; n > 0; --n) {
            /* Assign a factor to the smallest bin */
            pmin = bins;
            for (i = 1, p = pmin + 1; i < ndim; ++i, ++p) {
                if (*p < *pmin) {
                    pmin = p;
                }
            }
            *pmin *= f;
        }
     }
    
     /* Sort dimensions in decreasing order (O(n^2) for now) */
     for (i = 0, pmin = bins; i < ndim - 1; ++i, ++pmin) {
         for (j = i + 1, p = pmin + 1; j < ndim; ++j, ++p) {
             if (*p > *pmin) {
                n = *p;
                *p = *pmin;
                *pmin = n;
             }
         }
     }

     return MPI_SUCCESS;
}

/*
 *  getfactors
 *
 *  Function:   - factorize a number
 *  Accepts:    - number
 *          - # of primes
 *          - array of primes
 *          - ptr to array of counts (returned value)
 *  Returns:    - 0 or ERROR
 */
static int
getfactors(int num, int nprime, int *primes, int **pcounts)
{
    int *counts;
    int i;
    int *p;
    int *c;
    
    if (0 >= nprime) {
        return MPI_ERR_INTERN;
    }

    /* Allocate the factor counts array */
    counts = (int *) malloc((unsigned) nprime * sizeof(int));
    if (NULL == counts) {
       return MPI_ERR_NO_MEM;
    }

    *pcounts = counts;

    /* Loop over the prime numbers */
    i = nprime - 1;
    p = primes + i;
    c = counts + i;

    for (; i >= 0; --i, --p, --c) {
        *c = 0;
        while ((num % *p) == 0) {
            ++(*c);
            num /= *p;
        }
    }

    if (1 != num) {
        return MPI_ERR_INTERN;
    }

    return MPI_SUCCESS;
}

/*
 *  getprimes
 *
 *  Function:   - get primes smaller than number
 *          - array of primes dynamically allocated
 *  Accepts:    - number
 *          - ptr # of primes (returned value)
 *          - ptr array of primes (returned values)
 *  Returns:    - 0 or ERROR
 */
static int
getprimes(int num, int *pnprime, int **pprimes) {

   int i, j;
   int n;
   int size;
   int *primes;

   /* Allocate the array of primes */
   size = (num / 2) + 1;
   primes = (int *) malloc((unsigned) size * sizeof(int));
   if (NULL == primes) {
       return MPI_ERR_NO_MEM;
   }
   *pprimes = primes;

   /* Find the prime numbers */
   i = 0;
   primes[i++] = 2;

   for (n = 3; n <= num; n += 2) {
      for (j = 1; j < i; ++j) {
         if ((n % primes[j]) == 0) {
             break;
          }
      }

      if (j == i) {
        if (i >= size) {
           return MPI_ERR_DIMS;
         }
         primes[i++] = n;
      }
   }

   *pnprime = i;
   return MPI_SUCCESS;
}

