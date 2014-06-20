#include <stdio.h>

#include "opari_omp.h"

static void log(const char* msg, int val) {
  printf("--- %3d: %s", omp_get_thread_num(), msg);
  if ( val != -1 ) printf(" %d", val);
  printf("\n");
  fflush(stdout);
}

const int iterations = 4;

int main() {
  int i, k = 0;
  omp_lock_t lck;

  #pragma omp inst init

  log("sequential", 0);

  /* ---- plain parallel region ---- */
  #pragma omp parallel
  {
    log("parallel", 0);
  }

  log("sequential", 1);

  /* ---- large parallel region ---- */
  #pragma omp parallel
  {
    log("parallel", 1);

    #pragma omp inst begin(worksharing)

    /* ---- worksharing for loop without synchronisation ---- */
    #pragma omp for nowait
    for(i=0; i<iterations; ++i) {
      log("for nowait iteration", i);
    }

    /* ---- user specified barrier ---- */
    #pragma omp barrier

    /* ---- worksharing for loop with implicit synchronisation ---- */
    #pragma omp for
    for(i=0; i<iterations; ++i) {
      log("for iteration", i);
    }
 
    /* ---- worksharing tasks without synchronisation ---- */
    #pragma omp sections nowait
    {
      #pragma omp section
        log("section nowait", 1);
      #pragma omp section
      {
        log("section nowait", 2);
      }
    }

    /* ---- worksharing tasks with implicit synchronisation ---- */
    #pragma omp sections
    {
      #pragma omp section
      {
        log("section", 1);
      }
      #pragma omp section
        log("section", 2);
    }

    #pragma omp inst end(worksharing)

    #pragma omp inst begin(synchronisation)

    /* ---- critical section ---- */
    #pragma omp critical
    {
      log("critical\n", -1);
      k += 1;
    }

    /* ---- named critical section ---- */
    #pragma omp critical(kincr)
    {
      log("critical\n", -1);
      k += 1;
    }

    /* ---- atomic expression ---- */
    #pragma omp atomic
      k += 1;

    /* ---- update k just once without synchronisation ---- */
    #pragma omp single nowait
    {
      log("single nowait\n", -1);
      k += 1;
    }

    /* ---- update k just once with implicit synchronisation ---- */
    #pragma omp single
    {
      log("single\n", -1);
      k += 1;
    }

    #pragma omp master
    {
      log("master\n", -1);
      printf("k = %d\n", k);
      k = 0;
    }

    #pragma omp inst end(synchronisation)
  } /* end parallel ---- */

  log("sequential", 2);

  #pragma omp inst begin(parallelworksharing)

  /* ---- combined parallel worksharing for loop ---- */
  #pragma omp parallel for \
          reduction(+:k)  \
          private(i)       \
          schedule(dynamic)
  for(i=0; i<iterations; ++i) {
    log("pfor", i);
  }

  log("sequential", 3);

  /* ---- combined parallel worksharing tasks ---- */
  #pragma omp parallel sections
  {
    #pragma omp section
      log("psection", 1);
    #pragma omp section
      log("psection", 2);
  }

  log("sequential", 4);

  #pragma omp inst end(parallelworksharing)

  /* ---- OpenMP locking API ---- */
  #pragma omp inst begin(locking)

  omp_init_lock(&lck);

  #pragma omp parallel shared(lck)
  {
    omp_set_lock(&lck);
    log("got lock", -1);
    omp_unset_lock(&lck);

    while (! omp_test_lock(&lck)) {
      log("skipping", -1);
    }
    log("working", -1);
    omp_unset_lock(&lck);
  }

  omp_destroy_lock(&lck);

  #pragma omp flush(k)

  #pragma omp inst end(locking)

  return 0;
}
