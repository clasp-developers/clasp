#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <mpi.h>
#include <otf.h>

int main(int argc, char** argv)
{
    const char* namestub;
    uint64_t num_events = 100000000; /* 1E9 events */
    uint64_t i;
    uint64_t time = 0;
    struct timespec start_ts;
    struct timespec end_ts;
    double secs;
    int com_rank, com_size;

    OTF_FileManager* manager;
    OTF_WStream* wstream;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &com_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &com_size);

    if (argc > 1)
    {
        namestub = argv[1];
    } else {
        fprintf(stderr, "No filenamestub was given!\n");
        exit(1);
    }   
    if (argc > 2)
    {
        num_events = atol(argv[2]);
    }
    manager = OTF_FileManager_open(4);
    assert(manager);
    wstream = OTF_WStream_open(namestub, com_rank + 1, manager);
    assert(wstream);
    clock_gettime(CLOCK_REALTIME, &start_ts);
    printf("[%i] Starting to send %lli events\n", com_rank, (long long int)num_events / com_size);
    for(i = 0; i < num_events / 2 / com_size; i++)
    {
        OTF_WStream_writeEnter(wstream, time++, com_size + 1, com_rank + 1, 0);
        OTF_WStream_writeLeave(wstream, time++, com_size + 1, com_rank + 1, 0);
    }
    OTF_WStream_close(wstream);
    MPI_Barrier(MPI_COMM_WORLD);
    clock_gettime(CLOCK_REALTIME, &end_ts);
    /* write master control file */
    if (com_rank == 0)
    {
        OTF_MasterControl* master = OTF_MasterControl_new(manager);
        assert(master != NULL);
        for (i = 1; i <= (uint64_t)com_size; i++)
        {
            OTF_MasterControl_append(master, i, i);
        }
        OTF_MasterControl_write(master, namestub);
        OTF_MasterControl_close(master);
    }
    secs = (end_ts.tv_sec - start_ts.tv_sec) + (end_ts.tv_nsec - start_ts.tv_nsec) / 1E9;
    printf("Finished: %li events written in %1.5f secs\n--> %5.3f events per second\n", 
            num_events, secs, num_events / secs);

    OTF_FileManager_close(manager);
    MPI_Finalize();
    return 0;
}

