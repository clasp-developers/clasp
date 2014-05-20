#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
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

    OTF_FileManager* manager;
    OTF_WStream* wstream;
    OTF_WStream* wstream2;
    OTF_MasterControl* master;

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
    wstream = OTF_WStream_open(namestub, 0, manager);
    assert(wstream);
    wstream2 = OTF_WStream_open(namestub, 1, manager);
    assert(wstream2);
    /* OTF_WStream_setCompression(wstream, 1); */
    clock_gettime(CLOCK_REALTIME, &start_ts);
    for(i = 0; i < num_events / 2; i++)
    {
        OTF_WStream_writeEnter(wstream, time++, 0, 0, 0);
        /* OTF_WStream_writeEnter(wstream2, time++, 0, 0, 0); */
        OTF_WStream_writeLeave(wstream, time++, 0, 0, 0);
        /* OTF_WStream_writeLeave(wstream2, time++, 0, 0, 0); */
    }
    clock_gettime(CLOCK_REALTIME, &end_ts);
    master = OTF_MasterControl_new(manager);
    assert(master != NULL);
    OTF_MasterControl_append(master, i, i);
    OTF_MasterControl_write(master, namestub);
    OTF_MasterControl_close(master);
    secs = (end_ts.tv_sec - start_ts.tv_sec) + (end_ts.tv_nsec - start_ts.tv_nsec) / 1E9;
    printf("Finished: %li events written in %1.5f secs\n--> %5.3f events per second\n", 
            num_events, secs, num_events / secs);

    OTF_WStream_close(wstream);
    OTF_WStream_close(wstream2);
    OTF_FileManager_close(manager);
    clock_gettime(CLOCK_REALTIME, &end_ts);
    return 0;
}

