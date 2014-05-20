
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <sys/time.h>
#include <assert.h>

#include "otf.h"


#define HELPTEXT "" \
"                                                             \n" \
" ./hello_otf - OTF hello world example that writes a most    \n" \
"               simple trace to demonstrate the OTF writer API\n" \
"                                                             \n" \
"                                                             \n" \


int main( int argc, const char** argv ) {


    char *filename= "hello_world.otf";

    OTF_FileManager* manager;
    OTF_Writer* writer;

    /* test with # streams < or > # processes */
    uint32_t streams= 2;
    uint32_t processes= 2;

    /* file handles available */
    uint32_t files= 100;

    /* per-stream buffer size */
    uint32_t buffersize= 10*1024;

    /* compression on/off, test without compression to look at ASCII trace files */
    int compression= 0;

    uint64_t timestamp;
    uint32_t cpuid;
    uint32_t counterid= 5;
    uint64_t countervalue;

    /* key-value-list to re-use all the time */
    OTF_KeyValueList* keyvaluelist;

    /* two macros that define my keys in key-value-lists  */
    const uint32_t MY_KEY_1= 42;
    const uint32_t MY_KEY_2= 987654;

    manager= OTF_FileManager_open( files );
    assert( NULL != manager );

    writer = OTF_Writer_open( filename, streams, manager );
    OTF_Writer_setBufferSizes( writer, buffersize );

    if ( 0 < compression && compression <= 9 ) {

        OTF_Writer_setCompression( writer, compression );
    }

    /* prepare key-value list */
    keyvaluelist= OTF_KeyValueList_new();

    /* do for a number of processes */
    for( cpuid= 1; cpuid <= processes; cpuid++ ) {
        
        timestamp= 0;
        timestamp += 10000 + rand() % 100;

        /* enter main */

        OTF_Writer_writeEnter( writer,
            timestamp /* time */,
            1 /* statetoken */,
            cpuid /* cpuid */,
            0 /* scltoken */ );
        countervalue= 0;
        OTF_Writer_writeCounter( writer, 
            timestamp, 
            cpuid, 
            counterid, 
            countervalue );

        timestamp += 1000 + rand() % 100;

        /* enter foo */

        OTF_Writer_writeEnter( writer,
            timestamp /* time */,
            2 /* statetoken */,
            cpuid /* cpuid */,
            0 /* scltoken */ );
        countervalue= 100;
        OTF_Writer_writeCounter( writer, 
            timestamp, 
            cpuid, 
            counterid, 
            countervalue );

        timestamp += 1000 + rand() % 100;

        /* enter bar */
        OTF_KeyValueList_appendDouble( keyvaluelist, MY_KEY_2, 2.71828183 );

        OTF_Writer_writeEnterKV( writer,
            timestamp /* time */,
            3 /* statetoken */,
            cpuid /* cpuid */,
            0 /* scltoken */,
            keyvaluelist /*  OTF_KeyValueList* */ );
        countervalue= 110;
        OTF_Writer_writeCounter( writer, 
            timestamp, 
            cpuid, 
            counterid, 
            countervalue );

        timestamp += 1000 + rand() % 100;

        /* leave bar */

        countervalue= 210;
        OTF_Writer_writeCounter( writer, 
            timestamp, 
            cpuid, 
            counterid, 
            countervalue );

        OTF_Writer_writeLeave( writer,
            timestamp /* time */,
            3 /* statetoken */,
            cpuid /* cpuid */,
            0 /* scltoken */ );

        timestamp += 1000 + rand() % 100;

        /* enter bar */

        /* write variant with keyvalue list */

        OTF_KeyValueList_appendUint64( keyvaluelist, MY_KEY_1, 1 );
        OTF_KeyValueList_appendDouble( keyvaluelist, MY_KEY_2, 3.14159265 );

        OTF_Writer_writeEnterKV( writer,
            timestamp /* time */,
            3 /* statetoken */,
            cpuid /* cpuid */,
            0 /* scltoken */,
            keyvaluelist /*  OTF_KeyValueList* */ );
        countervalue= 220;
        OTF_Writer_writeCounter( writer, 
            timestamp, 
            cpuid, 
            counterid, 
            countervalue );

        timestamp += 1000 + rand() % 100;

        /* leave bar */

        countervalue= 320;
        OTF_Writer_writeCounter( writer, 
            timestamp, 
            cpuid, 
            counterid, 
            countervalue );

        OTF_Writer_writeLeave( writer,
            timestamp /* time */,
            3 /* statetoken */,
            cpuid /* cpuid */,
            0 /* scltoken */ );

        timestamp += 1000 + rand() % 100;

        /* leave foo */

        countervalue= 330;
        OTF_Writer_writeCounter( writer, 
            timestamp, 
            cpuid, 
            counterid, 
            countervalue );

        OTF_Writer_writeLeave( writer,
            timestamp /* time */,
            2 /* statetoken */,
            cpuid /* cpuid */,
            0 /* scltoken */ );

        timestamp += 1000 + rand() % 100;

        /* leave main */

        countervalue= 380;
        OTF_Writer_writeCounter( writer, 
            timestamp, 
            cpuid, 
            counterid, 
            countervalue );

        OTF_Writer_writeLeave( writer,
            timestamp /* time */,
            1 /* statetoken */,
            cpuid /* cpuid */,
            0 /* scltoken */ );

    }


    /* write definitions -- may be written at any time 
    before/during/after writing events */

    OTF_Writer_writeDefTimerResolution( writer, 
        0 /* uint32_t stream */, 
        1e6 /* uint64_t ticksPerSecond */ );

    for( cpuid= 1; cpuid <= processes; cpuid++ ) {

        char name[101];
        snprintf( name, 100, "Process %u", cpuid );

        OTF_Writer_writeDefProcess( writer, 
            0 /* uint32_t stream */, 
            cpuid /*uint32_t process */, 
            name /* const char *name */ , 
            0 /* uint32_t parent */ );
    }

    OTF_Writer_writeDefFunctionGroup( writer, 
        0 /* uint32_t stream */, 
        16 /* uint32_t funcGroup */, 
        "standard functions" /* const char *name */ );

    OTF_Writer_writeDefFunctionGroup( writer, 
        0 /* uint32_t stream */, 
        17 /* uint32_t funcGroup */, 
        "user functions" /* const char *name */ );

    OTF_Writer_writeDefFunction( writer, 
        0 /* uint32_t stream */, 
        1 /* uint32_t func */, 
        "main" /* const char *name */, 
        16 /* uint32_t funcGroup */, 
        0 /* uint32_t source */ );

    OTF_Writer_writeDefFunction( writer, 
        0 /* uint32_t stream */, 
        2 /* uint32_t func */, 
        "foo" /* const char *name */, 
        17 /* uint32_t funcGroup */, 
        0 /* uint32_t source */ );

    OTF_Writer_writeDefFunction( writer, 
        0 /* uint32_t stream */, 
        3/* uint32_t func */, 
        "bar" /* const char *name */, 
        17 /* uint32_t funcGroup */, 
        0 /* uint32_t source */ );

    OTF_Writer_writeDefCounterGroup( writer, 
        0 /* uint32_t stream */, 
        63 /* uint32_t counterGroup */, 
        "all my counters" /* const char *name */ );

    OTF_Writer_writeDefCounter( writer, 
        0 /* uint32_t stream */, 
        counterid /* uint32_t counter */, 
        "my counter" /* const char *name */, 
        OTF_COUNTER_TYPE_ACC | OTF_COUNTER_SCOPE_START /* uint32_t properties */, 
        63 /* uint32_t counterGroup */, 
        "my unit" /* const char *unit */ );

    OTF_Writer_writeDefKeyValue( writer,
        0 /* uint32_t   stream */,
        MY_KEY_1 /* uint32_t   key */, 
        OTF_UINT64 /* OTF_Type   type */, 
        "my_key" /* const char *   name */, 
        "my own key to mark my most favorite function call" /* const char *   description */ );

    OTF_Writer_writeDefKeyValue( writer,
        0 /* uint32_t   stream */,
        MY_KEY_2 /* uint32_t   key */, 
        OTF_DOUBLE /* OTF_Type   type */, 
        "first_arg" /* const char *   name */, 
        "first argument of functin bar" /* const char *   description */ );

    OTF_Writer_writeDefMarker( writer, 0 /* stream ID*/,
        101 /* uint32_t token */, 
        "everything is easy" /* const char* name */, 
        OTF_MARKER_TYPE_HINT /* uint32_t type */ );

    OTF_Writer_writeDefMarker( writer, 0 /* stream ID*/,
        102 /* uint32_t token */, 
        "yellow alarm" /* const char* name */, 
        OTF_MARKER_TYPE_WARNING /* uint32_t type */ );

    OTF_Writer_writeDefMarker( writer, 0 /* stream ID*/,
        103 /* uint32_t token */, 
        "red alarm flag" /* const char* name */, 
        OTF_MARKER_TYPE_ERROR /* uint32_t type */ );


    OTF_Writer_writeMarker( writer, 
        10000 /* uint64_t time */, 
        1 /* uint32_t process */, 
        101 /* uint32_t token */, 
        "tea, early grey, hot" /* const char* text */ );

    OTF_Writer_writeMarker( writer, 
        11000 /* uint64_t time */, 
        1 /* uint32_t process */, 
        102 /* uint32_t token */, 
        "bird of prey decloaking starboard" /* const char* text */ );

     OTF_Writer_writeMarker( writer, 
        12000 /* uint64_t time */, 
        1 /* uint32_t process */, 
        103 /* uint32_t token */, 
        "antimatter containment failing" /* const char* text */ );

    OTF_KeyValueList_close( keyvaluelist );

    OTF_Writer_close( writer );
    OTF_FileManager_close( manager );

    return 0;
}


