#ifndef OTFAUX_SHARED_STATE_H
#define OTFAUX_SHARED_STATE_H 1

#include "OTFAUX_Stack.h"

typedef struct OTFAUX_SharedState OTFAUX_SharedState;

struct OTFAUX_SharedState
{
    /* unused OTFAUX_FunctionCall objects */
    Stack functionCalls;

    /* unused OTFAUX_File objects */
    Stack files;

    /* unused OTFAUX_CollOp objects */
    Stack collOps;

    /* unused OTFAUX_FileOps objects */
    Stack fileOps;

    /* Snapshot writing callbacks */
    OTFAUX_WriteEnterSnapshotCallback       writeEnterSnapshot;
    OTFAUX_WriteSendSnapshotCallback        writeSendSnapshot;
    OTFAUX_WriteOpenFileSnapshotCallback    writeOpenFileSnapshot;
    OTFAUX_WriteBeginCollopSnapshotCallback writeBeginCollopSnapshot;
    OTFAUX_WriteBeginFileOpSnapshotCallback writeBeginFileOpSnapshot;
    OTFAUX_WriteCollopCountSnapshotCallback writeCollopCountSnapshot;
    OTFAUX_WriteCounterSnapshotCallback     writeCounterSnapshot;

    /* user provided callback to release an event data */
    OTFAUX_ReleaseEventData releaseEventData;
    void* userDataForReleaseEventData;
};

#endif /* OTFAUX_SHARED_STATE_H */
