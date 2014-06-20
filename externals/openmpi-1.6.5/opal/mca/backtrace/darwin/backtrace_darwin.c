/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>

#include "opal/constants.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/mca/backtrace/darwin/MoreBacktrace/MoreDebugging/MoreBacktrace.h"
#include "opal/mca/backtrace/darwin/MoreBacktrace/MoreDebugging/MoreAddrToSym.h"

#define SIGNAL_SKIPFRAME 4
#define ALWAYS_SKIPFRAME 2
#define kFrameCount 32

void
opal_backtrace_print(FILE *file)
{
    int ret;
    unsigned long frameCount;
    unsigned long validFrames;
    MoreBTFrame frames[kFrameCount];
    unsigned long frameIndex, skipframe = 0;
    MoreAToSSymInfo symbol;
    MoreAToSAddr address;

    frameCount = sizeof(frames) / sizeof(*frames);
    ret = MoreBacktraceMachSelf(0, 0, frames, frameCount, &validFrames);
    if (0 != ret) {
        fprintf(file, "Stack corrupted and not printable\n");
        fflush(stderr);
        return;
    }

    if (validFrames > frameCount) {
        validFrames = frameCount;
    }
        
    if ((validFrames >= SIGNAL_SKIPFRAME) && 
        (frames[SIGNAL_SKIPFRAME-1].flags & kMoreBTSignalHandlerMask)) {
            skipframe = SIGNAL_SKIPFRAME;
    } else if (validFrames >= ALWAYS_SKIPFRAME) {
        skipframe = ALWAYS_SKIPFRAME;
    }

    for (frameIndex = skipframe; frameIndex < validFrames; frameIndex++) {
        fprintf(stderr, "[%2ld] ", frameIndex - skipframe + 1);

        if (frames[frameIndex].flags & kMoreBTPCBadMask) {
            address = 0;
        } else {
            address = (MoreAToSAddr) frames[frameIndex].pc;
        }

        symbol.symbolName = NULL;
        symbol.symbolType = kMoreAToSNoSymbol;
        symbol.symbolOffset = 0;
                
        ret = MoreAToSCopySymbolNamesUsingDyld(1, &address, &symbol);
        
        if (symbol.symbolName) {
            if (symbol.symbolName[0]) {
                fprintf(file, "(%s + 0x%llx) ", 
                        symbol.symbolName, symbol.symbolOffset);
                free( (void *) symbol.symbolName);
            }
        }

        fprintf(file, "[0x%08llx, 0x%08llx] ",
                frames[frameIndex].fp, 
                frames[frameIndex].pc);

        if (frames[frameIndex].flags) {
            fprintf(file, "(%c%c%c)", 
                    (frames[frameIndex].flags & kMoreBTFrameBadMask)      ? 'F' : '-',
                    (frames[frameIndex].flags & kMoreBTPCBadMask)         ? 'P' : '-',
                    (frames[frameIndex].flags & kMoreBTSignalHandlerMask) ? 'S' : '-');
        }
                                                         
        fprintf(file, "\n");
    }
}


int
opal_backtrace_buffer(char ***message_out, int *len_out) 
{
    *message_out = NULL;
    *len_out = 0;

    return OPAL_ERR_NOT_IMPLEMENTED;
}
