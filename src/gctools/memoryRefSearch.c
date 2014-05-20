

#include "mps/code/mpscams.h"  // AMS pool
#include "mps/code/mpscamc.h" // AMC pool
#include "mps/code/ring.h"
#include "mps/code/mpm.h"
#include "mps/code/mpmtypes.h"


extern void pointerSearcherAddRef(void* searcher, mps_addr_t q);

void memory_find_ref(Arena arena, Addr ref, void* searcher )
{
    Ring node, next;  
    Addr base, limit, p;
    ArenaExposeRemember(ArenaGlobals(arena), 0); /* call this to remove protection */
    RING_FOR(node, &arena->chunkRing, next) {
        Chunk chunk = RING_ELT(Chunk, chunkRing, node);
        base = chunk->base;
        limit = chunk->limit;
        for (p=base; p<limit; p = AddrAdd(p, arena->alignment)) {
            if (ArenaHasAddr(arena, p)) {
                Addr q;
                for (q = p; q < AddrAdd(p,arena->alignment); q = AddrAdd(q, sizeof(Addr))) {
                    if (*(Ref*)q == ref) {
                        pointerSearcherAddRef(searcher,q);
                    }
                }
            }
        }
    }
}
