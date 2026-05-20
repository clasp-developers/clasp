#pragma once

#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Opaque handle types
typedef void* MMTkClaspMutator;
typedef void* MMTkClaspBuilder;

// Allocation semantics — must match mmtk::AllocationSemantics repr
typedef enum {
  MMTK_CLASP_ALLOC_DEFAULT = 0,
  MMTK_CLASP_ALLOC_IMMORTAL = 1,
  MMTK_CLASP_ALLOC_LOS = 2,
  MMTK_CLASP_ALLOC_CODE = 3,
  MMTK_CLASP_ALLOC_READ_ONLY = 4,
  MMTK_CLASP_ALLOC_LARGE_CODE = 5,
  MMTK_CLASP_ALLOC_NON_MOVING = 6,
} MMTkClaspAllocSemantics;

// Initialisation
extern MMTkClaspBuilder mmtk_clasp_create_builder(void);
extern bool mmtk_clasp_set_option(MMTkClaspBuilder builder, const char* name, const char* value);
extern bool mmtk_clasp_set_fixed_heap_size(MMTkClaspBuilder builder, size_t heap_size);
extern bool mmtk_clasp_set_dynamic_heap_size(MMTkClaspBuilder builder, size_t min_heap, size_t max_heap);
extern void mmtk_clasp_init(MMTkClaspBuilder builder);
extern void mmtk_clasp_initialize_collection(void* tls);

// Per-thread mutator lifecycle
extern MMTkClaspMutator mmtk_clasp_bind_mutator(void* tls);
extern void mmtk_clasp_destroy_mutator(MMTkClaspMutator mutator);

// Allocation
// Returns the alloc_start address (= where Header_s is placed).
// The client pointer (MMTk ObjectReference) is alloc_start + 8.
extern void* mmtk_clasp_alloc(MMTkClaspMutator mutator, size_t size, size_t align,
                              MMTkClaspAllocSemantics semantics);
extern void mmtk_clasp_post_alloc(MMTkClaspMutator mutator, void* object_ref, size_t bytes,
                                  MMTkClaspAllocSemantics semantics);

// Heap statistics
extern size_t mmtk_clasp_used_bytes(void);
extern size_t mmtk_clasp_free_bytes(void);
extern size_t mmtk_clasp_total_bytes(void);

// Object queries
extern bool mmtk_clasp_is_live_object(void* object_ref);
extern bool mmtk_clasp_will_never_move(void* object_ref);
extern bool mmtk_clasp_is_in_mmtk_spaces(const void* object_ref);
extern bool mmtk_clasp_is_mapped_address(void* addr);

// GC control
extern void mmtk_clasp_handle_user_collection_request(void* tls);

// Heap address range
extern void* mmtk_clasp_starting_heap_address(void);
extern void* mmtk_clasp_last_heap_address(void);

// Root scanning callbacks for the Rust scanning implementation.
// These are called with the world stopped.
typedef void (*ClaspPreciseRootCallback)(void* slot, void* data);
typedef void (*ClaspConservativeRootCallback)(void* client_ptr, void* data);

// Total allocation size (header + body) for a client pointer.
extern size_t clasp_object_size(void* client);

// Return the MMTk mutator for the thread whose ThreadLocalState* is tls.
extern MMTkClaspMutator clasp_get_mutator(void* tls);

// Walk global (non-thread-local) precise roots.
extern void clasp_walk_global_roots(ClaspPreciseRootCallback callback, void* data);

// Walk one thread's precise roots (TLS fields and VM stack).
// tls is the ThreadLocalState* passed to mmtk_clasp_bind_mutator at thread start.
extern void clasp_walk_thread_precise_roots(void* tls, ClaspPreciseRootCallback callback, void* data);

// Walk one thread's conservative (control stack) roots.
// tls is the ThreadLocalState* passed to mmtk_clasp_bind_mutator at thread start.
extern void clasp_walk_thread_conservative_roots(void* tls, ClaspConservativeRootCallback callback, void* data);

#ifdef __cplusplus
}
#endif
