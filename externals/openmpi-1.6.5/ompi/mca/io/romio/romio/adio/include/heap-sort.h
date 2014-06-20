#include "adio.h"

typedef struct {
    ADIO_Offset offset;
    int proc;
    ADIO_Offset reg_max_len;
} heap_node_t;

typedef struct {
    heap_node_t *nodes;
    int size;
} heap_t;

/*static inline int parent(heap_t *heap, int i);
static inline int left(heap_t *heap, int i);
static inline int right(heap_t *heap, int i); */
void ADIOI_Heap_free(heap_t *heap);
int ADIOI_Heap_create(heap_t *heap, int size);
void ADIOI_Heap_insert(heap_t *heap, ADIO_Offset offset, int proc,
		 ADIO_Offset reg_max_len);
void ADIOI_Heap_extract_min(heap_t *heap, ADIO_Offset* key, int *proc,
		      ADIO_Offset *reg_max_len);
