/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2008 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "heap-sort.h"

#define NOEXP2

static void heapify(heap_t *heap, int i);

/* From Introduction To Algorithms by Cormen, Leiserson, and Rivest */

static inline int parent(int i) {
    return (i/2);
}

static inline int left(int i) {
    return (2*i);
}

static inline int right(int i) {
    return (2*i + 1);
}

int ADIOI_Heap_create(heap_t *heap, int size) {
    heap->size = size;
    heap->nodes = (heap_node_t *) ADIOI_Calloc (size, sizeof(heap_node_t));
    if (heap->nodes == NULL)
	return 1;
    else
	return 0;
}

void ADIOI_Heap_free(heap_t *heap) {
    ADIOI_Free(heap->nodes);
}

/* should suppress unused warnings on GCC */
static void build_heap(heap_t *heap) ATTRIBUTE((unused, used));

static void build_heap(heap_t *heap)
{
    int i;
    for (i=(heap->size/2-1); i >= 0; i--)
	heapify(heap, i);
}

static void heapify(heap_t *heap, int i) {
    int l, r, smallest;
    heap_node_t *nodes;
    heap_node_t tmp_node;

    nodes = heap->nodes;

    l = left(i);
    r = right(i);

    if ((l <= heap->size) && (nodes[l].offset < nodes[i].offset))
	smallest = l;
    else
	smallest = i;

    if ((r <= heap->size) && (nodes[r].offset < nodes[smallest].offset))
	smallest = r;

    if (smallest != i) {
	tmp_node = nodes[i];
	nodes[i] = nodes[smallest];
	nodes[smallest] = tmp_node;
	heapify(heap, smallest);
    }
}

void ADIOI_Heap_insert(heap_t *heap, ADIO_Offset offset, int proc,
		 ADIO_Offset reg_max_len) {
    heap_node_t *nodes;
    int i;
    nodes = heap->nodes;
    i = ++heap->size - 1;
    while ((i > 0) && (nodes[parent(i)].offset > offset)) {
	nodes[i] = nodes[parent(i)];
	i = parent(i);
    }
    nodes[i].offset      = offset;
    nodes[i].proc        = proc;
    nodes[i].reg_max_len = reg_max_len;
}

void ADIOI_Heap_extract_min(heap_t *heap, ADIO_Offset* offset, int *proc,
		      ADIO_Offset *reg_max_len) {
    heap_node_t *nodes;
    nodes = heap->nodes;
    
    assert (heap->size > 0);
    *offset = nodes[0].offset;
    *proc = nodes[0].proc;
    *reg_max_len = nodes[0].reg_max_len;
    nodes[0] = nodes[heap->size-1];
    heap->size--;
    heapify(heap, 0);
}

/* should suppress unused warnings on GCC */
static void print_heap(heap_t *heap) ATTRIBUTE((unused, used));

static void print_heap(heap_t *heap)
{
#ifndef NOEXP2
    int i;
    double level = 0;
    int next_level_idx = 1;

    printf ("heap->size = %d\n", heap->size);
    printf ("offsets:\n");
    for (i=0; i < heap->size; i++) {
	printf ("%lld ", heap->nodes[i].offset);
	
	if ((i+1) == next_level_idx) {
	    printf ("\n");
	    next_level_idx += (int) exp2(level+1);
	    level++;
	}
    }
    printf ("\n");
#endif
}
