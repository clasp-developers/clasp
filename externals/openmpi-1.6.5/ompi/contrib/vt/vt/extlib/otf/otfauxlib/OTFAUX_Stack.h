#ifndef OTFAUX_STACK_H
#define OTFAUX_STACK_H 1


#include <stdlib.h>


typedef struct Stack
{
    struct Stack* prev;
    struct Stack* next;
} Stack;


static void
stack_init( Stack* stack )
{
    stack->next = stack;
    stack->prev = stack;
}


static Stack*
stack_next( Stack* stack )
{
    return stack->next;
}


static Stack*
stack_prev( Stack* stack )
{
    return stack->prev;
}


static void
stack_remove( Stack* stack )
{
    stack->prev->next = stack->next;
    stack->next->prev = stack->prev;
    stack_init( stack );
}


static Stack*
stack_pop( Stack* stack )
{
    Stack* pop = stack->next;
    if ( pop == stack )
        return NULL;
    stack_remove( pop );
    return pop;
}


static int
stack_empty( Stack* stack )
{
    return stack == stack->next;
}


static void
stack_push( Stack* stack, Stack* entry )
{
    entry->next = stack->next;
    entry->prev = stack;
    stack->next->prev = entry;
    stack->next = entry;
}


static void
stack_add( Stack* stack, Stack* entry )
{
    entry->prev = stack->prev;
    entry->next = stack;
    stack->prev->next = entry;
    stack->prev = entry;
}


#endif /* OTFAUX_STACK_H */
