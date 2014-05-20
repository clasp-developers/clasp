/* Copyright Vladimir Prus 2004. Distributed under the Boost */
/* Software License, Version 1.0. (See accompanying */
/* file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt) */

#include "../native.h"
#include "../lists.h"
#include "../strings.h"
#include "../newstr.h"
#include "../variable.h"


/* Use quite klugy approach: when we add order dependency from 'a' to 'b',
   just append 'b' to of value of variable 'a'.
*/
LIST *add_pair( PARSE *parse, FRAME *frame )
{
    LIST* arg = lol_get( frame->args, 0 );    

    var_set(arg->string, list_copy(0, arg->next), VAR_APPEND);

    return L0;
}

/** Given a list and a value, returns position of that value in
    the list, or -1 if not found.
*/
int list_index(LIST* list, const char* value)
{
    int result = 0;
    for(; list; list = list->next, ++result) {
        if (strcmp(list->string, value) == 0)
            return result;
    }
    return -1;
}

enum colors { white, gray, black };

/* Main routite of topological sort. Calls itself recursively on all
   adjacent vertices which were not yet visited. After that, 'current_vertex'
   is added to '*result_ptr'.
*/
void do_ts(int** graph, int current_vertex, int* colors, int** result_ptr)
{
    int i;

    colors[current_vertex] = gray;
    for(i = 0; graph[current_vertex][i] != -1; ++i) {
        int adjacent_vertex = graph[current_vertex][i];

        if (colors[adjacent_vertex] == white)
            do_ts(graph, adjacent_vertex, colors, result_ptr);
        else if (colors[adjacent_vertex] == gray)
            ; /* This is loop. Not sure what to do... */
    }
    colors[current_vertex] = black;
    **result_ptr = current_vertex;    
    (*result_ptr)++;
}

void topological_sort(int** graph, int num_vertices, int* result)
{
    int i;
    int* colors = (int*)BJAM_CALLOC(num_vertices, sizeof(int));
    for (i = 0; i < num_vertices; ++i)
        colors[i] = white;

    for(i = 0; i < num_vertices; ++i)
        if (colors[i] == white)
            do_ts(graph, i, colors, &result);

    BJAM_FREE(colors);
}

LIST *order( PARSE *parse, FRAME *frame )
{
    LIST* arg = lol_get( frame->args, 0 );  
    LIST* tmp;
    LIST* result = 0;
    int src;

    /* We need to create a graph of order dependencies between
       the passed objects. We assume that there are no duplicates
       passed to 'add_pair'.
    */
    int length = list_length(arg);
    int** graph = (int**)BJAM_CALLOC(length, sizeof(int*));
    int* order = (int*)BJAM_MALLOC((length+1)*sizeof(int));
   
    for(tmp = arg, src = 0; tmp; tmp = tmp->next, ++src) {
        /* For all object this one depend upon, add elements
           to 'graph' */
        LIST* dependencies = var_get(tmp->string);
        int index = 0;

        graph[src] = (int*)BJAM_CALLOC(list_length(dependencies)+1, sizeof(int));
        for(; dependencies; dependencies = dependencies->next) {          
            int dst = list_index(arg, dependencies->string);
            if (dst != -1)
                graph[src][index++] = dst;
        }
        graph[src][index] = -1;               
    }

    topological_sort(graph, length, order);

    {
        int index = length-1;
        for(; index >= 0; --index) {
            int i;
            tmp = arg;
            for (i = 0; i < order[index]; ++i, tmp = tmp->next);
            result = list_new(result, tmp->string);
        }
    }

    /* Clean up */
    {
        int i;
        for(i = 0; i < length; ++i)
            BJAM_FREE(graph[i]);
        BJAM_FREE(graph);
        BJAM_FREE(order);
    }

    return result;
}

void init_order()
{
    {
        char* args[] = { "first", "second", 0 };
        declare_native_rule("class@order", "add-pair", args, add_pair, 1);
    }

    {
        char* args[] = { "objects", "*", 0 };
        declare_native_rule("class@order", "order", args, order, 1);
    }


}
