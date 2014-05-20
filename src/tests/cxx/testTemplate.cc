#include <stdio.h>




template <typename 



template <int X, typename P, typename TUPLE, class... ARGS>
void select(ARGS...args)
{
    select<X+1,Y,TUPLE,ARGS...>(args...);
}


template <unsigned X, int X, typename TUPLE, class HEAD, class... ARGS>
void select(HEAD head, ARGS...args)
{
    printf("Selected %d\n", head);
    select<X+1,TUPLE::Head, TUPLE::Tail, ARGS...>(args...);
}


int main(int argc, char* argv[])
{
    select<1,1,mv_<1,3,5,9> >(1,2,3,4,5,6,7,8,9,10,11);
}
