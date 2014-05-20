#include <stdio.h>
#include <cstring>

#define GC_SCAN_ARGS_PROTOTYPE int


#include "vectorCore.h"


int main(int argc, const char* argv[])
{
    printf("sizeof myvector<void*> = %lu\n", sizeof(myvector<void*>));
    printf("sizeof myvector<int> = %lu\n", sizeof(myvector<int>));
}


