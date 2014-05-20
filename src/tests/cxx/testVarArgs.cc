#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

int foo(int n_args, ...)
{
    va_list ap;
    va_start(ap,n_args);
    int x = va_arg(ap,int);
    int y = va_arg(ap,int);
    int z = va_arg(ap,int);
    int w = va_arg(ap,int);
    va_end(ap);
    return x + y + z + w;
}

int bar(int n_args, int x, int y, int z, ...)
{
    va_list ap;
    va_start(ap,z);
    int w = va_arg(ap,int);
    va_end(ap);
    return x + y + z + w;
}


int main(int argc, char* argv[])
{
    int steps = atoi(argv[1]);
    if ( strcmp(argv[2],"foo") == 0 ) {
	printf("Running test with foo - complete varargs\n");
	for ( int i=0; i<100*steps; i++ )
	{
	    foo(4,1,2,3,4);
	}
    } else {
	printf("Running test with bar - prefix fixed arguments\n");
	for ( int i=0; i<100*steps; i++ )
	{
	    bar(4,1,2,3,4);
	}
    }
    printf("Done\n");
}

    
