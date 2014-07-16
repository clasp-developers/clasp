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

int bar3(int n_args, int x, int y, int z, ...)
{
    va_list ap;
    va_start(ap,z);
    int w = va_arg(ap,int);
    va_end(ap);
    return x + y + z + w;
}

int bar1(int n_args, int x, ...)
{
    va_list ap;
    va_start(ap,x);
    int y = va_arg(ap,int);
    int z = va_arg(ap,int);
    int w = va_arg(ap,int);
    va_end(ap);
    return x + y + z + w;
}


int main(int argc, char* argv[])
{
    if ( argc != 3 ) {
        printf("Usage:   %s {num} (foo|bar1|bar3)\n", argv[0] );
        exit(1);
    }
    int steps = atoi(argv[1]);
    printf("Starting with %d\n", steps);
    if ( strcmp(argv[2],"foo") == 0 ) {
	printf("Running test with foo - complete varargs\n");
	for ( int i=0; i<100*steps; i++ )
	{
	    foo(4,1,2,3,4,5);
	}
    } else if (strcmp(argv[2],"bar3") == 0 ) {
	printf("Running test with bar3 - prefix 3 fixed arguments\n");
	for ( int i=0; i<100*steps; i++ )
	{
	    bar3(4,1,2,3,4,5);
	}
    } else {
	printf("Running test with bar1 - prefix 1 fixed arguments\n");
	for ( int i=0; i<100*steps; i++ )
	{
	    bar1(4,1,2,3,4,5);
	}
    }
    printf("Done\n");
}

    
