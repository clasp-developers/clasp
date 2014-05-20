#include "stdio.h"

int a(int x)
{
    printf( "x = %d\n", x);
    return x;
}


int main(int argc, char* argv[])
{
    int x(1);
    {
	int y(2);
	if ( 1 ) {
	    printf("This is a test %d %d\n", x, y);
	}
	a(1);
    }
}
