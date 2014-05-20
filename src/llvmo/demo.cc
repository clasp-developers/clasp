

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    printf("Hello world\n");

    char *x = (char*)malloc(10 * sizeof(char*));
    free(x);
    return x[5];
}
