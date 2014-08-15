#include <stdio.h>


int main(int argc, const char* argv[])
{
    printf("Starting\n");
    int r = fseek(stdin,0,SEEK_END);
    printf("Done r = %d\n", r);
};
        
