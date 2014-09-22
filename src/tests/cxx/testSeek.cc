#include <stdio.h>
#include <utility>

int main(int argc, const char* argv[])
{
    int i = 1;
    int j = std::move(i);

    printf("Starting\n");
    int r = fseek(stdin,0,SEEK_END);
    printf("Done r = %d\n", r);
};
        
