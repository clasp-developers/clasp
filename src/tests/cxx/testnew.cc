#include <stdlib.h>
#include <stdio.h>


struct A {
    int x;
    void* operator new(size_t s) {
        printf("Don't use me EVER\n");
        exit(1);
    }
};

int main(int argc, char* argv[])
{
    A* a = reinterpret_cast<A*>(malloc(sizeof(A)));
    new(a) A(10);
    printf("A.x = %d\n", a->x);
};
