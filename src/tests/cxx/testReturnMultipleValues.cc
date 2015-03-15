#include <stdio.h>

struct SV {
    SV(void* ptr) : p(ptr) {};
    void* p;
};

struct MV : public SV {
    MV(void* ptr1, int nv) : SV(ptr1), n(nv) {};
    int n;
};

MV foo(void* ptr1, int n)
{
    MV res(ptr1,n);
    return res;
}


int main(int argc, char* argv[])
{
    MV x(NULL,1);
    x.p = NULL;
    x.n = 1;
};
    
