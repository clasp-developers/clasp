#include <stdio.h>

typedef int result_T;

extern "C" {
    result_T scan(void* ptr);
};

namespace core {

    class Foo {
        friend result_T ::scan(void* ptr);
    private:
        int x;
    public:
        Foo(int z) : x(z) {};
    };
}

void cxxscan(void* ptr)
{
};

extern "C" {
    result_T scan(void* ptr)
    {
        core::Foo* foo = reinterpret_cast<core::Foo*>(ptr);
        printf("The private member variable is %d\n", foo->x );
        return 1;
    }
};

int main(int argc, const char* argv[])
{
    printf("Starting\n");
    core::Foo u(10);
    void* ptr = (void*)&u;
    scan(ptr);
};
        

#if 0
fry:cxx$ clang++ testFriend.cc
testFriend.cc:12:16: error: expected a class or namespace
        friend res_t ::scan(void* ptr);
               ^
testFriend.cc:28:60: error: 'x' is a private member of 'core::Foo'
        printf("The private member variable is %d\n", foo->x );
                                                           ^
testFriend.cc:14:13: note: declared private here
        int x;
            ^
2 errors generated.
fry:cxx$ 
#endif
