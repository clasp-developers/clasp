#include "stdio.h"

template <typename T>
class smart_ptr {};

class A;
class B;
class C;
class D;

// Why don't I need to do the following?????
#if 0
template <> class smart_ptr<A>;
template <> class smart_ptr<B>;
template <> class smart_ptr<C>;
template <> class smart_ptr<smart_ptr<D>>;
#endif



template <typename T>
struct Info  {
    static const int Kind = 0;
};

template <> struct Info<smart_ptr<A>> { static const int Kind = 1;};
template <> struct Info<smart_ptr<B>> { static const int Kind = 2;};
template <> struct Info<smart_ptr<C>> { static const int Kind = 3;};
template <> struct Info<smart_ptr<smart_ptr<D>>> { static const int Kind = 4;};


template <> class smart_ptr<A>;

int main(int argc, const char* argv[] )
{
    printf("Info<smart_ptr<A>> = %d\n", Info<smart_ptr<A>>::Kind );
    printf("Info<smart_ptr<B>> = %d\n", Info<smart_ptr<B>>::Kind );
    printf("Info<smart_ptr<C>> = %d\n", Info<smart_ptr<C>>::Kind );
    printf("Info<smart_ptr<smart_ptr<D>>> = %d\n", Info<smart_ptr<smart_ptr<D>>>::Kind );
}
