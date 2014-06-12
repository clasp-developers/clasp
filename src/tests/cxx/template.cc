#include "stdio.h"

template <typename T>
class smart_ptr {};

class B;
class C;
class D;


template <typename T>
struct Info  {
    static const int Kind = 0;
};

template <> struct Info<smart_ptr<B>> { static const int Kind = 2;};
template <> struct Info<smart_ptr<C>> { static const int Kind = 3;};
template <> struct Info<smart_ptr<smart_ptr<D>>> { static const int Kind = 4;};
// template <> struct Info<C::Sub> { static const int Kind=5; };  // <- This won't work - can't forward define nested classes

struct CSub; // forward declare CSub and then declare it to inherit from C::Sub

template <> struct Info<CSub> { static const int Kind = 5;};

class C {
public:
    struct Sub {
        Sub(int j) : x(j) {};
        int x;
    };
};

struct CSub : public C::Sub {
    CSub(int j) : C::Sub(j) {};
    CSub(const C::Sub& other) : C::Sub(other) {};
};

int main(int argc, const char* argv[] )
{
    printf("Info<smart_ptr<B>> = %d\n", Info<smart_ptr<B>>::Kind );
    printf("Info<smart_ptr<C>> = %d\n", Info<smart_ptr<C>>::Kind );
    printf("Info<smart_ptr<smart_ptr<D>>> = %d\n", Info<smart_ptr<smart_ptr<D>>>::Kind );
    printf("Info<CSub> = %d\n", Info<CSub>::Kind );

    C::Sub s(99);
    CSub ss(s); // & ss = static_cast<CSub&>(s);  // Why can I do this?  This is very useful but why does it work?
    printf(" ss.x = %d\n", ss.x);
}
