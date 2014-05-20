#include <stdio.h>


namespace core {

struct T_sp {};

};


namespace translate {

    struct dont_adopt_pointer {};

    template <typename T>
    struct test {
    };

    template <>
    struct test<bool*> {
        static void print() {printf("Works\n");};
    };


    template <class oClass, class AdoptPolicy=dont_adopt_pointer> struct to_object
    {								
    };


    template <typename T>
    class to_object<T&,translate::dont_adopt_pointer> {
    public:
        static core::T_sp convert(T& val) {
            int*** wrongSpecialization = T(); printf("%p\n", wrongSpecialization);
            return core::T_sp();
        }
    };


    template <>
    struct to_object<bool*,translate::dont_adopt_pointer>
    {
        typedef bool* GivenType;
        static core::T_sp convert(GivenType v)
        {
//            int*** i = GivenType();
            test<bool*>::print();
            return core::T_sp();
        }
    };

};


int main(int argc, const char* argv[] )
{
    bool b(true);
    int i;
    translate::to_object<bool*>::convert(&b);
    translate::to_object<int&>::convert(i);
}
