#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <functional>


class __attribute__((weak)) MyException {
    virtual void test() __attribute__((weak));
public:
    __attribute__((weak)) MyException(int i) : _val(i) {};
    int _val;
    __attribute__((weak)) virtual ~MyException() {};
};


void MyException::test() {};

extern void foo()
{
    printf("Hi - I'm foo\n");
}




int main(int argc, char* argv[])
{
    try {
        throw MyException(1);
    } catch (MyException& e)
    {
        printf("Caught MyException\n");
    }

    // Here I would load a plugin that expects __ZTI11MyException to be external
    // but __ZTI11MyException is non-external so the exception handling will terminate
    //

    // Load_plugin_that_would_catch_MyException

}


#if 0

	clang++ -fvisibility=default -std=c++11 -stdlib=libc++  -o testExport testExport.cc

fry:cxx$ nm -mapv testExport | grep MyException
0000000100000db0 (__TEXT,__text) non-external (was a private external) __ZN11MyExceptionC1Ei
0000000100000e00 (__TEXT,__text) non-external (was a private external) __ZN11MyExceptionC2Ei
0000000100001070 (__DATA,__data) non-external (was a private external) __ZTI11MyException
0000000100000f34 (__TEXT,__const) weak external __ZTS11MyException
fry:cxx$ nm -mapv testExport | grep MyException | c++filt
0000000100000db0 (__TEXT,__text) non-external (was signed char private external) MyException::MyException(int)
0000000100000e00 (__TEXT,__text) non-external (was signed char private external) MyException::MyException(int)
0000000100001070 (__DATA,__data) non-external (was signed char private external) typeinfo for MyException
0000000100000f34 (__TEXT,__const) weak external typeinfo name for MyException

#endif
