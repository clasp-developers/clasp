#include <iostream>
#include <string>
#include <sstream>

using namespace std;


struct A {
    int _val;
    A(int x) : _val(x) {};
    int val() { return _val; };
};

template <typename T>
struct wrapped_ptr {
    wrapped_ptr() : px(NULL) {};
    wrapped_ptr(T* p) : px(p) {};


    T*  px;

//    T* operator->() { return this->px;};
};


template <typename T>
struct smart_ptr : public wrapped_ptr<T> {
    smart_ptr()     : wrapped_ptr<T>(NULL) {};
    smart_ptr(T* p) : wrapped_ptr<T>(p) {};

    T* operator->() { return this->px;};
};


void print(smart_ptr<A> v)
{
    printf("Int = %d\n", v->val());
}

int main(int argc, char* argv[])
{
    printf("Starting\n");
    smart_ptr<A> x = new A(5);
    print(x);
};
