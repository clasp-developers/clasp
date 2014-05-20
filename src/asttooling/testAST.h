#ifndef testAST_H
#define testAST_H

#if 0
namespace asttooling {

    template <class T> class Foo : public gctools::GCObject {};


    template<> class Foo<int> : public gctools::GCObject {};

    class FooBar /*: public gctools::GCObject */ {
        struct metadata_test {};
        struct metadata_Another_Test;
    public:
        core::T_sp field;
        std::ofstream out;
        FooBar(core::T_sp f) : field(f) {};

        virtual ~FooBar() {
//            printf("In dtor\n");
//            printf("In dtor2\n");
        };
    };




};
#endif


#endif
