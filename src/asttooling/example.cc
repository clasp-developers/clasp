
#include <memory>
#include <core/foundation.h>
#include <core/evaluator.h>
#include <clbind/clbind.h>
#include <asttooling/asttoolingPackage.h>
#include <asttooling/symbolTable.h>




namespace asttooling {
    class A {
    public:
        A() {};
        void a1() { printf("A::a1\n");};
        void a2() { printf("A::a2\n");};
        void dox() { this->x();};
        virtual void x() {printf("A::x\n");};
        virtual ~A() {};
    };

    class B : public A {
    public:
        B() {};
        void b1() { printf("B::b1\n");};
        virtual void x() {printf("B::x\n");};
        virtual ~B() {};
    };

    class C : public B {
    public:
        C() {};
        void c1() { printf("C::c1\n");};
        virtual void x() {printf("C::x\n");};
        virtual ~C() {};
    };


    class B_Adapter : public B, public clbind::Adapter<B_Adapter> {
    public:
        B_Adapter() : B(), clbind::Adapter<B_Adapter>("B_Adapter") {};
        SYMBOL_EXPORT_SC_(AstToolingPkg,x);
        virtual void x() {core::eval::funcall(_sym_x,this->wrapper_from_this());};
        void default_x() {this->B::x();};
        virtual ~B_Adapter() {};
    };


    void initialize_example()
    {
        using namespace clbind;
        package(AstToolingPkg) [
            class_<A>("A")
            .def_constructor("make-a",constructor<>())
            .def("a1",&A::a1)
            .def("a2",&A::a2)
            .def("dox",&A::dox)
            ,
            class_<B,bases<A> >("B")
            .def_constructor("make-b",constructor<>())
            .def("b1",&B::b1)
            ,
            class_<C,bases<B> >("C")
            .def_constructor("make-c",constructor<>())
            .def("c1",&C::c1)
            ,
            class_<B_Adapter,B >("B-Adapter")
            .def("x",&B_Adapter::default_x)
            ];

    };

};


typedef clbind::Wrapper<asttooling::A> A_wrapper;
typedef clbind::Wrapper<asttooling::B> B_wrapper;
typedef clbind::Wrapper<asttooling::C> C_wrapper;
typedef clbind::Wrapper<asttooling::B_Adapter> B_Adapter_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(A_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(B_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(B_Adapter_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(C_wrapper);

