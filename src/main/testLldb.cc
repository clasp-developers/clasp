/*
    File: testLldb.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#define	DEBUG_LEVEL_FULL

#include <cstdint>
#include <stdio.h>


namespace mem
{



    template <class T> class smart_ptr;
    template <class T> class weak_smart_ptr;


    template <class T> inline bool isNilDowncastableTo() {return false;};

    void initialize_smart_pointers();

    template <class T>
    struct tagged_intrusive_ptr {
        tagged_intrusive_ptr(uintptr_t p) : px(reinterpret_cast<T*>(p)) {};
        T* px;
        static const uintptr_t tagged_NULL = 0;
        static const uintptr_t tagged_nil = 1;
        static const uintptr_t tagged_unbound = 2;
        T* get() { return px;};
    };

    template <class T>
	class smart_ptr : public tagged_intrusive_ptr<T>
    {
    public:
	typedef tagged_intrusive_ptr<T>	BaseType;
    public:
	typedef T Type;
	typedef T* PointerType;
	static const uintptr_t _NULL = BaseType::tagged_NULL;
	static const uintptr_t nil = BaseType::tagged_nil;
	static const uintptr_t unbound = BaseType::tagged_unbound;
    public:
	smart_ptr() : BaseType() {};
	explicit smart_ptr(uintptr_t p) : BaseType(p) {}; // TODO: this converts ints to smart_ptr's - its dangerous
	smart_ptr( T* objP) : BaseType(objP) {};
	smart_ptr(const smart_ptr<T>& obj) : BaseType(obj) {};
	smart_ptr(int f) : BaseType(f) {};
	template <class Y> smart_ptr(const smart_ptr<Y>& yy) : BaseType(yy) {} ; //.get()) {};
//	template <class Y> smart_ptr(const smart_ptr<const Y>& yy) : BaseType(/*const_cast<T*>*/(yy.pxget())) {} ; //.get()) {};
#if defined(USE_MPS)
	template <class Y>  smart_ptr(const mem::tagged_ptr<Y>& yy) : BaseType(yy) {};
#else
	template <class Y>  smart_ptr(const tagged_intrusive_ptr<Y>& yy) : BaseType(yy) {};
#endif

	template <class o_class>
	inline smart_ptr<o_class> pointerAsUnsafe()
	{
	    o_class* new_px = dynamic_cast<o_class*>(this->px);
	    return smart_ptr<o_class>(new_px);
	}

	template <class o_class>
	inline smart_ptr<o_class> pointerAsUnsafe() const
	{
	    o_class* new_px = dynamic_cast<o_class*>(this->px);
	    return smart_ptr<o_class>(new_px);
	}


        
    };
};

    namespace core {
        struct T_O {
        };

        typedef mem::smart_ptr<T_O>      T_sp;


    };




void test(core::T_sp n)
{
    printf("n.get() = @%p\n", n.get());
};

int main(int argc, char* argv[] )
{	// Do not touch debug log until after MPI init


    core::T_sp n = mem::smart_ptr<core::T_O>(mem::smart_ptr<core::T_O>::_NULL);

    test(n);
    return 0;
}
