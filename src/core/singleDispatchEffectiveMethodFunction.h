/*
    File: singleDispatchEffectiveMethodFunction.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
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
#ifndef _core_singleDispatchEffectiveMethodFunction_H_
#define _core_singleDispatchEffectiveMethodFunction_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/executables.h"
#include "core/singleDispatchEffectiveMethodFunction.fwd.h"



namespace core
{
    class SingleDispatchEffectiveMethodFunction_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(core,CorePkg,SingleDispatchEffectiveMethodFunction_O,"SingleDispatchEffectiveMethodFunction");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(SingleDispatchEffectiveMethodFunction_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit SingleDispatchEffectiveMethodFunction_O(core::Class_sp const& mc) : T_O(mc), T(mc) {};
//    virtual ~SingleDispatchEffectiveMethodFunction_O() {};
    public:
	static SingleDispatchEffectiveMethodFunction_sp create(Cons_sp methods,Lisp_sp lisp);
    public:
	void initialize();
	
    GCPRIVATE: // instance variables here
	Cons_sp	_Methods;

    private:
	void LISP_INVOKE();
	
    public: // Functions here

	string __repr__() const;


    }; // SingleDispatchEffectiveMethodFunction class
    
}; // core namespace
TRANSLATE(core::SingleDispatchEffectiveMethodFunction_O);





#endif // _core_singleDispatchEffectiveMethodFunction_H_
