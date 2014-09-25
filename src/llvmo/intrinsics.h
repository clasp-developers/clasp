/*
    File: intrinsics.h
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
#ifndef	llvmo_primitives_H
#define llvmo_primitives_H

extern "C"
{

    typedef void (*fnLispCallingConvention)(LISP_CALLING_CONVENTION_RETURN, LISP_CALLING_CONVENTION_CLOSED_ENVIRONMENT, LISP_CALLING_CONVENTION_ARGS );
    typedef void (*fnVoidType)();

#if 0
    class  JITClosure : public core::FunctionClosure
    {
    protected:
	fnLispCallingConvention	_FuncPtr;
    public:
	// By default the zeroth argument is dispatched on
	explicit JITClosure(core::T_sp name,core::T_sp closedEnv,fnLispCallingConvention fptr) : FunctionClosure(name,closedEnv), _FuncPtr(fptr) {};
        DISABLE_NEW();
        virtual size_t templatedSizeof() const { return sizeof(*this);};
	virtual string describe() const {return "JITClosure";};
	void LISP_CALLING_CONVENTION()
	{
            IMPLEMENT_MEF(BF("Handle LISP_CALLING_CONVENTION"));
#if 0
	    core::T_mv result;
	    core::ActivationFrame_sp tclosedEnv(closedEnv);
	    this->_FuncPtr(&result,&tclosedEnv,nargs, args);
	    return result;
#endif
	}
    };
#endif


};




namespace llvmo
{

    void redirect_llvm_interface_addSymbol();

    void initialize_intrinsics();
}

#endif
