/*
    File: cproto.cc
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


#include <stdio.h>

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/symbolTable.h>

typedef void (*afFunctionPtrType)(core::T_sp* resultP, core::ActivationFrame_sp* afP);

extern "C"
{
extern core::ActivationFrame_sp* activationFrameNil();
extern void invokeLlvmFunction(core::T_sp* resultP, afFunctionPtrType fn, core::ActivationFrame_sp* afP);
    extern void makeNil(core::T_sp* resultP);
    extern void throwCatchThrow(core::T_sp* tag, core::T_sp* val);
    extern void throwReturnFrom(int depth, core::T_sp* val);

class Tstruct
{
public:
    int x;
    Tstruct() {};
    virtual ~Tstruct() {};
};



    struct errA {};
    struct errB {};
    struct errC {};

    struct X { int i; X(int z) throw(): i(z){};};
    struct Y { int j; Y(int z) throw(): j(z){};};
    
    void cproto_complex0()
    {
	try
	{
	    X x(1);
	    printf("x=%d\n", x.i);
	    try
	    {
		try
		{
		Y y(2);
		printf("y=%d\n", y.j);
		throw errC();
		} catch (errA&) {}
	    } catch (errB& err) {}
	} catch (errC& err) {}
    }
		    

    int cproto_jumpTable(int index)
    {
	switch (index)
	{
	case 0:
	    goto c0;
	    break;
	case 1:
	    goto c1;
	    break;
	case 2:
	    goto c2;
	case 3:
	    goto c3;
	case 4:
	    goto c4;
	}

    c0: return 0;
    c1: return 1;
    c2: return 2;
    c3: return 3;
    c4: return 4;
    }
	    
	

void cproto_returnFrom(core::T_sp* resultP, core::ActivationFrame_sp* afP)
{
    printf("In repl\n");
//    core::T_sp val;
//    makeNil(&val);
    makeNil(resultP);
    Tstruct xx;
    throwReturnFrom(2,resultP);
}





void cproto_try_catch(core::T_sp* resultP, core::ActivationFrame_sp* afP)
{
    printf("In repl\n");
//    core::T_sp val;
//    makeNil(&val);
    makeNil(resultP);
    Tstruct xx;
    throwCatchThrow(resultP,resultP);

}

void ___main___(core::T_sp* resultP, core::ActivationFrame_sp* afP)
{
    printf("Running c-function ___main___\n");
    core::ActivationFrame_sp* afNilP = activationFrameNil();
    invokeLlvmFunction(resultP,&cproto_try_catch,afNilP);
    makeNil(resultP);
}

};
