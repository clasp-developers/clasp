/*
    File: lispString.h
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
#ifndef	_core_String_H
#define _core_String_H

#include "core/foundation.h"
#include "core/object.h"
#include "lispVector.h"

namespace core
{

FORWARD(String);
class String_O : public Vector_O
{
    LISP_BASE1(Vector_O);
    LISP_CLASS(core,ClPkg,String_O,"String");




    DECLARE_INIT();
#if defined(XML_ARCHIVE)
    DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
public:
    explicit String_O() : Base() {};
    virtual ~String_O() {};
public:
	void initialize();

private: // instance variables here


public: // Functions here


    virtual int pushCharExtend(claspChar c, int extension = 0) {SUBIMP(); };
    virtual int fillPointer() const {SUBIMP();};


};

}; /* core */

TRANSLATE(core::String_O);



namespace core {

    Str_sp af_string(T_sp str);


    Str_sp af_string_upcase(T_sp arg);
    Str_sp af_string_downcase(T_sp arg);


    claspChar af_char(T_sp str, int idx);

    bool brcl_memberChar(claspChar c, T_sp charBag);

    Str_sp cl_stringTrim(T_sp charbag, T_sp str);
    Str_sp cl_stringLeftTrim(T_sp charbag, T_sp str);
    Str_sp cl_stringRightTrim(T_sp charbag, T_sp str);
};
#endif /* _core_String_H */
