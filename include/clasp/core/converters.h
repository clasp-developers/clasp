/*
    File: converters.h
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
#ifndef converters_H
#define converters_H

// ------------------------------------------------------------
//
//  Type conversion from T_sp to T_sp
//
//
//
template <typename T_return>
struct from_object {
  typedef T_return ExpectedType;
  typedef T_return DeclareType;
#if 0
    static ExpectedType convert(core::T_sp o)
    {_G();
	stringstream ss;
	ss << "Called generic from_object for (";
#ifdef __GNUG__
	size_t len;
	int s;
	char* demangled = abi::__cxa_demangle(typeid(T_return).name(),0,&len,&s);
	ss << demangled;
#else
	ss << typeid(T_return).name();
#endif
	ss << ") should not be invoked, define a more specific one";
	SIMPLE_ERROR_SPRINTF("%s", ss.str().c_str() );
    }
#endif
};

#endif // converters_H
