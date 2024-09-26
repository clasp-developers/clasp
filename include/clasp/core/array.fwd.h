#pragma once
/*
    File: array.fwd.h
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
/* @(#)array.fwd.h
 */

#include <clasp/core/coretypes.h> // FORWARD

namespace core {
FORWARD(AbstractSimpleVector);
FORWARD(SimpleString);
FORWARD(SimpleBaseString);
FORWARD(SimpleCharacterString);
FORWARD(StrNs);
FORWARD(Str8Ns);
FORWARD(StrWNs);
FORWARD(MDArrayT);
FORWARD(SimpleMDArrayT);
FORWARD(ComplexVector_T);
FORWARD(MDArrayBit);
FORWARD(SimpleMDArrayBit);
FORWARD(MDArrayBaseChar);
FORWARD(SimpleMDArrayBaseChar);
FORWARD(MDArrayCharacter);
FORWARD(SimpleMDArrayCharacter);
//
FORWARD(SimpleVector_short_float);
FORWARD(MDArray_short_float);
FORWARD(SimpleMDArray_short_float);
FORWARD(ComplexVector_short_float);
//
FORWARD(SimpleVector_double);
FORWARD(MDArray_double);
FORWARD(SimpleMDArray_double);
FORWARD(ComplexVector_double);
//
FORWARD(SimpleVector_long_float);
FORWARD(MDArray_long_float);
FORWARD(SimpleMDArray_long_float);
FORWARD(ComplexVector_long_float);
//
FORWARD(SimpleVector_size_t);
FORWARD(MDArray_size_t);
FORWARD(SimpleMDArray_size_t);
}; // namespace core

namespace core {
// ------------------------------------------------------------
//  Convert 8-bit strings

/*! Access functions to avoid having to include the entire class */
string string_get_std_string(T_sp str);
string string_get_std_string(String_sp str);

/*! Create a SimpleBaseString_O object */
SimpleBaseString_sp str_create(const string& val);

/*! Create a SimpleBaseString_O object from a const char* */
SimpleBaseString_sp str_create(const char* val);

}; // namespace core
