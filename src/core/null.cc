/*
    File: null.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/environment.h>
#include <clasp/core/null.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//






#if 0
    void Null_O::serialize(::serialize::SNodeP node)
    {
	IMPLEMENT_ME();
        this->Bases::serialize(node);
	// Archive other instance variables here
    }

#endif
#if defined(XML_ARCHIVE)
void Null_O::archiveBase(::core::ArchiveP node) {
  this->T_O::archiveBase(node);
  this->Symbol_O::archiveBase(node);
  this->List_O::archiveBase(node);
}
#endif // defined(XML_ARCHIVE)

void Null_O::initialize() {
  _OF();
  //        this->T_O::initialize();
  IMPLEMENT_MEF(BF("Null needs to implement single inheritance"));
  this->Symbol_O::initialize();
  //	this->List_O::initialize();
}

string Null_O::__repr__() const {
  _OF();
  return "#<NULL--- NON-NULL instance of NULL!!!!!>";
}

}; /* core */
