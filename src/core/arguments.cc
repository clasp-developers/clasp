/*
    File: arguments.cc
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
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/arguments.h>

namespace core {

List_sp Argument::lambda_list() const {
  return ((this->_ArgTarget));
};

Symbol_sp Argument::symbol() const {
  return ((gc::As<Symbol_sp>(this->_ArgTarget)));
};

List_sp Argument::classified() const {
  if (this->_ArgTargetFrameIndex == SPECIAL_TARGET) {
    return coerce_to_list(Cons_O::create(ext::_sym_specialVar, this->_ArgTarget));
  } else if (this->_ArgTargetFrameIndex >= 0) {
    return coerce_to_list(Cons_O::create(ext::_sym_lexicalVar, Cons_O::create(this->_ArgTarget, make_fixnum(this->_ArgTargetFrameIndex))));
  } else if (this->_ArgTargetFrameIndex == UNDEFINED_TARGET) {
    return ((nil<List_V>()));
  }
  SIMPLE_ERROR("Illegal target");
}

string Argument::asString() const {
  stringstream ss;
  ss << "#<Argument ";
  ss << ":target ";
  ss << _rep_(this->_ArgTarget);
  ss << " :tfi ";
  ss << this->_ArgTargetFrameIndex;
  ss << " >  ";
  return ((ss.str()));
}

string ArgumentWithDefault::asString() const {
  stringstream ss;
  ss << "#<ArgumentWithDefault ";
  ss << ":target ";
  ss << _rep_(this->_ArgTarget);
  ss << " :tfi ";
  ss << this->_ArgTargetFrameIndex;
  ss << " :default ";
  ss << _rep_(this->_Default);
  ss << " >  ";
  return ((ss.str()));
}

string RequiredArgument::asString() const {
  stringstream ss;
  ss << "#<RequiredArgument ";
  ss << ":target ";
  this->Base::asString();
  ss << " >  ";
  return ((ss.str()));
}

string OptionalArgument::asString() const {
  stringstream ss;
  ss << "#<OptionalArgument ";
  ss << ":target ";
  ss << this->Base::asString();
  if (this->_Sensor.isDefined()) {
    ss << " :sensor ";
    ss << this->_Sensor.asString();
  }
  ss << " >  ";
  return ((ss.str()));
}

string RestArgument::asString() const {
  stringstream ss;
  ss << "#<RestArgument ";
  ss << ":target ";
  ss << _rep_(this->_ArgTarget);
  ss << " :tfi ";
  ss << this->_ArgTargetFrameIndex;
  ss << " >  ";
  return ((ss.str()));
}

string KeywordArgument::asString() const {
  stringstream ss;
  ss << "#<KeywordArgument ";
  ss << ":keyword " << _rep_(this->_Keyword);
  ss << " :target ";
  ss << this->Base::asString();
  if (this->_Sensor.isDefined()) {
    ss << " :sensor ";
    ss << this->_Sensor.asString();
  }
  ss << " >  ";
  return ((ss.str()));
}

string AuxArgument::asString() const {
  stringstream ss;
  ss << "#<AuxArgument ";
  ss << ":target ";
  this->Base::asString();
  ss << " :expression ";
  ss << _rep_(this->_Expression);
  ss << " >  ";
  return ((ss.str()));
}

};
