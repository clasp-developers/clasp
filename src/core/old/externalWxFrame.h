/*
    File: externalWxFrame.h
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
#ifndef ExternalObject_H //[
#define ExternalObject_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"

namespace mbb {

__BEGIN_CLASS_DEFINITION(MbbPackage, O_ExternalObject, ExternalObject, O_Object)
public:
template <typename T>
static void expose(T cl) {
  cl
      .def("getMatter", &O_ExternalObject::getMatter)
      .def("numberOfTrajectoryFrames", &O_ExternalObject::numberOfTrajectoryFrames)
      .def("addFrame", &O_ExternalObject::addFrame)
      .def("getTrajectoryFrame", &O_ExternalObject::getTrajectoryFrame)
      .def("applyTrajectoryFrameToMatter", &O_ExternalObject::applyTrajectoryFrameToMatter);
}
static void exposeCando(RPLisp e);
static void exposePython();

public: // virtual functions inherited from Object
void initialize();
void lispInitialize(RPKeyedArguments kargs, RPLisp);
void archiveBase(RPNode node);
//	string	__repr__() const;

private: // instance variables
string _ExternalObjectName;
RPKeyedArguments _Arguments;

public:
O_ExternalObject(const O_ExternalObject &ss); //!< Copy constructor

__END_CLASS_DEFINITION(O_ExternalObject)
};
#endif //]
