/*
    File: regex.h
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
#ifndef _core_Regex_H
#define _core_Regex_H

#include <clasp/core/useBoostRegex.h>

#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(RegexMatch);
class RegexMatch_O : public core::General_O {
  friend class Regex_O;
  LISP_CLASS(core, CorePkg, RegexMatch_O, "RegexMatch",core::General_O);
  DEFAULT_CTOR_DTOR(RegexMatch_O);

public:
  void initialize();

private:                     // instance variables here
  string _CopyOfTextToMatch; // _Match only has meaning as long as string exists
  boost::cmatch _Match;

public: // Functions here
  /*! Return the number of matches */
  int size() const;
  /*! Return the captures that matched
	  - index 0 is the entire string and the subsequent
	  indices are the individual captures */
  string part(int idx) const;

  /*! Return true if the part was part of the match */
  bool matched(int idx) const;

  /*! Return the prefix of the match */
CL_LISPIFY_NAME("regex-match-prefix");
CL_DEFMETHOD   string prefix() const { return this->_Match.prefix(); };

  /*! Return the suffix of the match */
CL_LISPIFY_NAME("regex-match-suffix");
CL_DEFMETHOD   string suffix() const { return this->_Match.suffix(); };
};

FORWARD(Regex);
class Regex_O : public core::General_O {
  LISP_CLASS(core, CorePkg, Regex_O, "Regex",core::General_O);
  //    DECLARE_ARCHIVE();
  DEFAULT_CTOR_DTOR(Regex_O);

public:
  void initialize();

private: // instance variables here
  boost::regex _Regex;

public:
  static Regex_sp make(const string &str);

public: // Functions here
  bool regexMatches(const string &str) const;

  RegexMatch_sp regexMatch(const string &str) const;

  /*! Return a new string that is replaced using "sed"-like syntax */
  string regexSedReplace(const string &str, const string &replace) const;
};

}; /* core */

TRANSLATE(core::Regex_O);
TRANSLATE(core::RegexMatch_O);

#endif /* _core_Regex_H */
