/*
    File: regex.cc
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
#undef USEBOOSTPYTHON // currently including regex.h runs into problems with boost python

#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/regex.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

CL_LAMBDA(regex-str);
CL_DECLARE();
CL_DOCSTRING("makeRegex");
CL_DEFUN Regex_sp core__make_regex(const string &str) {
  Regex_sp regex = Regex_O::make(str);
  return regex;
};



  SYMBOL_EXPORT_SC_(CorePkg, makeRegex);




Regex_sp Regex_O::make(const string &regex) {
  GC_ALLOCATE(Regex_O, re);
  re->_Regex = regex;
  return re;
}

#ifdef XML_ARCHIVE
void Regex_O::archiveBase(core::ArchiveP node) {
  this->Base::archiveBase(node);
  // Archive other instance variables here
  IMPLEMENT_ME();
}
#endif

void Regex_O::initialize() {
  _OF();
  this->Base::initialize();
}

CL_LISPIFY_NAME("regexMatches");
CL_DEFMETHOD bool Regex_O::regexMatches(const string &str) const {
  _OF();
  return boost::regex_match(str, this->_Regex);
}

CL_LISPIFY_NAME("regexMatch");
CL_DEFMETHOD RegexMatch_sp Regex_O::regexMatch(const string &str) const {
  _OF();
  GC_ALLOCATE(RegexMatch_O, match);
  match->_CopyOfTextToMatch = str;
  boost::regex_match(match->_CopyOfTextToMatch.data(), match->_Match, this->_Regex);
  return match;
}

CL_LISPIFY_NAME("regexSedReplace");
CL_DEFMETHOD string Regex_O::regexSedReplace(const string &str, const string &replace) const {
  _OF();
  return boost::regex_replace(str, this->_Regex, replace,
                              boost::match_default | boost::format_sed);
}

CL_LAMBDA(regex-match &optional (idx 0));
CL_LISPIFY_NAME("regex-match-matched");
CL_DEFMETHOD bool RegexMatch_O::matched(int idx) const {
  ASSERTF(idx < (int)this->_Match.size(), BF("index[%d] exceeded max[%d]") % idx % this->_Match.size());
  return this->_Match[idx].matched;
}






void RegexMatch_O::initialize() {
  _OF();
  this->Base::initialize();
}

CL_LISPIFY_NAME("regex-match-length");
CL_DEFMETHOD int RegexMatch_O::size() const {
  _OF();
  return this->_Match.size();
}

CL_LISPIFY_NAME("regex-match-part");
CL_DEFMETHOD string RegexMatch_O::part(int idx) const {
  _OF();
  ASSERTF(idx < (int)this->_Match.size(), BF("index[%d] exceeded max[%d]") % idx % this->_Match.size());
  string result = "";
  if (this->_Match[idx].matched) {
    result.assign(this->_Match[idx].first, this->_Match[idx].second);
    //            printf("%s:%d - RegexMatch_O::part %d = [%s]\n", __FILE__,__LINE__,idx,result.c_str());
  }
  return result;
}

}; /* core */
