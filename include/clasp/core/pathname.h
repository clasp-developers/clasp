/*
    File: pathname.h
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
#ifndef core_pathname_H //[
#define core_pathname_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/numbers.fwd.h>
#include <clasp/core/sourceFileInfo.fwd.h>
#include <clasp/core/object.h>

namespace kw {
extern core::Symbol_sp& _sym_unspecific;
extern core::Symbol_sp& _sym_local;
extern core::Symbol_sp& _sym_newest;
}

namespace cl {
extern core::Symbol_sp& _sym_STARdefaultPathnameDefaultsSTAR;
};

namespace core {

FORWARD(Pathname);
Pathname_sp cl__pathname(T_sp x);
 Pathname_sp core__safe_default_pathname_defaults();
 Pathname_sp core__safe_default_pathname_defaults_host_only();
 Pathname_sp cl__merge_pathnames(T_sp arg, T_sp defaultPathname = core__safe_default_pathname_defaults(), T_sp defaultVersion = kw::_sym_newest);

T_mv cl__parse_namestring(T_sp thing,
                          T_sp host = _Nil<T_O>(),
                          T_sp defaultPathname = core__safe_default_pathname_defaults(),
                          Fixnum_sp start = make_fixnum(0),
                          T_sp end = _Nil<T_O>(),
                          bool junkAllowed = false);

T_sp cl__pathname_host(T_sp pathname, Symbol_sp acase);
T_sp cl__pathname_device(T_sp pathname, Symbol_sp acase);
T_sp cl__pathname_directory(T_sp pathname, Symbol_sp acase = kw::_sym_local);
T_sp cl__pathname_name(T_sp pathname, Symbol_sp acase);
T_sp cl__pathname_type(T_sp pathname, Symbol_sp acase);
T_sp cl__pathname_version(T_sp pathname);

Pathname_sp cl__translate_logical_pathname(T_sp x);

bool cl__wild_pathname_p(T_sp pathname, T_sp component);

Pathname_sp core__coerce_to_physical_pathname(T_sp x);
String_sp core__coerce_to_filename(T_sp pathname_orig);
Pathname_sp core__coerce_to_file_pathname(T_sp tpathname);

};

namespace core {

SMART(Pathname);
class Pathname_O : public General_O {
  LISP_CLASS(core, ClPkg, Pathname_O, "pathname",General_O);

  friend bool cl__wild_pathname_p(T_sp tpathname, T_sp component);
  friend Pathname_sp core__coerce_to_physical_pathname(T_sp x);
  friend Pathname_sp core__coerce_to_file_pathname(T_sp x);
  friend String_sp core__coerce_to_filename(T_sp pathname);
  friend T_sp clasp_namestring(T_sp tx, int flags);
  friend Pathname_mv cl__parse_namestring(T_sp thing, T_sp host, T_sp defaults, Fixnum_sp start, Fixnum_sp end, bool junk_allowed);
  friend Pathname_sp cl__make_pathname(T_sp host, bool hostp, T_sp device, bool devicep, T_sp directory, bool directoryp, T_sp name, bool namep, T_sp type, bool typep, T_sp version, bool versionp, T_sp scase, T_sp defaults);
  friend T_sp cl__pathname_host(T_sp tpname, Symbol_sp scase);
  friend T_sp cl__pathname_device(T_sp tpname, Symbol_sp scase);
  friend T_sp cl__pathname_directory(T_sp tpname, Symbol_sp scase);
  friend T_sp cl__pathname_name(T_sp tpname, Symbol_sp scase);
  friend T_sp cl__pathname_type(T_sp tpname, Symbol_sp scase);
  friend T_sp cl__pathname_version(T_sp tpname);
  friend T_sp cl__file_namestring(T_sp tpname);
  friend T_sp cl__directory_namestring(T_sp tpname);
  friend T_sp cl__host_namestring(T_sp tpname);
  friend T_sp cl__enough_namestring(T_sp tpath, T_sp tdefaults);
  friend bool cl__pathname_match_p(T_sp tpath, T_sp tmask);
  friend Pathname_sp cl__translate_pathname(T_sp tsource, T_sp tfrom, T_sp tto, T_sp scase);
  friend Pathname_sp cl__translate_logical_pathname(T_sp tsource);

public:
  T_sp _Host;
  T_sp _Device;
  T_sp _Directory;
  T_sp _Name;
  T_sp _Type;
  T_sp _Version;

public:
  /*! Returns either a Pathname_sp or LogicalPathname_sp depending on host */
  static Pathname_sp makePathname(T_sp host, T_sp device, T_sp directory,
                                  T_sp name, T_sp type, T_sp version=_Nil<core::T_O>(),
                                  T_sp fromcase=kw::_sym_local, bool logical = false);

public:
  static Pathname_sp tilde_expand(Pathname_sp pathname);

public:
  Pathname_O(const Pathname_O &ss); //!< Copy constructor

  Pathname_O() : _Host(kw::_sym_unspecific),
                 _Device(kw::_sym_unspecific),
                 _Directory(_Nil<T_O>()),
                 _Name(_Nil<T_O>()),
                 _Type(_Nil<T_O>()),
                 _Version(kw::_sym_unspecific){};

  virtual ~Pathname_O(){};

  virtual bool equal(T_sp obj) const;
  virtual bool equalp(T_sp obj) const { return this->equal(obj); };
  virtual void sxhash_(HashGenerator &hg) const;
  virtual void sxhash_equal(HashGenerator &hg) const;
  virtual void sxhash_equalp(HashGenerator &hg) const;
  
  virtual string __repr__() const;
  //! Common Lisp __write__(T_sp strm)
  virtual void __write__(T_sp strm) const;
  virtual void __writeReadable__(T_sp strm) const;
};
};

namespace core {
SMART(LogicalPathname);
class LogicalPathname_O : public Pathname_O {
  LISP_CLASS(core, ClPkg, LogicalPathname_O, "logical-pathname",Pathname_O);

public:
  LogicalPathname_O(const LogicalPathname_O &ss); //!< Copy constructor
  LogicalPathname_O(){};
  virtual ~LogicalPathname_O(){};
};
};

namespace core {
bool clasp_stringMatch(T_sp s, size_t j, size_t ls,
                       T_sp p, size_t i, size_t lp);
bool clasp_logical_hostname_p(T_sp host);
bool clasp_wild_string_p(T_sp item);
T_sp clasp_namestring(T_sp x, int flags);
Pathname_sp clasp_mergePathnames(T_sp path, T_sp def, T_sp defaultVersion);

bool cl__pathname_match_p(T_sp path, T_sp mask);
T_sp cl__namestring(T_sp x);
T_sp cl__file_namestring(T_sp tpname);
T_sp cl__directory_namestring(T_sp tpname);
T_sp cl__host_namestring(T_sp tpname);
T_sp cl__enough_namestring(T_sp tpath, T_sp tdefaults);

T_sp core__pathname_translations(T_sp host, T_sp hostp, T_sp set);

/* If you want to call makePathname use:
		Pathname_sp backupPathname = af_makePathname(_Nil<T_O>(), // host 
							     false, // hostp 
							     _Nil<T_O>(), // device 
							     false, // devicep 
							     _Nil<T_O>(), // directory 
							     false, // directoryp 
							     _Nil<T_O>(), // name 
							     false, // namep 
							     <string>, // string
							     true, // typep 
							     _Nil<T_O>(), // version 
							     false, // versionp 
							     kw::_sym_local, // scase 
							     this->_OriginalPathname, // defaults 
							     )
    */
Pathname_sp cl__make_pathname(T_sp host, bool hostp,
                            T_sp device, bool devicep,
                            T_sp directory, bool directoryp,
                            T_sp name, bool namep,
                            T_sp type, bool typep,
                            T_sp version, bool versionp,
                            T_sp scase = kw::_sym_local,
                              T_sp defaults = core__safe_default_pathname_defaults_host_only());
};

#endif //]
