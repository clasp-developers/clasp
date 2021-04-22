/* -^- */
#ifndef debugger2_H
#define debugger2_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/arguments.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/pathname.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/myReadLine.h>
#include <clasp/core/symbol.h>
#include <clasp/core/stacks.h>

namespace core {

// TODO: Delete Frame_O, make this Frame_O
FORWARD(DebuggerFrame);
class DebuggerFrame_O : public General_O {
  LISP_CLASS(core, CorePkg, DebuggerFrame_O, "DebuggerFrame", General_O);
  virtual ~DebuggerFrame_O() {};
public:
  DebuggerFrame_O(T_sp a_fname, T_sp a_sp, T_sp a_fd, T_sp a_lang)
    : fname(a_fname), source_position(a_sp), function_description(a_fd),
      lang(a_lang), up(_Nil<T_O>()), down(_Nil<T_O>())
  {}
  static DebuggerFrame_sp make(T_sp fname, T_sp sp, T_sp fd, T_sp lang) {
    GC_ALLOCATE_VARIADIC(DebuggerFrame_O, ret, fname, sp, fd, lang);
    return ret;
  }
public:
  T_sp fname;
  T_sp source_position;
  T_sp function_description;
  T_sp lang;
  T_sp up;
  T_sp down;
};

FORWARD(DebuggerLocal);
class DebuggerLocal_O : public General_O {
  LISP_CLASS(core, CorePkg, DebuggerLocal_O, "DebuggerLocal", General_O);
  virtual ~DebuggerLocal_O() {};
public:
  DebuggerLocal_O(T_sp a_fname, T_sp a_name, T_sp a_declfile, T_sp a_declline)
    : fname(a_fname), name(a_name), declfile(a_declfile), declline(a_declline)
  {}
  static DebuggerLocal_sp make(T_sp fname, T_sp name,
                               T_sp declfile, T_sp declline) {
    GC_ALLOCATE_VARIADIC(DebuggerLocal_O, ret,
                         fname, name, declfile, declline);
    return ret;
  }
public:
  T_sp fname;
  T_sp name;
  T_sp declfile;
  T_sp declline;
};
} // namespace core

#endif // guard
