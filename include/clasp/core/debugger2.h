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
