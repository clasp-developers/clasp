#pragma once
/* -^- */

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

namespace core {

// TODO: Delete Frame_O, make this Frame_O
FORWARD(DebuggerFrame);
class

    DebuggerFrame_O : public General_O {
  LISP_CLASS(core, CorePkg, DebuggerFrame_O, "DebuggerFrame", General_O);

public:
  bool fieldsp() const override { return true; };
  void fields(Record_sp node) override;
  std::string __repr__() const;
  DebuggerFrame_O(T_sp a_fname, T_sp a_return_address, T_sp a_sp, T_sp a_fd, T_sp a_closure, T_sp a_args, bool a_av,
                  List_sp a_locals, T_sp a_lang, bool a_is_xep)
      : fname(a_fname), return_address(a_return_address), source_position(a_sp), function_description(a_fd), closure(a_closure),
        args(a_args), args_available(a_av), locals(a_locals), lang(a_lang), up(nil<T_O>()), down(nil<T_O>()), is_xep(a_is_xep) {}
  static DebuggerFrame_sp make(T_sp fname, T_sp ra, T_sp sp, T_sp fd, T_sp closure, T_sp args, bool args_available, List_sp locals,
                               T_sp lang, bool is_xep) {
    auto ret = gctools::GC<DebuggerFrame_O>::allocate(fname, ra, sp, fd, closure, args, args_available, locals, lang, is_xep);
    return ret;
  }
  T_sp returnAddress() const;

public:
  T_sp fname;
  T_sp return_address;
  T_sp source_position;
  T_sp function_description;
  T_sp closure;
  T_sp args;
  bool args_available;
  List_sp locals;
  T_sp lang;
  T_sp up;
  T_sp down;
  bool is_xep;
};

T_mv call_with_frame(std::function<T_mv(DebuggerFrame_sp)>);

FORWARD(DebuggerLocal);
class DebuggerLocal_O : public General_O {
  LISP_CLASS(core, CorePkg, DebuggerLocal_O, "DebuggerLocal", General_O);

public:
  DebuggerLocal_O(T_sp a_fname, T_sp a_name, T_sp a_declfile, T_sp a_declline)
      : fname(a_fname), name(a_name), declfile(a_declfile), declline(a_declline) {}
  static DebuggerLocal_sp make(T_sp fname, T_sp name, T_sp declfile, T_sp declline) {
    auto ret = gctools::GC<DebuggerLocal_O>::allocate(fname, name, declfile, declline);
    return ret;
  }

public:
  T_sp fname;
  T_sp name;
  T_sp declfile;
  T_sp declline;
};

}; // namespace core
namespace llvmo {
FORWARD(DWARFContext);
FORWARD(SectionedAddress);
}; // namespace llvmo

namespace core {

T_sp dwarf_ep(size_t frameIndex, llvmo::ObjectFile_sp ofi, llvmo::DWARFContext_sp dcontext, llvmo::SectionedAddress_sp sa,
              void*& codeStart, void*& functionStartAddress, bool& XEPp, int& arityCode);

} // namespace core
