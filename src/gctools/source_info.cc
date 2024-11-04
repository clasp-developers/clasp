#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbol.h>
#include <clasp/core/numbers.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/documentation.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/array.h>
#include <clasp/core/bundle.h>

typedef enum { code_kind, setf_kind, method_kind, class_kind, variable_kind, unknown_kind } source_info_kind;

NOINLINE void define_source_info(source_info_kind kind, const string& lisp_name, const string& file, size_t character_offset,
                                 size_t line, const string& docstring) {
  std::string package_part, symbol_part;
  core::colon_split(lisp_name, package_part, symbol_part);
  core::Symbol_sp sym = core::lisp_intern(symbol_part, package_part);
  core::SimpleBaseString_sp sourceFile = core::SimpleBaseString_O::make(file);
  core::SimpleBaseString_sp docs;
  core::Function_sp func;
  bool gotdocs = false;

  if (docstring != "") {
    docs = core::SimpleBaseString_O::make(docstring);
    gotdocs = true;
  }

  switch (kind) {
  case code_kind:
  case setf_kind:
    func = (kind == code_kind) ? sym->symbolFunction() : sym->getSetfFdefinition();
    func->setSourcePosInfo(sourceFile, character_offset, line, 0);
    if (gotdocs) {
      func->setf_docstring(docs);
    }
    break;
  case method_kind:
    core::core__put_sysprop(
        core::Cons_O::create(core::Cons_O::createList(sourceFile, core::clasp_make_fixnum((Fixnum)character_offset)),
                             core__get_sysprop(sym, core::_sym_cxx_method_source_location)),
        sym, core::_sym_cxx_method_source_location);
    if (gotdocs) {
      ext__annotate(sym, cl::_sym_documentation, cl::_sym_method, docs);
    }
    break;
  case class_kind:
    core::core__put_sysprop(core::Cons_O::createList(sourceFile, core::clasp_make_fixnum((Fixnum)character_offset)),
                            sym, core::_sym_class_source_location);
    if (gotdocs) {
      gc::As<core::Instance_sp>(core::cl__find_class(sym))->instanceSet(core::Instance_O::REF_CLASS_DOCSTRING, docs);
    }
    break;
  case variable_kind:
    printf("%s:%d Handle setting source location of variable_kind\n", __FILE__, __LINE__);
    break;
  default:
    printf("%s:%d Could not set source-location for %d\n", __FILE__, __LINE__, kind);
    break;
  }
}

NOINLINE void define_pathname_translation(const string& from, const string& to) {
  core::T_sp host = core::SimpleBaseString_O::make("SYS");
  core::cl__setf_logical_pathname_translations(
      core::Cons_O::create(core::Cons_O::createList(
                               core::cl__pathname(core::SimpleBaseString_O::make(from)),
                               core::cl__pathname(core::SimpleBaseString_O::make(globals_->_Bundle->_Directories->_SysDir / to))),
                           core::cl__logical_pathname_translations(gc::As_assert<core::String_sp>(host))),
      gc::As_assert<core::String_sp>(host));
}

#define SOURCE_INFO_HELPERS
#undef SOURCE_INFO
#ifndef SCRAPING
#include SOURCE_INFO_INC_H
#endif
#undef SOURCE_INFO_HELPERS

void initialize_source_info(){
#define SOURCE_INFO
#ifndef SCRAPING
#include SOURCE_INFO_INC_H
#endif
#undef SOURCE_INFO
};
