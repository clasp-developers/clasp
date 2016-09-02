#include <clasp/core/foundation.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/documentation.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/str.h>

typedef enum { code_kind, method_kind, class_kind, variable_kind, unknown_kind } source_info_kind;
NOINLINE void define_source_info(source_info_kind kind,
                        const string& lisp_name,
                        const string& file, size_t character_offset,
                        size_t line, const string& docstring ) {
  std::string package_part, symbol_part;
  core::colon_split(lisp_name,package_part,symbol_part);
  core::Symbol_sp sym = core::lisp_intern(symbol_part,package_part);
  core::Str_sp sourceFile = core::Str_O::create(file);
  core::Str_sp docs = core::Str_O::create(docstring);
  if ( kind == code_kind ) {
    core::Function_sp func = core::coerce::functionDesignator(sym);
    func->setSourcePosInfo(sourceFile, character_offset, line, 0 );
    ext__annotate(sym,cl::_sym_documentation,cl::_sym_function, docs);
    ext__annotate(func,cl::_sym_documentation,cl::_sym_function, docs);
  } else if ( kind == method_kind ) {
    core::List_sp info = core__get_sysprop(sym,core::_sym_cxx_method_source_location);
    info = core::Cons_O::create(core::Cons_O::createList(sourceFile,core::clasp_make_fixnum(character_offset)),info);
    core::core__put_sysprop(sym,core::_sym_cxx_method_source_location,info);
    core::Str_sp docs = core::Str_O::create(docstring);
    ext__annotate(sym,cl::_sym_documentation,cl::_sym_method, docs);
  } else if ( kind == class_kind ) {
    core::List_sp info = core::Cons_O::createList(sourceFile,core::clasp_make_fixnum(character_offset));
    core::core__put_sysprop(sym,core::_sym_class_source_location,info);
    ext__annotate(sym,cl::_sym_documentation,cl::_sym_class, docs);
  } else if ( kind == variable_kind ) {
    printf("%s:%d Handle setting source location of variable_kind\n", __FILE__, __LINE__ );
  } else {
    printf("%s:%d Could not set source-location for %d\n", __FILE__, __LINE__, kind);
  }
}

#define SOURCE_INFO_HELPERS
#undef SOURCE_INFO
#ifndef SCRAPING
#include SOURCE_INFO_INC_H
#endif
#undef SOURCE_INFO_HELPERS

void initialize_source_info() {
#define SOURCE_INFO
#ifndef SCRAPING
#include SOURCE_INFO_INC_H
#endif
#undef SOURCE_INFO
};

