/*
    File: clangTooling.cc
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

#include <clang/AST/DeclTemplate.h>
#include <clang/AST/DeclFriend.h>
#include <clang/AST/DeclOpenMP.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>
#include <clang/AST/ExprObjC.h>
#include <clang/AST/StmtObjC.h>
#include <clang/AST/StmtOpenMP.h>
#include <clang/Basic/Version.h>
#include <clang/Tooling/JSONCompilationDatabase.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/Comment.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Tooling/Refactoring.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Lex/Lexer.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/ASTMatchers/Dynamic/VariantValue.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/translators.h>
#include <clasp/core/str.h>
#include <clasp/core/arguments.h>
#include <clasp/clbind/clbind.h>
#include <clasp/llvmo/translators.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/asttooling/astExpose.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/asttooling/translators.h>
#include <clasp/asttooling/symbolTable.h>
#include <clasp/core/wrappers.h>
#include <clasp/asttooling/Diagnostics.h>
#include <clasp/asttooling/Registry.h>
#include <clasp/asttooling/clangTooling.h>
#include <clasp/core/translators.h>

#ifdef USE_MPS
#define NAMESPACE_clbind_clang
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_clbind_clang
#endif

namespace translate {

template <>
struct from_object<clang::tooling::CommandLineArguments> {
  typedef clang::tooling::CommandLineArguments DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.notnilp()) {
      if (core::Cons_sp cargs = o.asOrNull<core::Cons_O>()) {
        core::List_sp args = cargs;
        _v.clear();
        for (auto cur : args) {
          core::Str_sp s = gc::As<core::Str_sp>(oCar(cur));
          _v.push_back(s->get());
        }
        return;
      } else if (core::Vector_sp vargs = o.asOrNull<core::Vector_O>()) {
        _v.clear();
        for (int i(0), iEnd(vargs->length()); i < iEnd; ++i) {
          core::Str_sp s = gc::As<core::Str_sp>((*vargs)[i]);
          _v.push_back(s->get());
        }
        return;
      }
    }
    SIMPLE_ERROR(BF("Conversion of %s to clang::tooling::CommandLineArguments not supported yet") % _rep_(o));
  }
};

#if 0
    // This is not necessary because CommandLineArguments are just vector<string>
        template <>
    struct from_object<const clang::tooling::CommandLineArguments&> {
        typedef clang::tooling::CommandLineArguments DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) {
	    if ( o.notnilp() ) {
		if ( core::Cons_sp args = o.asOrNull<core::Cons_O>() ) {
		    _v.clear();
		    for ( core::Cons_sp cur = args; cur.notnilp(); cur=cCdr(cur) ) {
			core::Str_sp s = oCar(cur).as<core::Str_O>();
			_v.push_back(s->get());
		    }
		    return;
		} else if ( core::Vector_sp vargs = o.asOrNull<core::Vector_O>() ) {
                    _v.clear();
                    for ( int i(0), iEnd(vargs->length()); i<iEnd; ++i ) {
                        core::Str_sp s = (*vargs)[i].as<core::Str_O>();
                        _v.push_back(s->get());
                    }
                    return;
		}
	    }
	    SIMPLE_ERROR(BF("Conversion of %s to clang::tooling::CommandLineArguments not supported yet") % _rep_(o) );
	}
    };
#endif

template <>
struct from_object<clang::tooling::ArgumentsAdjuster> {
  typedef clang::tooling::ArgumentsAdjuster DeclareType;
  //	clang::tooling::CommandLineArguments(const clang::tooling::CommandLineArguments&)> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    printf("%s:%d Entered from_object<clang::tooling::ArgumentsAdjuster>\n", __FILE__, __LINE__);
    if (o.nilp()) {
      SIMPLE_ERROR(BF("You cannot pass nil as a function"));
    } else if (core::Function_sp func = o.asOrNull<core::Function_O>()) {
      gctools::tagged_pointer<core::Closure> closure = func->closure;
      if (auto compiledClosure = closure.asOrNull<llvmo::CompiledClosure>()) {
        core::CompiledClosure_fptr_type fptr = compiledClosure->fptr;
        this->_v = [fptr](const clang::tooling::CommandLineArguments &args) -> clang::tooling::CommandLineArguments {
			// Should resolve to vector<string>
			core::T_sp targs = translate::to_object<clang::tooling::CommandLineArguments>::convert(args);
			core::T_mv result;
			// Call the fptr
                        STACK_FRAME(buff,onearg,1);
                        onearg[0] = targs.raw_();
                        core::VaList_S onearg_valist_s(onearg);
                        core::T_O* lcc_arglist = onearg_valist_s.asTaggedPtr();
			result = fptr(LCC_PASS_ARGS1_VA_LIST(targs.raw_()));
			// Should resolve to const vector<string>& 
			translate::from_object<const clang::tooling::CommandLineArguments&> cresult(result);
			return cresult._v;
          // Convert args to CL object
          // Call fptr
          // Convert result to CommandLineArguments and return them
        };
        return;
      } else {
        SIMPLE_ERROR(BF("Figure out what to do with the %s Closure %s ") % closure->describe() % _rep_(closure->name));
      }
    } else if (clang::tooling::ArgumentsAdjuster *argAdj = gc::As<core::WrappedPointer_sp>(o)->cast<clang::tooling::ArgumentsAdjuster>()) {
      this->_v = *argAdj;
      return;
    }
    SIMPLE_ERROR(BF("Cannot convert %s into a clang::tooling::ArgumentsAdjuster") % _rep_(o));
  }
};
};

namespace asttooling {

using namespace clbind;

SYMBOL_EXPORT_SC_(AstToolingPkg, ArgumentsAdjusterAdjust);
SYMBOL_EXPORT_SC_(AstToolingPkg, bind);
SYMBOL_EXPORT_SC_(AstToolingPkg, VisitStmt);
SYMBOL_EXPORT_SC_(AstToolingPkg, VisitDecl);
SYMBOL_EXPORT_SC_(AstToolingPkg, VisitType);
SYMBOL_EXPORT_SC_(AstToolingPkg, HandleTranslationUnit);
SYMBOL_EXPORT_SC_(AstToolingPkg, CreateASTConsumer);
SYMBOL_EXPORT_SC_(AstToolingPkg, create);
SYMBOL_EXPORT_SC_(AstToolingPkg, run);
SYMBOL_EXPORT_SC_(AstToolingPkg, onStartOfTranslationUnit);
SYMBOL_EXPORT_SC_(AstToolingPkg, onEndOfTranslationUnit);
};

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::comments::Comment>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::comments::FullComment>);

typedef clbind::Wrapper<clang::tooling::CompilationDatabase> CompilationDatabase_wrapper;
typedef clbind::Wrapper<clang::tooling::JSONCompilationDatabase> JSONCompilationDatabase_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(CompilationDatabase_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(JSONCompilationDatabase_wrapper);
typedef clbind::Wrapper<clang::tooling::ClangTool> ClangTool_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(ClangTool_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::tooling::RefactoringTool>);

typedef clbind::Wrapper<clang::FrontendAction> FrontendAction_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(FrontendAction_wrapper);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::tooling::FrontendActionFactory>);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ASTConsumer>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::CompilerInstance>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<const clang::LangOptions>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<const clang::SourceManager>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::SourceManager>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::SourceLocation>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::PresumedLoc>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::SourceRange>);

typedef clbind::Wrapper<clang::SourceLocation, std::unique_ptr<clang::SourceLocation>> SourceLocation_unique_ptr_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(SourceLocation_unique_ptr_wrapper)
typedef clbind::Wrapper<clang::PresumedLoc, std::unique_ptr<clang::PresumedLoc>> PresumedLoc_unique_ptr_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(PresumedLoc_unique_ptr_wrapper)

typedef clbind::Wrapper<clang::SourceRange, std::unique_ptr<clang::SourceRange>> SourceRange_unique_ptr_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(SourceRange_unique_ptr_wrapper)

typedef clbind::Wrapper<clang::CharSourceRange, std::unique_ptr<clang::CharSourceRange>> CharSourceRange_unique_ptr_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(CharSourceRange_unique_ptr_wrapper);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::Lexer>);

typedef clbind::Wrapper<clang::tooling::ArgumentsAdjuster> ArgumentsAdjuster_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(ArgumentsAdjuster_wrapper);

//typedef clbind::Wrapper<clang::tooling::ClangSyntaxOnlyAdjuster> ClangSyntaxOnlyAdjuster_wrapper;
//INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(ClangSyntaxOnlyAdjuster_wrapper);
//typedef clbind::Wrapper<clang::tooling::ClangStripOutputAdjuster> ClangStripOutputAdjuster_wrapper;
//INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(ClangStripOutputAdjuster_wrapper);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<asttooling::Diagnostics>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<asttooling::ParserValue>);

typedef clbind::Wrapper<clang::ast_matchers::dynamic::VariantValue, std::unique_ptr<clang::ast_matchers::dynamic::VariantValue>> VariantValue_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(VariantValue_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::dynamic::VariantValue>);

typedef clbind::Wrapper<clang::ast_matchers::dynamic::VariantMatcher, std::unique_ptr<clang::ast_matchers::dynamic::VariantMatcher>> VariantMatcher_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(VariantMatcher_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::dynamic::VariantMatcher>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::MatchFinder>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::internal::DynTypedMatcher const>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::internal::DynTypedMatcher>);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::MatchFinder::MatchCallback>);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::MatchFinder::MatchResult>);
//INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::BoundNodes>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::BoundNodes const>);
typedef clbind::Wrapper<clang::ast_matchers::BoundNodes, std::unique_ptr<clang::ast_matchers::BoundNodes>> BoundNodes_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(BoundNodes_wrapper);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ast_matchers::MatchFinder::MatchResult const>);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::ASTUnit>);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::tooling::Replacement>);

typedef clbind::Wrapper<clang::tooling::Replacement, std::unique_ptr<clang::tooling::Replacement>> Replacement_unique_ptr_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Replacement_unique_ptr_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::tooling::Range>);
typedef clbind::Wrapper<clang::tooling::Range, std::unique_ptr<clang::tooling::Range>> Range_unique_ptr_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Range_unique_ptr_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::Rewriter>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<clang::tooling::Replacements>);

typedef clbind::Wrapper<clang::tooling::CompileCommand, std::unique_ptr<clang::tooling::CompileCommand>> CompileCommand_unique_ptr_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(CompileCommand_unique_ptr_wrapper);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(asttooling::DerivableArgumentsAdjuster);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(asttooling::DerivableMatchCallback);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(asttooling::DerivableASTFrontendAction);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(asttooling::DerivableSyntaxOnlyAction);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(asttooling::DerivableFrontendActionFactory);

namespace asttooling {

#define ARGS_af_clangVersionString "()"
#define DECL_af_clangVersionString ""
#define DOCS_af_clangVersionString "clangVersionString"
core::Str_sp af_clangVersionString() {
  _G();
  core::Str_sp version = core::Str_O::create(CLANG_VERSION_STRING);
  return version;
};

#define ARGS_af_getSingleMatcher "(variant-matcher)"
#define DECL_af_getSingleMatcher ""
#define DOCS_af_getSingleMatcher "getSingleMatcher"
core::T_sp af_getSingleMatcher(core::T_sp variantMatcher) {
  _G();
  clang::ast_matchers::dynamic::VariantMatcher *vp = gc::As<core::WrappedPointer_sp>(variantMatcher)->cast<clang::ast_matchers::dynamic::VariantMatcher>();
  llvm::Optional<clang::ast_matchers::internal::DynTypedMatcher> dtm = vp->getSingleMatcher();
  if (dtm.hasValue()) {
    return clbind::Wrapper<clang::ast_matchers::internal::DynTypedMatcher>::create(*dtm, reg::registered_class<clang::ast_matchers::internal::DynTypedMatcher>::id);
  }
  return _Nil<core::T_O>();
};

#define ARGS_af_IDToNodeMap "(bound-nodes)"
#define DECL_af_IDToNodeMap ""
#define DOCS_af_IDToNodeMap "IDToNodeMap - returns a HashTable of bound keyword symbols to wrapped nodes"
core::HashTable_sp af_IDToNodeMap(core::T_sp bn) {
  _G();
  if (const clang::ast_matchers::BoundNodes *boundNodes = gc::As<core::WrappedPointer_sp>(bn)->cast<const clang::ast_matchers::BoundNodes>()) {
    core::HashTable_sp ht = core::HashTable_O::create(::cl::_sym_eq);
    const clang::ast_matchers::BoundNodes::IDToNodeMap &nodemap = boundNodes->getMap();
    clang::ast_matchers::BoundNodes::IDToNodeMap::const_iterator it;
    for (it = nodemap.begin(); it != nodemap.end(); it++) {
      const string &key = it->first;
      const clang::ast_type_traits::DynTypedNode &dtn = it->second;
      core::T_sp value;
      if (const clang::Decl *decl = dtn.get<clang::Decl>()) {
        value = mostDerivedDecl(decl);
      } else if (const clang::Stmt *stmt = dtn.get<clang::Stmt>()) {
        value = mostDerivedStmt(stmt);
      } else if (const clang::Type *type = dtn.get<clang::Type>()) {
        value = mostDerivedType(type);
      } else if (const clang::QualType *qtype = dtn.get<clang::QualType>()) {
        value = translate::to_object<clang::QualType>::convert(*qtype);
      } else if (const clang::TypeLoc *typeLocType = dtn.get<clang::TypeLoc>()) {
        value = translate::to_object<clang::TypeLoc>::convert(*typeLocType);
      } else {
        SIMPLE_ERROR(BF("%s:%d Handle boxing of other node types in af_IDToNodeMap") % __FILE__ % __LINE__);
      }
      ht->hash_table_setf_gethash(_lisp->internKeyword(key), value);
    }
    return ht;
  }
  SIMPLE_ERROR(BF("Wrong argument type for IDToNodeMap"));
}

#define ARGS_af_match "(match-finder node ast-context)"
#define DECL_af_match ""
#define DOCS_af_match "Run the MATCH-FINDER on the NODE. This will handle any kind of clang ast node."
void af_match(core::T_sp tmatchFinder, core::T_sp tnode, core::T_sp tastContext) {
  _G();
  clang::ast_matchers::MatchFinder *matchFinder = gc::As<core::WrappedPointer_sp>(tmatchFinder)->cast<clang::ast_matchers::MatchFinder>();
  clang::ASTContext *astContext = gc::As<core::WrappedPointer_sp>(tastContext)->cast<clang::ASTContext>();
  core::WrappedPointer_sp wp_node = gc::As<core::WrappedPointer_sp>(tnode);
  if (clang::Decl *decl = wp_node->castOrNull<clang::Decl>()) {
    matchFinder->match(*decl, *astContext);
  } else if (clang::Stmt *stmt = wp_node->castOrNull<clang::Stmt>()) {
    matchFinder->match(*stmt, *astContext);
  } else if (clang::Type *type = wp_node->castOrNull<clang::Type>()) {
    matchFinder->match(*type, *astContext);
  } else if (clang::QualType *qtype = wp_node->castOrNull<clang::QualType>()) {
    matchFinder->match(*qtype, *astContext);
  } else if (clang::TypeLoc *typeloc = wp_node->castOrNull<clang::TypeLoc>()) {
    matchFinder->match(*typeloc, *astContext);
  } else {
    SIMPLE_ERROR(BF("%s:%d Handle unboxing of other node types in af_match") % __FILE__ % __LINE__);
  }
};

#define ARGS_af_newFrontendActionFactory "(consumer-factory)"
#define DECL_af_newFrontendActionFactory ""
#define DOCS_af_newFrontendActionFactory "newFrontendActionFactory"
core::T_sp af_newFrontendActionFactory(core::T_sp consumerFactory) {
  _G();
  if (clang::ast_matchers::MatchFinder *matchFinder = gc::As<core::WrappedPointer_sp>(consumerFactory)->cast<clang::ast_matchers::MatchFinder>()) {
    typedef clbind::Wrapper<clang::tooling::FrontendActionFactory, std::unique_ptr<clang::tooling::FrontendActionFactory>> wrapped_type;
    std::unique_ptr<clang::tooling::FrontendActionFactory> val = clang::tooling::newFrontendActionFactory(matchFinder);
#if 0
	    clang::tooling::FrontendActionFactory* fafptr = val.release();
	    gctools::smart_ptr<wrapped_type> sp(wrapped_type::create(fafptr,reg::registered_class<clang::tooling::FrontendActionFactory>::id));
#else
    gctools::smart_ptr<wrapped_type> sp(wrapped_type::create(std::move(val), reg::registered_class<clang::tooling::FrontendActionFactory>::id));
#endif
    return sp;
  }
  SIMPLE_ERROR(BF("Implement newFrontendActionFactory for %s") % _rep_(consumerFactory));
};

#define ARGS_af_Replacements_insert "(replacement)"
#define DECL_af_Replacements_insert ""
#define DOCS_af_Replacements_insert "Replacements_insert - try to insert the Replacement, return true if successful"
bool af_Replacements_insert(clang::tooling::Replacements &replacements, const clang::tooling::Replacement &one) {
  _G();
  pair<clang::tooling::Replacements::iterator, bool> res = replacements.insert(one);
  return res.second;
};

#define ARGS_af_deduplicate "(replacements)"
#define DECL_af_deduplicate ""
#define DOCS_af_deduplicate "deduplicate wraps and lispifys clang::tooling::deduplicate - it takes a Cons of replacements and returns (values replacements overlapping-ranges)"
core::T_mv af_deduplicate(core::List_sp replacements) {
  _G();
  core::List_sp creps = replacements;
  vector<clang::tooling::Replacement> vreps;
  for (; creps.notnilp(); creps = oCdr(creps)) {
    core::T_sp one = oCar(creps);
    clang::tooling::Replacement oneRep = *(gc::As<core::WrappedPointer_sp>(one)->cast<clang::tooling::Replacement>());
    vreps.push_back(oneRep);
  }
  vector<clang::tooling::Range> vranges;
  clang::tooling::deduplicate(vreps, vranges);
  core::Cons_sp firstRep = core::Cons_O::create();
  core::Cons_sp curRep = firstRep;
  for (auto i : vreps) {
    clang::tooling::Replacement *rp = new clang::tooling::Replacement(i);
    core::T_sp wrapRep = clbind::Wrapper<clang::tooling::Replacement, std::unique_ptr<clang::tooling::Replacement>>::create(rp, reg::registered_class<clang::tooling::Replacement>::id);
    core::Cons_sp oneRepCons = core::Cons_O::create(wrapRep);
    curRep->setCdr(oneRepCons);
    curRep = oneRepCons;
  }
  core::Cons_sp firstRang = core::Cons_O::create();
  core::Cons_sp curRang = firstRang;
  for (auto j : vranges) {
    // Why does Range not have a Copy constructor?????
    clang::tooling::Range *rp = new clang::tooling::Range(j); //i.getOffset(),i.getLength());
    core::T_sp wrapRang = clbind::Wrapper<clang::tooling::Range, std::unique_ptr<clang::tooling::Range>>::create(rp, reg::registered_class<clang::tooling::Range>::id);
    core::Cons_sp oneRangCons = core::Cons_O::create(wrapRang);
    curRang->setCdr(oneRangCons);
    curRang = oneRangCons;
  }
  return Values(oCdr(firstRep), oCdr(firstRang));
}

#define ARGS_af_testDerivable "(obj)"
#define DECL_af_testDerivable ""
#define DOCS_af_testDerivable "testDerivable"
void af_testDerivable(clang::ast_matchers::MatchFinder::MatchCallback *ptr) {
  _G();
  printf("%s:%d - got DerivableMatchCallback object --> %p\n", __FILE__, __LINE__, ptr);
  ptr->onEndOfTranslationUnit();
};
};

namespace asttooling {

void initialize_clangTooling() {

  // overloaded functions that had trouble resolving
  clang::ASTContext &(clang::ASTUnit::*clang_ASTUnit_getASTContext)() = &clang::ASTUnit::getASTContext;
  package(AstToolingPkg, {"CLANG"}, {"CL", "CORE", "AST-TOOLING"})[
    class_<clang::tooling::CompilationDatabase>("CompilationDatabase", no_default_constructor)
        .def("getAllFiles", &clang::tooling::CompilationDatabase::getAllFiles)
        .def("getCompileCommands", &clang::tooling::CompilationDatabase::getCompileCommands)
        .def("getAllCompileCommands", &clang::tooling::CompilationDatabase::getAllCompileCommands),
    class_<clang::tooling::JSONCompilationDatabase, bases<clang::tooling::CompilationDatabase>>("JSONCompilationDatabase", no_default_constructor),
    def("JSONCompilationDatabase-loadFromFile",
        &clang::tooling::JSONCompilationDatabase::loadFromFile,
        policies<adopt<result>, pureOutValue<2>>()),
    class_<clang::ASTConsumer>("Clang-ASTConsumer", no_default_constructor),
    class_<clang::LangOptions>("LangOptions", no_default_constructor),
    class_<clang::Lexer>("Lexer", no_default_constructor),
    class_<clang::Preprocessor>("Preprocessor", no_default_constructor),
    class_<clang::ASTContext>("ASTContext", no_default_constructor)
        .def("getTranslationUnitDecl", &clang::ASTContext::getTranslationUnitDecl)
        .def("getLangOpts", &clang::ASTContext::getLangOpts)
        .def("getCommentForDecl", &clang::ASTContext::getCommentForDecl),
    class_<clang::SourceManager>("SourceManager", no_default_constructor)
        .def("getPresumedLoc", &clang::SourceManager::getPresumedLoc, policies<>(), "((self ast-tooling:source-manager) source-location &optional (use-line-directives t))")
        .def("getFilename", &clang::SourceManager::getFilename)
        .def("getExpansionLoc", &clang::SourceManager::getExpansionLoc)
        .def("getExpansionLineNumber", &clang::SourceManager::getExpansionLineNumber, policies<pureOutValue<2>>())
        .def("getExpansionColumnNumber", &clang::SourceManager::getExpansionColumnNumber, policies<pureOutValue<2>>())
        .def("getSpellingLoc", &clang::SourceManager::getSpellingLoc)
        .def("getSpellingLineNumber", &clang::SourceManager::getSpellingLineNumber, policies<pureOutValue<2>>())
        .def("getSpellingColumnNumber", &clang::SourceManager::getSpellingColumnNumber, policies<pureOutValue<2>>())
        .def("getPresumedLineNumber", &clang::SourceManager::getPresumedLineNumber, policies<pureOutValue<2>>())
        .def("getPresumedColumnNumber", &clang::SourceManager::getPresumedColumnNumber, policies<pureOutValue<2>>()),
    class_<clang::SourceLocation>("SourceLocation", no_default_constructor)
        .def("isFileID", &clang::SourceLocation::isFileID)
        .def("printToString", &clang::SourceLocation::printToString),
    class_<clang::PresumedLoc>("PresumedLoc", no_default_constructor)
        .def("isValid", &clang::PresumedLoc::isValid)
        .def("isInvalid", &clang::PresumedLoc::isInvalid)
        .def("PresumedLoc-getFilename", &clang::PresumedLoc::getFilename)
        .def("getLine", &clang::PresumedLoc::getLine)
        .def("getColumn", &clang::PresumedLoc::getColumn)
        .def("getIncludeLoc", &clang::PresumedLoc::getIncludeLoc),
    class_<clang::SourceRange>("SourceRange", no_default_constructor)
        .def("getBegin", &clang::SourceRange::getBegin)
        .def("getEnd", &clang::SourceRange::getEnd),
    class_<clang::CharSourceRange>("CharSourceRange", no_default_constructor)
    // Create a CharSourceRange from a pair of begin/end SourceLocations that contains a TokenRange
    ,
    def("newCharSourceRange-getTokenRange",
        (clang::CharSourceRange (*)(clang::SourceLocation, clang::SourceLocation)) & clang::CharSourceRange::getTokenRange)
    // Create a CharSourceRange from a pair of begin/end SourceLocations that contains a CharRange
    ,
    def("newCharSourceRange-getCharRange",
        (clang::CharSourceRange (*)(clang::SourceLocation, clang::SourceLocation)) & clang::CharSourceRange::getCharRange)
    // Create a CharSourceRange from a SourceRange that contains a TokenRange
    ,
    def("newCharSourceRange-getTokenRange-SourceRange",
        (clang::CharSourceRange (*)(clang::SourceRange)) & clang::CharSourceRange::getTokenRange)
    // Create a CharSourceRange from a SourceRange that contains a CharRange
    ,
    def("newCharSourceRange-getCharRange-SourceRange",
        (clang::CharSourceRange (*)(clang::SourceRange)) & clang::CharSourceRange::getCharRange),
    class_<clang::CompilerInstance>("CompilerInstance")
        .def("getASTContext", &clang::CompilerInstance::getASTContext),
    class_<clang::FrontendAction>("FrontendAction", no_default_constructor),
    class_<clang::ASTFrontendAction, clang::FrontendAction>("Clang-ASTFrontendAction", no_default_constructor),
    class_<clang::SyntaxOnlyAction, clang::ASTFrontendAction>("Clang-SyntaxOnlyAction", no_default_constructor),
    derivable_class_<DerivableASTFrontendAction, clang::ASTFrontendAction>("ASTFrontendAction")
        .def("CreateASTConsumer", &DerivableASTFrontendAction::CreateASTConsumer, policies<adopt<result>>()),
    class_<clang::tooling::ClangTool>("ClangTool", no_default_constructor)
        .def_constructor("newClangTool", constructor<const clang::tooling::CompilationDatabase &, llvm::ArrayRef<std::string>>())
        .def("clearArgumentsAdjusters", &clang::tooling::ClangTool::clearArgumentsAdjusters)
        //            .  def("addArgumentsAdjuster",&clang::tooling::ClangTool::addArgumentsAdjuster)
        .def("appendArgumentsAdjuster", &clang::tooling::ClangTool::appendArgumentsAdjuster)
        .def("clangToolRun", &clang::tooling::ClangTool::run)
        .def("buildASTs", &clang::tooling::ClangTool::buildASTs, policies<pureOutValue<1>>()),
    class_<clang::tooling::Replacement>("Replacement", no_default_constructor)
        .def_constructor("newReplacement", constructor<clang::SourceManager &, const clang::CharSourceRange &, StringRef>())
        .def("toString", &clang::tooling::Replacement::toString)
        .def("apply", &clang::tooling::Replacement::apply),
    class_<clang::tooling::Range>("Range", no_default_constructor),
    class_<clang::tooling::Replacements>("Replacements", no_default_constructor),
    def("Replacements-insert", &af_Replacements_insert) // I have to wrap this one by hand - the overloads for std::set::insert are too many and too complicated
    ,
    class_<clang::tooling::RefactoringTool, clang::tooling::ClangTool>("RefactoringTool", no_default_constructor)
        .def_constructor("newRefactoringTool", constructor<const clang::tooling::CompilationDatabase &, llvm::ArrayRef<std::string>>())
        .def("getReplacements", &clang::tooling::RefactoringTool::getReplacements)
        .def("applyAllReplacements", &clang::tooling::RefactoringTool::applyAllReplacements)
        .def("runAndSave", &clang::tooling::RefactoringTool::runAndSave),
    class_<clang::Rewriter>("Rewriter", no_default_constructor)
        .def_constructor("newRewriter", constructor<clang::SourceManager &, const clang::LangOptions &>()),
    class_<clang::ASTUnit>("ASTUnit", no_default_constructor)
        .def("getASTContext", clang_ASTUnit_getASTContext) // (clang::ASTContext&(*)())&clang::ASTUnit::getASTContext)
    ,
    derivable_class_<DerivableSyntaxOnlyAction, clang::SyntaxOnlyAction>("SyntaxOnlyAction")
        .def("CreateASTConsumer", &DerivableSyntaxOnlyAction::CreateASTConsumer),
    class_<clang::tooling::ToolAction>("ToolAction", no_default_constructor),
    class_<clang::tooling::FrontendActionFactory, clang::tooling::ToolAction>("Clang-FrontendActionFactory", no_default_constructor),
    def("newFrontendActionFactory", &af_newFrontendActionFactory),
    derivable_class_<DerivableFrontendActionFactory, clang::tooling::FrontendActionFactory>("FrontendActionFactory")
        .def("create", &DerivableFrontendActionFactory::default_create),
    class_<clang::tooling::ArgumentsAdjuster>("ArgumentsAdjuster", no_default_constructor),
    def("getClangSyntaxOnlyAdjuster", &clang::tooling::getClangSyntaxOnlyAdjuster),
    def("getClangStripOutputAdjuster", &clang::tooling::getClangStripOutputAdjuster)

    // Don't need derivable_class_ ???????
    //            ,derivable_class_<DerivableArgumentsAdjuster,clang::tooling::ArgumentsAdjuster>("ArgumentsAdjuster")
    //            .    def("ArgumentsAdjuster-adjust",&DerivableArgumentsAdjuster::Adjust)

    /* Expose the Dynamic Matcher library */
    ,
    class_<clang::ast_matchers::dynamic::DynTypedMatcher>("DynTypedMatcher", no_default_constructor),
    class_<ParserValue>("ParserValue", no_default_constructor)
        .def_constructor("newParserValue", constructor<core::Cons_sp, const VariantValue &>()),
    class_<clang::ast_matchers::dynamic::VariantValue>("VariantValue", no_default_constructor)
        .def_constructor("newVariantValueUnsigned", constructor<unsigned>())
        .def_constructor("newVariantValueString", constructor<std::string>())
        .def_constructor("newVariantValueMatcher", constructor<const clang::ast_matchers::dynamic::VariantMatcher &>()),
    class_<clang::ast_matchers::dynamic::VariantMatcher>("VariantMatcher", no_default_constructor)
        .def("getTypeAsString", &clang::ast_matchers::dynamic::VariantMatcher::getTypeAsString)
    //            .def("getSingleMatcher",&clang::ast_matchers::dynamic::VariantMatcher::getSingleMatcher,policies<pureOutValue<1> >())
    ,
    def("getSingleMatcher", &af_getSingleMatcher),
    class_<Diagnostics>("Diagnostics", no_default_constructor)
        .def("toStringFull", &Diagnostics::toStringFull)
        .def_constructor("newDiagnostics", constructor<>()),
    def("constructMatcher", &Registry::constructMatcher),
    def("constructBoundMatcher", &Registry::constructBoundMatcher),
    class_<clang::ast_matchers::MatchFinder>("MatchFinder", no_default_constructor)
        .def_constructor("newMatchFinder", constructor<>())
        .def("addDynamicMatcher", &clang::ast_matchers::MatchFinder::addDynamicMatcher) // TODO: Add a nurse/patient relationship for argument and object
        .def("matchAST", &clang::ast_matchers::MatchFinder::matchAST),
    def("match", &af_match, policies<>(), ARGS_af_match, DECL_af_match, DOCS_af_match),
    def("runToolOnCode", &clang::tooling::runToolOnCode),
    class_<clang::ast_matchers::MatchFinder::MatchCallback>("MatchCallback-abstract", no_default_constructor),
    derivable_class_<DerivableMatchCallback, clang::ast_matchers::MatchFinder::MatchCallback>("MatchCallback")
        .def("run", &DerivableMatchCallback::default_run)
        .def("onStartOfTranslationUnit", &DerivableMatchCallback::default_onStartOfTranslationUnit)
        .def("onEndOfTranslationUnit", &DerivableMatchCallback::default_onEndOfTranslationUnit),
    class_<clang::ast_matchers::MatchFinderMatchResult>("MatchResult", no_default_constructor)
        .def("Nodes", &clang::ast_matchers::MatchFinderMatchResult::getNodes)
        .def("Context", &clang::ast_matchers::MatchFinderMatchResult::getContext)
        .def("SourceManager", &clang::ast_matchers::MatchFinderMatchResult::getSourceManager)
    //            .  property("Nodes",&clang::ast_matchers::MatchFinderMatchResult::Nodes)
    //            .  property("Context",&clang::ast_matchers::MatchFinderMatchResult::Context)
    //            .  property("SourceManager",&clang::ast_matchers::MatchFinderMatchResult::SourceManager)
    ,
    class_<clang::ast_matchers::BoundNodes>("BoundNodes", no_default_constructor),
    def("IDToNodeMap", &af_IDToNodeMap, policies<>(), ARGS_af_IDToNodeMap, DECL_af_IDToNodeMap, DOCS_af_IDToNodeMap),
    def("Lexer-getLocForEndOfToken", &clang::Lexer::getLocForEndOfToken),
    def("Lexer-getSourceText", &clang::Lexer::getSourceText, policies<pureOutValue<4>>()),
    class_<clang::tooling::CompileCommand>("CompileCommand", no_default_constructor)
        .property("CompileCommandDirectory", &clang::tooling::CompileCommand::Directory)
        .property("CompileCommandCommandLine", &clang::tooling::CompileCommand::CommandLine)
    //            ,def("buildASTFromCodeWithArgs",&clang::tooling::buildASTFromCodeWithArgs)
  ];
  Defun(deduplicate);
  Defun(clangVersionString);

  Defun(testDerivable);

  package("CLANG-COMMENTS", {}, {})[
    class_<clang::comments::Comment>("Comment", no_default_constructor)
        .def("getSourceRange", &clang::comments::Comment::getSourceRange),
    class_<clang::comments::FullComment, clang::comments::Comment>("FullComment", no_default_constructor)
  ];
}
};
