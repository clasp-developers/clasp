/*
    File: clangTooling.h
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

#ifndef asttooling_clangTooling_H
#define asttooling_clangTooling_H

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

#include <clasp/core/common.h>
#include <clasp/core/evaluator.h>
#include <clasp/asttooling/symbolTable.h>
#include <clasp/clbind/clbind.h>

namespace clang {
namespace ast_matchers {
/*! This class is defined to inherit from the MatchFinder::MatchResult to
          deal with the inability of forward declaring nested classes in C++
          MatchFinderMatchResult inherits everything from MatchFinder::MatchResult
          and I can expose it and forward declare it while I cannot forward declare
          MatchFinder::MatchResult */
struct MatchFinderMatchResult : public MatchFinder::MatchResult {
  MatchFinderMatchResult(const MatchFinder::MatchResult &other) : MatchFinder::MatchResult(other){};
  const BoundNodes &getNodes() const { return this->Nodes; };
  clang::ASTContext *getContext() const { return this->Context; };
  clang::SourceManager *getSourceManager() const { return this->SourceManager; };
};
};
};

namespace asttooling {

#if 0
    template <typename T>
    gctools::smart_ptr<clbind::Wrapper<T,T*> > Wrap(T* p) { return clbind::Wrapper<T,T*>::create(p,reg::registered_class<T>::id);};

    template <typename T>
    gctools::smart_ptr<clbind::Wrapper<T,std::unique_ptr<T>> > Wrap(std::unique_ptr<T>& p) {
	return clbind::Wrapper<T,std::unique_ptr<T>>::create(p,reg::registered_class<T>::id);
    };
#endif

#if 0
    class DerivableArgumentsAdjuster : public clbind::Derivable<clang::tooling::ArgumentsAdjuster> {
        typedef clang::tooling::ArgumentsAdjuster  Base;
    public:
        virtual clang::tooling::CommandLineArguments Adjust(const clang::tooling::CommandLineArguments& args) {
            core::T_sp obj = core::eval::funcall(_sym_ArgumentsAdjusterAdjust,this->asSmartPtr()
                                           , translate::to_object<clang::tooling::CommandLineArguments>::convert(args));
            translate::from_object<const clang::tooling::CommandLineArguments&> result(obj);
            return result._v;
        }

        clang::tooling::CommandLineArguments default_Adjust(const clang::tooling::CommandLineArguments& args) {
            return this->Base::Adjust(args);
        }
    };
#endif

class DerivableASTFrontendAction : public clbind::Derivable<clang::ASTFrontendAction> {
  typedef clang::ASTFrontendAction Base;

public:
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    core::T_sp obj = core::eval::funcall(_sym_CreateASTConsumer, this->asSmartPtr(), translate::to_object<clang::CompilerInstance &>::convert(Compiler), translate::to_object<llvm::StringRef>::convert(InFile));
    // translate::from_object<std::unique_ptr<clang::ASTConsumer>> result(obj);
    // return result._v;
    return translate::from_object<std::unique_ptr<clang::ASTConsumer>>(obj)._v;
  }

  std::unique_ptr<clang::ASTConsumer> default_CreateASTConsumer(
      clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return this->Base::CreateASTConsumer(Compiler, InFile);
  }

  virtual void ExecuteAction() {
    clang::CompilerInstance &CI = this->getCompilerInstance();
    CI.getFrontendOpts().DisableFree = true;
    this->Base::ExecuteAction();
  }
};

class DerivableSyntaxOnlyAction : public clbind::Derivable<clang::SyntaxOnlyAction> {
  typedef clang::SyntaxOnlyAction Base;

public:
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    core::T_sp obj = core::eval::funcall(_sym_CreateASTConsumer, this->asSmartPtr(), translate::to_object<clang::CompilerInstance &>::convert(Compiler), translate::to_object<llvm::StringRef>::convert(InFile));
    return translate::from_object<std::unique_ptr<clang::ASTConsumer>>(obj)._v;
  }

  std::unique_ptr<clang::ASTConsumer> default_CreateASTConsumer(
      clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return this->Base::CreateASTConsumer(Compiler, InFile);
  }

  virtual void ExecuteAction() {
    clang::CompilerInstance &CI = this->getCompilerInstance();
    CI.getFrontendOpts().DisableFree = true;
    this->Base::ExecuteAction();
  }
};

class DerivableFrontendActionFactory : public clbind::Derivable<clang::tooling::FrontendActionFactory> {
  typedef clang::tooling::FrontendActionFactory Base;

public:
  virtual clang::FrontendAction *create() {
    core::T_sp obj = core::eval::funcall(_sym_create, this->asSmartPtr());
    translate::from_object<clang::FrontendAction *> result(obj);
    return result._v;
  }

  clang::FrontendAction *default_create() {
    SIMPLE_ERROR(BF("Subclass must implement create"));
  };
};

class DerivableMatchCallback
    : public clbind::Derivable<clang::ast_matchers::MatchFinder::MatchCallback> {
  typedef clang::ast_matchers::MatchFinder::MatchCallback AlienBase;

public:
  virtual void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) {
    const clang::ast_matchers::MatchFinderMatchResult conv(Result); //  = static_cast<const clang::ast_matchers::MatchFinderMatchResult&>(Result);
    core::eval::funcall(asttooling::_sym_run, this->asSmartPtr(), translate::to_object<const clang::ast_matchers::MatchFinderMatchResult &>::convert(conv));
  }

  void default_run(const clang::ast_matchers::MatchFinderMatchResult &Result) {
    SIMPLE_ERROR(BF("Subclass must implement"));
  };

  virtual void onStartOfTranslationUnit() {
    //            printf("%s:%d entered onStartOfTranslationUnit funcalling\n", __FILE__, __LINE__);
    core::eval::funcall(_sym_onStartOfTranslationUnit, this->asSmartPtr());
  }
  void default_onStartOfTranslationUnit() {
    //            printf("%s:%d entered default_onStartOfTranslationUnit\n", __FILE__, __LINE__);
    this->AlienBase::onStartOfTranslationUnit();
  }
  virtual void onEndOfTranslationUnit() {
    //            printf("%s:%d entered onEndOfTranslationUnit funcalling\n", __FILE__, __LINE__);
    core::eval::funcall(_sym_onEndOfTranslationUnit, this->asSmartPtr());
  }
  void default_onEndOfTranslationUnit() {
    //            printf("%s:%d entered default_onEndOfTranslationUnit\n", __FILE__, __LINE__);
    this->AlienBase::onEndOfTranslationUnit();
  }

  void describe() {
    printf("%s:%d Entered DerivableMatchCallback::describe()\n", __FILE__, __LINE__);
    printf("this=%p  typeid(this)@%p  typeid(this).name=%s\n", this, &typeid(this), typeid(this).name());
    printf("dynamic_cast<void*>(this) = %p\n", dynamic_cast<void *>(this));
    printf("dynamic_cast<core::T_O*>(this) = %p\n", dynamic_cast<core::T_O *>(this));
    printf("typeid(dynamic_cast<core::T_O>*>(this))@%p  typeid.name=%s\n", &typeid(dynamic_cast<core::T_O *>(this)), typeid(dynamic_cast<core::T_O *>(this)).name());
    printf("dynamic_cast<Derivable<clang::ast_matchers::MatchFinder::MatchCallback>*>(this) = %p\n", dynamic_cast<Derivable<clang::ast_matchers::MatchFinder::MatchCallback> *>(this));
    printf("dynamic_cast<DerivableMatchCallback*>(this) = %p\n", dynamic_cast<DerivableMatchCallback *>(this));

    printf("alien pointer = %p\n", this->pointerToAlienWithin());
    printf("isgf %d\n", this->_isgf);
    printf("_Class: %s\n", _rep_(this->_Class).c_str());
    for (int i(0); i < this->_Slots.size(); ++i) {
      printf("_Slots[%d]: %s\n", i, _rep_(this->_Slots[i]).c_str());
    }
  }
  virtual ~DerivableMatchCallback() {
    printf("%s:%d ~DerivableMatchCallback dtor\n", __FILE__, __LINE__ );
  }
};
};
template <>
struct gctools::GCInfo<asttooling::DerivableMatchCallback> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = unmanaged;
};
DERIVABLE_TRANSLATE(asttooling::DerivableMatchCallback);



namespace asttooling {
void initialize_clangTooling();
};
//DERIVABLE_TRANSLATE(asttooling::DerivableArgumentsAdjuster);
DERIVABLE_TRANSLATE(asttooling::DerivableASTFrontendAction);
DERIVABLE_TRANSLATE(asttooling::DerivableSyntaxOnlyAction);
DERIVABLE_TRANSLATE(asttooling::DerivableFrontendActionFactory);

#if 0
namespace translate {

    template <>
    struct from_object<asttooling::DerivableMatchCallback*> {
        typedef asttooling::DerivableMatchCallback* DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) {
            if ( gctools::smart_ptr<asttooling::DerivableMatchCallback> dmc = o.asOrNull<asttooling::DerivableMatchCallback>())  {
            this->_v = dmc.px_ref();
            printf("%s:%d Converted T_sp to gctools::smart_ptr<asttooling::DerivableMatchCallback> -> converting to asttooling::DerivableMatchCallback* = %p\n", __FILE__, __LINE__, this->_v);
            return;
        }
#if 0
        clbind::Derivable<T>* dtptr = dynamic_cast<clbind::Derivable<T>*>(o.px_ref());
        printf("%s:%d In from_object<T*>(core::T_sp o)\n", __FILE__, __LINE__ );
        printf("dynamic_cast<clbind::Derivable<T>*>(o.px_ref()) = %p (SHOULD NOT BE NULL!!!)\n", dynamic_cast<clbind::Derivable<T>*>(o.px_ref()));
        printf("o.px_ref() = %p\n", o.px_ref());
        printf("typeid(o.px_ref())@%p  typeid(o.px_ref()).name=%s\n", &typeid(o.px_ref()),typeid(o.px_ref()).name());
        printf("typeid(clbind::Derivable<T>*)@%p   typeid(clbind::Derivable<T>*).name() = %s\n", &typeid(clbind::Derivable<T>*), typeid(clbind::Derivable<T>*).name());
        printf("dynamic_cast<void*>(o.px_ref()) = %p\n", dynamic_cast<void*>(o.px_ref()));
        printf("Invoking o.px_ref()->describe(); /* A virtual function */\n");
        o.px_ref()->describe();
#endif
        SIMPLE_ERROR(BF("Could not convert %s of RTTI type %s to %s") % _rep_(o) % typeid(o).name() % typeid(asttooling::DerivableMatchCallback*).name() );
    }
};


};

#endif

#endif
