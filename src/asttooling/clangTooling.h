
#ifndef	asttooling_clangTooling_H
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

#include "core/common.h"
#include "core/evaluator.h"
#include "symbolTable.h"
#include "clbind/clbind.h"

namespace asttooling
{

#if 0
    template <typename T>
    gctools::smart_ptr<clbind::Wrapper<T,T*> > Wrap(T* p) { return clbind::Wrapper<T,T*>::create(p,reg::registered_class<T>::id);};

    template <typename T>
    gctools::smart_ptr<clbind::Wrapper<T,std::unique_ptr<T>> > Wrap(std::unique_ptr<T>& p) {
	return clbind::Wrapper<T,std::unique_ptr<T>>::create(p,reg::registered_class<T>::id);
    };
#endif


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


    class DerivableASTFrontendAction : public clbind::Derivable<clang::ASTFrontendAction> {
        typedef clang::ASTFrontendAction      Base;
    public:
        virtual clang::ASTConsumer* CreateASTConsumer(
            clang::CompilerInstance& Compiler, llvm::StringRef InFile) {
            core::T_sp obj =  core::eval::funcall(_sym_CreateASTConsumer
                                                  , this->asSmartPtr()
                                                  , translate::to_object<clang::CompilerInstance&>::convert(Compiler)
                                                  , translate::to_object<llvm::StringRef>::convert(InFile));
            translate::from_object<clang::ASTConsumer*> result(obj);
            return result._v;
        }

        clang::ASTConsumer* default_CreateASTConsumer(
            clang::CompilerInstance& Compiler, llvm::StringRef InFile) {
            return this->Base::CreateASTConsumer(Compiler,InFile);
        }

        virtual void ExecuteAction() {
            clang::CompilerInstance &CI = this->getCompilerInstance();
            CI.getFrontendOpts().DisableFree = true;
            this->Base::ExecuteAction();
        }
    };


    class DerivableSyntaxOnlyAction : public clbind::Derivable<clang::SyntaxOnlyAction> {
        typedef clang::SyntaxOnlyAction      Base;
    public:
        virtual clang::ASTConsumer* CreateASTConsumer(
            clang::CompilerInstance& Compiler, llvm::StringRef InFile) {
            core::T_sp obj =  core::eval::funcall(_sym_CreateASTConsumer
                                                  , this->asSmartPtr()
                                                  , translate::to_object<clang::CompilerInstance&>::convert(Compiler)
                                                  , translate::to_object<llvm::StringRef>::convert(InFile));
            translate::from_object<clang::ASTConsumer*> result(obj);
            return result._v;
        }

        clang::ASTConsumer* default_CreateASTConsumer(
            clang::CompilerInstance& Compiler, llvm::StringRef InFile) {
            return this->Base::CreateASTConsumer(Compiler,InFile);
        }

        virtual void ExecuteAction() {
            clang::CompilerInstance &CI = this->getCompilerInstance();
            CI.getFrontendOpts().DisableFree = true;
            this->Base::ExecuteAction();
        }
    };



    class DerivableFrontendActionFactory : public clbind::Derivable<clang::tooling::FrontendActionFactory> {
        typedef clang::tooling::FrontendActionFactory      Base;
    public:
           
        virtual clang::FrontendAction* create()
        {
            core::T_sp obj = core::eval::funcall(_sym_create,this->asSmartPtr());
            translate::from_object<clang::FrontendAction*> result(obj);
            return result._v;
        }

        clang::FrontendAction* default_create()
        {
            SIMPLE_ERROR(BF("Subclass must implement create"));
        };
    };


    class DerivableMatchCallback
        : public clbind::Derivable<clang::ast_matchers::MatchFinder::MatchCallback> {
        typedef clang::ast_matchers::MatchFinder::MatchCallback      AlienBase;
    public:
        virtual void run(const clang::ast_matchers::MatchFinder::MatchResult& Result)
        {
            core::eval::funcall(asttooling::_sym_run
                                , this->asSmartPtr()
                                , translate::to_object<const clang::ast_matchers::MatchFinder::MatchResult&>::convert(Result)
                );
        }

        void default_run(const clang::ast_matchers::MatchFinder::MatchResult& Result)
        {
            SIMPLE_ERROR(BF("Subclass must implement"));
        };


        virtual void onStartOfTranslationUnit() {
//            printf("%s:%d entered onStartOfTranslationUnit funcalling\n", __FILE__, __LINE__);
            core::eval::funcall(_sym_onStartOfTranslationUnit
                                , this->asSmartPtr() );
        }
        void default_onStartOfTranslationUnit() {
//            printf("%s:%d entered default_onStartOfTranslationUnit\n", __FILE__, __LINE__);
            this->AlienBase::onStartOfTranslationUnit();
        }
        virtual void onEndOfTranslationUnit() {
//            printf("%s:%d entered onEndOfTranslationUnit funcalling\n", __FILE__, __LINE__);
            core::eval::funcall(_sym_onEndOfTranslationUnit
                                , this->asSmartPtr() );
        }
        void default_onEndOfTranslationUnit() {
//            printf("%s:%d entered default_onEndOfTranslationUnit\n", __FILE__, __LINE__);
            this->AlienBase::onEndOfTranslationUnit();
        }


        void describe()
        {
            printf("%s:%d Entered DerivableMatchCallback::describe()\n", __FILE__, __LINE__ );
            printf("this=%p  typeid(this)@%p  typeid(this).name=%s\n", this, &typeid(this), typeid(this).name());
            printf("dynamic_cast<void*>(this) = %p\n", dynamic_cast<void*>(this));
            printf("dynamic_cast<core::T_O*>(this) = %p\n", dynamic_cast<core::T_O*>(this));
            printf("typeid(dynamic_cast<core::T_O>*>(this))@%p  typeid.name=%s\n", &typeid(dynamic_cast<core::T_O*>(this)), typeid(dynamic_cast<core::T_O*>(this)).name());
            printf("dynamic_cast<Derivable<clang::ast_matchers::MatchFinder::MatchCallback>*>(this) = %p\n", dynamic_cast<Derivable<clang::ast_matchers::MatchFinder::MatchCallback>*>(this));
            printf("dynamic_cast<DerivableMatchCallback*>(this) = %p\n", dynamic_cast<DerivableMatchCallback*>(this));

            printf("alien pointer = %p\n", this->pointerToAlienWithin() );
            printf("isgf %d\n", this->_isgf);
            printf("_Class: %s\n", _rep_(this->_Class).c_str());
            for (int i(0); i<this->_Slots.size(); ++i ) {
                printf("_Slots[%d]: %s\n", i, _rep_(this->_Slots[i]).c_str());
            }
        }


    };

};

template<> struct gctools::GCInfo<asttooling::DerivableMatchCallback> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

namespace asttooling {


    void initialize_clangTooling();



};
DERIVABLE_TRANSLATE(asttooling::DerivableArgumentsAdjuster);
DERIVABLE_TRANSLATE(asttooling::DerivableMatchCallback);
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

