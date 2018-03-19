/*
    File: expose_tools.h
*/

/*
Copyright (c) 2018, Christian E. Schafmeister

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

// NOTE: Most of the content here was taken from astExpose.cc -
// You may need to create your own macros for your purposes with
// exposing another C++ library.
// -- frgo, 2018-01-07

// ----------------------------------------------------------------------------
// For reference: This is an excerpt of a discussion on the
// #clasp IRC channel on 2018-ß1-23 where drmeister explains
// how to expose classes, constructors and such using clbind
// ...
//
// 17:48 <frgo> drmeister: Small distraction from the eh challenge: When I
//              have exposed a class using package[] I should be able to use
//              make-instance to create instances of that class, no?
// 17:49 <drmeister> Not yet - we need to do more work to integrate these into
//                   CLOS.    Use  (core:make-cxx-object 'whatever <initargs>)
// 17:50 <frgo> Ah! ... testing ...
// 17:50 <drmeister> Or bind constructors and give them names like
//                   make-whatever
// 17:51 <drmeister> See these:
//                   https://github.com/drmeister/clasp/blob/dev/src/asttooling/clangTooling.cc#L860
// 17:52 <drmeister>  .def_constructor("newVariantValueString",
//                   constructor<std::string>())
// 17:52 <drmeister> or
// 17:52 <drmeister> .def_constructor("newVariantValueUnsigned",
//                   constructor<unsigned>())
// 17:53 <frgo> Ok.
// 17:53 <frgo> I get:
// 17:54 <frgo> When doing:  (core:make-cxx-object 'dds.core.cond::condition)
//              =>
// 17:54 <frgo> This class cannot allocate instances
// 17:54 <frgo>    [Condition of type CORE:SIMPLE-PROGRAM-ERROR]
// 17:54 <drmeister> Ah - you declared that with no_default_constructor
//                   something like this:
//                   class_<clang::ast_matchers::MatchFinder>("MatchFinder",
//                   no_default_constructor)
// 17:55 <frgo> Yes. I did the monkey coding style: Copy & Paste
// 17:55 *** mcrist JOIN
// 17:56 <drmeister> I think that shuts down make-cxx-object -
//                   make-cxx-object is only good for default constructors
//                   that take no arguments.
// 17:56 <drmeister> Try defining a constructor explicitly with
//                   .def_constructor("newVariantValueUnsigned",
//                   constructor<unsigned>())
// 17:56 <drmeister> You put the constructor signature in here...
//                   constructor<types>
// 17:57 <drmeister> The make-cxx-object lets you pass initializers - but I'm
//                   not sure it works yet with clbind for external libraries
//                   - it works with the older, more builtin version of clbind
//                   now that I think about it.
// 17:58 <drmeister> If you have a constructor that takes no arguments use...
// 17:58 <drmeister> .def_constructor("make-dds.core.cond-condition",constructor<>)
// 17:59 <drmeister> Note that I'm rolling the package name into the function
//                   name - you can call it what you want as long as it is
//                   unique to the package.
// 17:59 <frgo> Yep, ok. Will try that.
// 17:59 <drmeister> It's one of the tricky things about exposing C++
//                   constructors (and methods and functions) - in C++ they
//                   can be overloaded to take different types - and Common
//                   Lisp expects everything to have unique names.
// 18:00 <drmeister> So I do things like:
//                   .def_constructor("newVariantValueUnsigned",
//                   constructor<unsigned>())
// 18:00 <drmeister> Then from CL I use (pkg:new-variant-value-unsigned 1234)
// 18:01 <frgo> Ok.
// 18:02 <frgo> For 'make-instance to work you said "we need to do some more
//              work". What kind of work would that be?
// 18:04 <Bike> well, if these things aren't standard-objects it might be as
//              simple as defining a method on make-instance to do what
//              make-cxx-object does
// 18:08 *** JonSmith JOIN
// 18:08 <drmeister> Yeah - there you go - that would do it. It would be made
//                   easier if we set them up with a common base class.
//
// ----------------------------------------------------------------------------

#if !defined( _CLASP_EXPOSE_TOOLS_H_ ) //[
#define _CLASP_EXPOSE_TOOLS_H_

// #if defined( __cplusplus )

#include <clasp/core/foundation.h>

#include <clang/AST/Comment.h>
#include <clang/AST/DeclBase.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/DeclFriend.h>
#include <clang/AST/DeclOpenMP.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>
#include <clang/AST/ExprObjC.h>
#include <clang/AST/StmtObjC.h>
#include <clang/AST/StmtOpenMP.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/ASTMatchers/Dynamic/VariantValue.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Tooling/Refactoring.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/Frontend/FrontendActions.h>

#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/package.h>
#include <clasp/llvmo/translators.h>
#include <clasp/asttooling/translators.h>
#include <clasp/core/symbolTable.h>

//
// This needs to be before clbind is included
//
#ifdef USE_MPS
#define NAMESPACE_clbind_clang
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_clbind_clang
#endif

#include <clasp/clbind/clbind.h>

#if !defined( LISP_CLASS )
#error "C macro LISP_CLASS not defined. Please make sure you include <clasp/core/object.h> !"
#endif

namespace clasp_expose
{
  using namespace clbind;
  // using namespace clang;

  core::T_sp mostDerivedDecl(const clang::Decl *cd);
  core::T_sp mostDerivedStmt(const clang::Stmt *x);
  core::T_sp mostDerivedType(const clang::Type *x);

    // --- TRANSLATION ---

#undef TRANSLATE_DECL
#define TRANSLATE_DECL(_T_, _ignore_)                  \
  namespace translate                                  \
  {                                                    \
    template <> struct to_object<clang::_T_##Decl *>   \
    {                                                  \
      static core::T_sp convert(clang::_T_##Decl *p)   \
      {                                                \
        if (!p)                                        \
          return _Nil<core::T_O>();                    \
        return clasp_expose::mostDerivedDecl(p);         \
      }                                                \
    };                                                 \
  };

#undef TRANSLATE_STMT
#define TRANSLATE_STMT(_T_, _ignore_)                  \
  namespace translate                                  \
  {                                                    \
    template <> struct to_object<clang::_T_##Decl *>   \
    {                                                  \
      static core::T_sp convert(clang::_T_##Decl *p)   \
      {                                                \
        if (!p)                                        \
          return _Nil<core::T_O>();                    \
        return clasp_expose::mostDerivediStmt(p);        \
      }                                                \
    };                                                 \
  };

#undef TRANSLATE_TYPE
#define TRANSLATE_TYPE(_T_, _ignore_)                  \
  namespace translate                                  \
  {                                                    \
    template <> struct to_object<clang::_T_##Decl *>   \
    {                                                  \
      static core::T_sp convert(clang::_T_##Decl *p)   \
      {                                                \
        if (!p)                                        \
          return _Nil<core::T_O>();                    \
        return clasp_expose::mostDerivedType(p);         \
      }                                                \
    };                                                 \
  };

  typedef clbind::Wrapper<clang::QualType, std::unique_ptr<clang::QualType>> QualType_wrapper;

  typedef clbind::Wrapper<clang::TypeLoc, std::unique_ptr<clang::TypeLoc>> TypeLoc_wrapper;

  typedef clbind::Wrapper<clang::TemplateName, std::unique_ptr<clang::TemplateName>> TemplateName_wrapper;

  // --- CASTING ---

  template <typename T>
    core::T_sp cast_decl(clang::Decl *d)
  {
    if (T *x = clang::dyn_cast<T>(d))
    {
      return clbind::Wrapper<T, T *>::make_wrapper(x, reg::registered_class<T>::id);
    }
    SIMPLE_ERROR(BF("Could not cast Decl to known Decl"));
  }

  template <typename T>
    core::T_sp cast_stmt(clang::Stmt *d)
  {
    T *x = llvm::cast<T>(d);
    return clbind::Wrapper<T, T *>::make_wrapper(x, reg::registered_class<T>::id);
  }

#undef CAST_DECL_CASE
#define CAST_DECL_CASE(_C_, _ignore_)                  \
  case clang::Decl::_C_:                               \
    {                                                  \
      core::T_sp res = cast_decl<clang::_C_##Decl>(d); \
      return res;                                      \
    }

#undef CAST_STMT_CASE
#define CAST_STMT_CASE(_C_, _ignore_)                  \
  case clang::Stmt::_C_:                               \
    {                                                  \
      core::T_sp res = cast_stmt<clang::_C_##Stmt>(d); \
      return res;                                      \
    }

#undef CAST_TYPE_CASE
#define CAST_TYPE_CASE(_C_, _ignore_)                  \
  case clang::Type::_C_:                               \
    {                                                  \
      core::T_sp res = cast_type<clang::_C_##Type>(d); \
      return res;                                      \
    }

    // --- DECLARATION ---

#undef ABSTRACT_TYPE
#define ABSTRACT_TYPE(_C_, _B_)

#undef EXPOSE_BASE_CLASS
#define EXPOSE_BASE_CLASS(_Class_) class_<_Class_>(#_Class_, no_default_constructor)

#undef EXPOSE_BASE_CLASS_EXPLCIT
#define EXPOSE_BASE_CLASS_EXPLICIT(_Class_,_Name_) class_<_Class_>(_Name_, no_default_constructor)

#undef EXPOSE_CLASS
#define EXPOSE_CLASS(_Class_, _Base_) class_<_Class_, _Base_>(#_Class_, no_default_constructor)

#undef EXPOSE_CLASS_2
#define EXPOSE_CLASS_2(_Class_, _Base1_, _Base2_) class_<_Class_, _Base1_, _Base2_>(#_Class_, no_default_constructor)

#undef EXPOSE_CLASS_3
#define EXPOSE_CLASS_3(_Class_, _Base1_, _Base2_, _Base3_) class_<_Class_, _Base1_, _Base2_, _Base3_>(#_Class_, no_default_constructor)

#undef EXPOSE_CLASS_EXPLICIT
#define EXPOSE_CLASS_EXPLICIT(_Class_, _Base_, _Name_) class_<_Class_, _Base_>(_Name_, no_default_constructor)

} // namespace

// #endif // __cplusplus
#endif //]
