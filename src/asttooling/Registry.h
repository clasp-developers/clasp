//===--- Registry.h - Matcher registry -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Registry of all known matchers.
///
/// The registry provides a generic interface to construct any matcher by name.
///
//===----------------------------------------------------------------------===//

#ifndef asttooling_registry_H
#define asttooling_registry_H
#include <core/common.h>
#include <core/lispVector.h>
#include <asttooling/Diagnostics.h>
#include "clang/ASTMatchers/Dynamic/VariantValue.h"
#include "clang/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

namespace asttooling {
    namespace internal {
        class MatcherDescriptor;
    };
};

namespace asttooling {
    namespace RegMap {
        class SymbolMatcherDescriptorPair {
	public:
            SymbolMatcherDescriptorPair(core::Symbol_sp k, /*const*/ internal::MatcherDescriptor* v) : Name(k), matcher(v) {};
            core::Symbol_sp     Name;
            internal::MatcherDescriptor*    matcher;
        };


        class RegistryMaps {
            struct metadata_always_fix_pointers_to_derived_classes;
            FRIEND_GC_SCANNER();
            friend class SymbolMatcherDescriptorPair;
        public:
            RegistryMaps();
            ~RegistryMaps();

            void lazyInitialize() const;

            typedef gctools::Vec0<SymbolMatcherDescriptorPair> ConstructorMap;
            typedef ConstructorMap::iterator iterator;
            typedef ConstructorMap::const_iterator const_iterator;
            iterator begin() { this->lazyInitialize(); return this->Constructors.begin();};
            iterator end() { this->lazyInitialize(); return this->Constructors.end();};
            const_iterator begin() const { this->lazyInitialize(); return this->Constructors.begin();};
            const_iterator end() const { this->lazyInitialize(); return this->Constructors.end();};

            const ConstructorMap &constructors() const { this->lazyInitialize(); return this->Constructors; }


            /*! Find the constructor associated with the symbol */
            ConstructorMap::iterator find(core::Symbol_sp key)
            {
                this->lazyInitialize();
                ConstructorMap::iterator it;
                for ( it = this->Constructors.begin(); it!=this->Constructors.end(); ++it ) {
                    if (it->Name == key ) return it;
                }
                return it;
            }


        private:
            void _registerMatcher(core::Symbol_sp MatcherName, internal::MatcherDescriptor *Callback) const;
            /*! This is used to replace the map<Symbol_sp,const MatcherDescriptor*> that used to be a ConstructorMap */
        private:
            bool Initialized;
            mutable ConstructorMap Constructors;
        };
    };

class Registry {
public:
  /// \brief Construct a matcher from the registry by name.
  ///
  /// Consult the registry of known matchers and construct the appropriate
  /// matcher by name.
  ///
  /// \param MatcherName The name of the matcher to instantiate.
  ///
  /// \param NameRange The location of the name in the matcher source.
  ///   Useful for error reporting.
  ///
  /// \param Args The argument list for the matcher. The number and types of the
  ///   values must be valid for the matcher requested. Otherwise, the function
  ///   will return an error.
  ///
  /// \return The matcher object constructed if no error was found.
  ///   A null matcher if the matcher is not found, or if the number of
  ///   arguments or argument types do not match the signature.
  ///   In that case \c Error will contain the description of the error.
    static clang::ast_matchers::dynamic::VariantMatcher constructMatcher(core::Symbol_sp  MatcherName,
                                                                         core::Cons_sp NameRange,
                                                                         core::Vector_sp Args,
                                                                         Diagnostics* Error);

  /// \brief Construct a matcher from the registry and bind it.
  ///
  /// Similar the \c constructMatcher() above, but it then tries to bind the
  /// matcher to the specified \c BindID.
  /// If the matcher is not bindable, it sets an error in \c Error and returns
  /// a null matcher.
    static clang::ast_matchers::dynamic::VariantMatcher constructBoundMatcher(core::Symbol_sp MatcherName,
                                                                              core::Cons_sp NameRange,
                                                                              StringRef BindID,
                                                                              core::Vector_sp Args,
                                                                              Diagnostics* Error);

private:
  Registry() LLVM_DELETED_FUNCTION;
};

}  // namespace asttooling

#endif  // asttooling_registry_H
