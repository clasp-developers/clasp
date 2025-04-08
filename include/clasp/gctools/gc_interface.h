#pragma once

/*
    File: gc_interface.h
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

#include <clasp/gctools/snapshotSaveLoad.fwd.h>
#include <clasp/clbind/inheritance.fwd.h>

//
// All class forward declarations
//
namespace core {
class T_O;
class WrappedPointer_O;
class Function_O;
class Creator_O;
class Iterator_O;
}; // namespace core
namespace asttooling {
namespace internal {
class MatcherDescriptor_O;
};
}; // namespace asttooling
namespace clbind {
class ConstructorCreator_O;
class ClassRep_O;
}; // namespace clbind
namespace gctools {
class WeakPointer;
class Ephemeron;
}; // namespace gctools

//////////////////////////////////////////////////////////////////////
//
// Forward declarations
//
//
namespace core {
class KeyValuePair;
class KeywordArgument;
class RequiredArgument;
class SymbolClassHolderPair;
class SymbolStorage;
class DynamicBinding;
class AuxArgument;
class OptionalArgument;
class CacheRecord;
class ExceptionEntry;
}; // namespace core
namespace asttooling {
class DerivableSyntaxOnlyAction;
class DerivableASTFrontendAction;
class DerivableMatchCallback;
class DerivableFrontendActionFactory;
}; // namespace asttooling

////////////////////////////////////////////////////////////
//
// Forward definition for classes
//

#ifndef USE_PRECISE_GC
#ifndef SCRAPING
#define DECLARE_FORWARDS
#include INIT_CLASSES_INC_H // REPLACED CLASP_GC_CC // "main/clasp_gc.cc"
#undef DECLARE_FORWARDS
#endif
#endif
#if defined(USE_PRECISE_GC) && !defined(SCRAPING)
#ifndef RUNNING_PRECISEPREP
#define DECLARE_FORWARDS
#include CLASP_GC_CC // "main/clasp_gc.cc"
#undef DECLARE_FORWARDS
#endif
#endif

////////////////////////////////////////////////////////////
//
// Define the stamps
#ifndef USE_PRECISE_GC
#ifndef SCRAPING
#define GC_STAMP_SELECTORS
#include INIT_CLASSES_INC_H // REPLACED CLASP_GC_CC
#undef GC_STAMP_SELECTORS
#endif
#elif !defined(SCRAPING)
#ifndef RUNNING_PRECISEPREP
#define GC_STAMP_SELECTORS
#include CLASP_GC_CC // "main/clasp_gc.cc"
#undef GC_STAMP_SELECTORS
#endif
#endif

extern "C" {
bool valid_stamp(gctools::stamp_t kind);
const char* obj_name(gctools::stamp_t kind);
extern void obj_dump_base(void* base);
extern void obj_deallocate_unmanaged_instance(gctools::smart_ptr<core::T_O> obj);

extern int global_symbol_count;
extern gctools::smart_ptr<core::Symbol_O> global_symbols[];

extern void client_validate_internal(void* tagged_client);
extern void client_validate_recursive(void* tagged_client, std::set<void*>& seen);
};

void initialize_clasp_Kinds();
void initialize_clasp();

void initialize_functions();
void initialize_source_info();
void initialize_classes_and_methods();
void initialize_typeq_map();

extern std::map<std::string, size_t> global_unshifted_nowhere_stamp_name_map;
extern std::vector<std::string> global_unshifted_nowhere_stamp_names;
extern size_t global_last_stamp;
extern void register_stamp_name(const std::string& stamp_name, size_t stamp_num);
