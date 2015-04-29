/*
    File: translators.h
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
#ifndef _llvmo_translators_H
#define _llvmo_translators_H


#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DIBuilder.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/str.h>
#include <clasp/llvmo/llvmoExpose.fwd.h>


namespace translate {

    template<>
    struct from_object<llvm::StringRef,std::true_type>
    {
        typedef llvm::StringRef DeclareType;
	string _Storage; // Store the string here so it won't get wiped out before its used by the callee
        DeclareType _v;;
	from_object(T_P o) : _Storage(o.as<core::Str_O>()->get()), _v(llvm::StringRef(this->_Storage)) {};
    };


    template<>
    struct from_object<const llvm::Twine&>
    {
        typedef llvm::Twine DeclareType;
	string _Storage; // Store the string here so it won't get wiped out before its used by the callee
        DeclareType _v;;
	from_object(T_P o) : _Storage(o.as<core::Str_O>()->get()), _v(llvm::Twine(this->_Storage)) {};
    };


    template <>
    struct to_object<const llvm::StringRef&>
    {
	static core::T_sp convert(llvm::StringRef const& sr)
	{_G(); return(Values( core::Str_O::create(sr.data()))); }
    };


    template <>
    struct to_object<llvm::StringRef&>
    {
	static core::T_sp convert(llvm::StringRef & sr)
	{return core::Str_O::create(sr.data()); }
    };

    template <>
    struct to_object<llvm::StringRef>
    {
	static core::T_sp convert(const llvm::StringRef & sr)
	{return core::Str_O::create(sr.data(),sr.size()); }
    };




    template <>
    struct from_object<llvm::DIBuilder::DebugEmissionKind>
    {
        typedef llvm::DIBuilder::DebugEmissionKind DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) {
            if ( o.nilp() ) {
                goto INVALID;
            }
            if ( o.isA<core::Symbol_O>() ) {
                core::Symbol_sp sym = o.as<core::Symbol_O>();
                if ( sym == kw::_sym_FullDebug ) {
                    this->_v = llvm::DIBuilder::FullDebug;
                    return;
                } else if ( sym == kw::_sym_LineTablesOnly ) {
                    this->_v = llvm::DIBuilder::LineTablesOnly;
                    return;
                }
            }
        INVALID:
            SIMPLE_ERROR(BF("You must pass :full-debug or :line-tables-only, only those are valid DebugEmissionKind"));
        }
    };



    template <>
    struct from_object<llvm::ArrayRef<std::string> > {
        typedef std::vector<std::string> DeclareType;
        DeclareType _v;
        from_object(core::T_sp o) {
            if ( o.nilp() ) {
                _v.clear();
                return;
            } else if ( core::List_sp lcstrs = o.asOrNull<core::Cons_O>() ) {
                for ( auto cstrs : lcstrs ) {
                    core::Str_sp s = core::oCar(cstrs).as<core::Str_O>();
                    _v.push_back(std::string(s->get()));
                }
                return;
            } else if ( core::Vector_sp vstrs = o.asOrNull<core::Vector_O>() ) {
                _v.resize(vstrs->length());
                for (int i(0),iEnd(vstrs->length()); i<iEnd; ++i ) {
                    _v[i] = vstrs->elt(i).as<core::Str_O>()->get();
                }
                return;
            }
            SIMPLE_ERROR(BF("Could not convert %s to llvm::ArrayRef<std::string>") % core::_rep_(o));
        }
    };








};



#endif
