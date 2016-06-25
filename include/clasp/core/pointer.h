// -*- Mode: C++; tab-width: 2; indent-tabs-mode: c++; c-basic-offset: 2 -*-
//
//   ██████╗    ██╗          █████╗     ███████╗    ██████╗
//  ██╔════╝    ██║         ██╔══██╗    ██╔════╝    ██╔══██╗
//  ██║         ██║         ███████║    ███████╗    ██████╔╝
//  ██║         ██║         ██╔══██║    ╚════██║    ██╔═══╝
//  ╚██████╗    ███████╗    ██║  ██║    ███████║    ██║
//   ╚═════╝    ╚══════╝    ╚═╝  ╚═╝    ╚══════╝    ╚═╝
//
// ===========================================================================
//    C L A S P - COMMON LISP AND C++
// ===========================================================================
//
// Copyright (c) 2014, Christian E. Schafmeister
//
// CLASP is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
//
// See directory 'clasp/licenses' for full details.
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// ===========================================================================

//! @file   pointer.h
//! @author Christian E. Schafmeister
//! @brief  Pointer Lisp Object Implementation

// -^-


#if !defined( __CLASP_CORE_POINTER_H__ )
#define __CLASP_CORE_POINTER_H__

// ---------------------------------------------------------------------------
//    SYSTEM INCLUDES
// ---------------------------------------------------------------------------

// --- Standard C++ Includes ---
// NONE

// --- Platform-specific Includes ---
// NONE

// ---------------------------------------------------------------------------
//    APPLICaATION INCLUDES
// ---------------------------------------------------------------------------

#include "clasp/core/foundation.h"
#include "clasp/core/object.h"
#include "clasp/core/pointer.fwd.h"

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

#if defined( __cplusplus )

namespace core {

// ---------------------------------------------------------------------------
//   CLASSES & METHODS & FUNCTIONS
// ---------------------------------------------------------------------------

  class Pointer_O : public General_O {
    LISP_CLASS( core, CorePkg, Pointer_O, "Pointer", General_O );

  public: // public methods

    DEFAULT_CTOR_DTOR( Pointer_O );

    static Pointer_sp   create( void * );
    // static Pointer_sp   createForT_sp( T_sp );

    static Pointer_sp   null_pointer( void );
    static Pointer_sp   make( core::Number_sp );

    Pointer_sp          inc_pointer( core::Integer_sp );

    void                initialize( void );
    void               *ptr() const { return this->m_rawptr; };
    virtual bool        eql_( T_sp ) const;
    string              __repr__( void ) const;


    bool                pointerP() const { IMPLEMENT_ME(); };
    bool                null_pointerP() const { IMPLEMENT_ME(); };

  private: // instance variables

    void               *m_rawptr;

  }; // Pointer class

// ---------------------------------------------------------------------------
//   END OF FILE
// ---------------------------------------------------------------------------

}; // namespace

#endif // __cplusplus
#endif // __CLASP_CORE_POINTER_H__
