/*
    File: queue.cc
*/

/*
Copyright (c) 2017, Christian E. Schafmeister
Copyright (c) 2017, Frank Goenninger, Goenninger B&T UG, Germany

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

// ===========================================================================
//
//  FLI - Foreign Language Interface for allocating, setting,
//        getting and freeing foreign memory and calling foreign
//        functions.
//
//  Augiust 2016 by Frank Goenninger, Gönninger B&T UG, Germany
//
// ===========================================================================

// --- TODO ---
//
// Test, test, test ...

#define DEBUG_LEVEL_FULL

// ---------------------------------------------------------------------------
//   SYSTEM INCLUDES
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   DEBUG SETTINGS
// ---------------------------------------------------------------------------

// #define DEBUG_LEVEL_FULL

// ---------------------------------------------------------------------------
//   LLVM INCLUDES
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   CLASP INCLUDES
// ---------------------------------------------------------------------------

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbol.h>
#include <clasp/core/package.h>
#include <clasp/core/multipleValues.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/core/primitives.h>
#include <clasp/core/lispList.h>
#include <clasp/gctools/interrupt.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/queue.h>

#include <clasp/core/wrappers.h> // last include is wrappers.h

// ---------------------------------------------------------------------------
//   DEFINES
// ---------------------------------------------------------------------------

#if defined( DEBUG_LEVEL_FULL )
#define DEBUG_PRINT(_msg_) fprintf( stderr, "%s", (_msg_).str().c_str())
#else
#define DEBUG_PRINT(msg)
#endif

// ---------------------------------------------------------------------------
//   I M P L E M E N T A T I O N
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//   LOCAL DEFINES
// ---------------------------------------------------------------------------

// None

// ---------------------------------------------------------------------------
//   NAMESPACE
// ---------------------------------------------------------------------------

namespace mp {

// ---------------------------------------------------------------------------
//   TYPE DEFINTITIONS
// ---------------------------------------------------------------------------

// None

// ---------------------------------------------------------------------------
//   GLOBAL VARS
// ---------------------------------------------------------------------------

// None

// ---------------------------------------------------------------------------
//   FORWARD DECLARATIONS
// ---------------------------------------------------------------------------

// None

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
Queue_O::Queue_O() : _Name( _Nil<T_O>() )
{
  DEBUG_PRINT(BF("%s (%s:%d) | Constructor for %s called\n.") % __FUNCTION__ % __FILE__ % __LINE__ % this->__repr__().c_str() );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
Queue_O::~Queue_O()
{
  DEBUG_PRINT(BF("%s (%s:%d) | Destructor for %s called\n.") % __FUNCTION__ % __FILE__ % __LINE__ % this->__repr__().c_str() );

}
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
Queue_sp make_queue( std::string name )
{
  return Queue_O::create( name );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
Queue_sp PERCENTmake_queue( core::String_sp name )
{
  return Queue_O::create( name );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
Queue_sp Queue_O::create( std::string name )
{
  GC_ALLOCATE(Queue_O, self);
  self->_Name = core::Str_O::create( name );
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
Queue_sp Queue_O::create( core::String_sp name )
{
  GC_ALLOCATE(Queue_O, self);
  self->_Name = name;
  return self;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::T_sp Queue_O::PERCENTenqueue( core::T_sp value )
{
  if ( this->_Queue.enqueue( value ) == true )
  {
    return Values( value, _lisp->_true() );
  }
  else
  {
    return Values( _Nil<core::T_O>(), _Nil<core::T_O>() );
  }
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::T_sp Queue_O::PERCENTdequeue( void )
{
  core::T_sp item;

  if ( this->_Queue.try_dequeue( item ) == true )
  {
    return Values( item, _lisp->_true() );
  }
  else
  {
    return Values( _Nil<core::T_O>(), _Nil<core::T_O>() );
  }
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
inline uint64_t Queue_O::count( void ) const
{
  return static_cast< uint64_t >( this->_Queue.size_approx() );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
core::Integer_sp Queue_O::PERCENTcount( void )
{
  return mk_integer_uint64( this->count() );
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
inline string Queue_O::__repr__( void ) const
{
  stringstream ss;

  ss << "#<"
     << this->_instanceClass()->classNameAsString()
#ifdef USE_BOEHM // things don't move in boehm
     << " @ " << ( void * )( this->asSmartPtr().raw_() )
#endif
     << " name: " << _rep_( this->_Name )
     << " size: " <<  this->count()
     << ">";

  return ss.str();
}

}; // namespace
