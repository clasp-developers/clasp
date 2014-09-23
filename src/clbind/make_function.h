/*
    File: make_function.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
// Copyright Daniel Wallin 2008. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef CLBIND_MAKE_FUNCTION_081014_HPP
# define CLBIND_MAKE_FUNCTION_081014_HPP

# include <clbind/config.h>
//# include <clbind/object.hpp>
//# include <clbind/detail/call.hpp>
//# include <clbind/detail/compute_score.hpp>
# include <clbind/deduce_signature.h>
//# include <clbind/detail/format_signature.hpp>

namespace clbind {

namespace detail
{
# ifndef CLBIND_NO_EXCEPTIONS
  CLBIND_API void handle_exception_aux(cl_State* L);
# endif

// MSVC complains about member being sensitive to alignment (C4121)
// when F is a pointer to member of a class with virtual bases.
# ifdef BOOST_MSVC
#  pragma pack(push)
#  pragma pack(16)
# endif

  template <class F, class Signature, class Policies>
  struct function_object_impl : function_object
  {
      function_object_impl(F f, Policies const& policies)
        : function_object(&entry_point)
        , f(f)
        , policies(policies)
      {}

      int call(cl_State* L, invoke_context& ctx) const
      {
          return invoke(L, *this, ctx, f, Signature(), policies);
      }

      void format_signature(cl_State* L, char const* function) const
      {
          detail::format_signature(L, function, Signature());
      }

      static int entry_point(cl_State* L)
      {
          function_object_impl const* impl =
              *(function_object_impl const**)cl_touserdata(L, cl_upvalueindex(1));

          invoke_context ctx;

          int results = 0;

# ifndef CLBIND_NO_EXCEPTIONS
          bool exception_caught = false;

          try
          {
              results = invoke(
                  L, *impl, ctx, impl->f, Signature(), impl->policies);
          }
          catch (...)
          {
              exception_caught = true;
              handle_exception_aux(L);
          }

          if (exception_caught)
              cl_error(L);
# else
          results = invoke(L, *impl, ctx, impl->f, Signature(), impl->policies);
# endif

          if (!ctx)
          {
              ctx.format_error(L, impl);
              cl_error(L);
          }

          return results;
      }

      F f;
      Policies policies;
  };

# ifdef BOOST_MSVC
#  pragma pack(pop)
# endif

  CLBIND_API object make_function_aux(
      cl_State* L, function_object* impl
  );

  CLBIND_API void add_overload(object const&, char const*, object const&);

} // namespace detail

template <class F, class Signature, class Policies>
object make_function(cl_State* L, F f, Signature, Policies)
{
    return detail::make_function_aux(
        L
      , new detail::function_object_impl<F, Signature, Policies>(
            f, Policies()
        )
    );
}

template <class F>
object make_function(cl_State* L, F f)
{
    return make_function(L, detail::deduce_signature(f), null_type());
}

} // namespace clbind

#endif // CLBIND_MAKE_FUNCTION_081014_HPP
