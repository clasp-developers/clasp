/*============================================================================
  KWSys - Kitware System Library
  Copyright 2000-2009 Kitware, Inc., Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#ifndef cmsys_IOStream_hxx
#define cmsys_IOStream_hxx

#include <cmsys/ios/iosfwd>

/* Define these macros temporarily to keep the code readable.  */
#if !defined (KWSYS_NAMESPACE) && !cmsys_NAME_IS_KWSYS
# define kwsysEXPORT cmsys_EXPORT
# define kwsys_ios cmsys_ios
#endif

/* Whether istream supports long long.  */
#define cmsys_IOS_HAS_ISTREAM_LONG_LONG 1

/* Whether ostream supports long long.  */
#define cmsys_IOS_HAS_OSTREAM_LONG_LONG 1

/* Size of type long long and 0 if not available.  */
#define cmsys_IOS_SIZEOF_LONG_LONG TRUE

/* Determine whether we need to define the streaming operators for
   long long or __int64.  */
#if cmsys_IOS_SIZEOF_LONG_LONG
# if !cmsys_IOS_HAS_ISTREAM_LONG_LONG || \
     !cmsys_IOS_HAS_OSTREAM_LONG_LONG
# define cmsys_IOS_NEED_OPERATORS_LL 1
  namespace cmsys
  {
    typedef long long IOStreamSLL;
    typedef unsigned long long IOStreamULL;
  }
# endif
#elif defined(_MSC_VER) && _MSC_VER < 1300
# define cmsys_IOS_NEED_OPERATORS_LL 1
  namespace cmsys
  {
    typedef __int64 IOStreamSLL;
    typedef unsigned __int64 IOStreamULL;
  }
#endif
#if !defined(cmsys_IOS_NEED_OPERATORS_LL)
# define cmsys_IOS_NEED_OPERATORS_LL 0
#endif

#if cmsys_IOS_NEED_OPERATORS_LL
# if !cmsys_IOS_HAS_ISTREAM_LONG_LONG

/* Input stream operator implementation functions.  */
namespace cmsys
{
kwsysEXPORT kwsys_ios::istream& IOStreamScan(kwsys_ios::istream&,
                                             IOStreamSLL&);
kwsysEXPORT kwsys_ios::istream& IOStreamScan(kwsys_ios::istream&,
                                             IOStreamULL&);
}

/* Provide input stream operator for long long.  */
#  if !defined(cmsys_IOS_NO_ISTREAM_LONG_LONG) && \
      !defined(KWSYS_IOS_ISTREAM_LONG_LONG_DEFINED)
#   define KWSYS_IOS_ISTREAM_LONG_LONG_DEFINED
#   define cmsys_IOS_ISTREAM_LONG_LONG_DEFINED
inline kwsys_ios::istream&
operator>>(kwsys_ios::istream& is, cmsys::IOStreamSLL& value)
{
  return cmsys::IOStreamScan(is, value);
}
#  endif

/* Provide input stream operator for unsigned long long.  */
#  if !defined(cmsys_IOS_NO_ISTREAM_UNSIGNED_LONG_LONG) && \
      !defined(KWSYS_IOS_ISTREAM_UNSIGNED_LONG_LONG_DEFINED)
#   define KWSYS_IOS_ISTREAM_UNSIGNED_LONG_LONG_DEFINED
#   define cmsys_IOS_ISTREAM_UNSIGNED_LONG_LONG_DEFINED
inline kwsys_ios::istream&
operator>>(kwsys_ios::istream& is, cmsys::IOStreamULL& value)
{
  return cmsys::IOStreamScan(is, value);
}
#  endif
# endif /* !cmsys_IOS_HAS_ISTREAM_LONG_LONG */

# if !cmsys_IOS_HAS_OSTREAM_LONG_LONG

/* Output stream operator implementation functions.  */
namespace cmsys
{
kwsysEXPORT kwsys_ios::ostream& IOStreamPrint(kwsys_ios::ostream&,
                                              IOStreamSLL);
kwsysEXPORT kwsys_ios::ostream& IOStreamPrint(kwsys_ios::ostream&,
                                              IOStreamULL);
}

/* Provide output stream operator for long long.  */
#  if !defined(cmsys_IOS_NO_OSTREAM_LONG_LONG) && \
      !defined(KWSYS_IOS_OSTREAM_LONG_LONG_DEFINED)
#   define KWSYS_IOS_OSTREAM_LONG_LONG_DEFINED
#   define cmsys_IOS_OSTREAM_LONG_LONG_DEFINED
inline kwsys_ios::ostream&
operator<<(kwsys_ios::ostream& os, cmsys::IOStreamSLL value)
{
  return cmsys::IOStreamPrint(os, value);
}
#  endif

/* Provide output stream operator for unsigned long long.  */
#  if !defined(cmsys_IOS_NO_OSTREAM_UNSIGNED_LONG_LONG) && \
      !defined(KWSYS_IOS_OSTREAM_UNSIGNED_LONG_LONG_DEFINED)
#   define KWSYS_IOS_OSTREAM_UNSIGNED_LONG_LONG_DEFINED
#   define cmsys_IOS_OSTREAM_UNSIGNED_LONG_LONG_DEFINED
inline kwsys_ios::ostream&
operator<<(kwsys_ios::ostream& os, cmsys::IOStreamULL value)
{
  return cmsys::IOStreamPrint(os, value);
}
#  endif
# endif /* !cmsys_IOS_HAS_OSTREAM_LONG_LONG */
#endif /* cmsys_IOS_NEED_OPERATORS_LL */

/* Undefine temporary macros.  */
#if !defined (KWSYS_NAMESPACE) && !cmsys_NAME_IS_KWSYS
# undef kwsysEXPORT
# undef kwsys_ios
#endif

/* If building a C++ file in kwsys itself, give the source file
   access to the macros without a configured namespace.  */
#if defined(KWSYS_NAMESPACE)
# define KWSYS_IOS_SIZEOF_LONG_LONG      cmsys_IOS_SIZEOF_LONG_LONG
# define KWSYS_IOS_HAS_ISTREAM_LONG_LONG cmsys_IOS_HAS_ISTREAM_LONG_LONG
# define KWSYS_IOS_HAS_OSTREAM_LONG_LONG cmsys_IOS_HAS_OSTREAM_LONG_LONG
# define KWSYS_IOS_NEED_OPERATORS_LL     cmsys_IOS_NEED_OPERATORS_LL
#endif

#endif

