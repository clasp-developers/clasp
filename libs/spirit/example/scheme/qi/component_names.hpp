//  Copyright (c) 2001-2010 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#if !defined(BOOST_SPIRIT_QI_COMPONENT_NAMES)
#define BOOST_SPIRIT_QI_COMPONENT_NAMES

///////////////////////////////////////////////////////////////////////////////
namespace scheme { namespace qi 
{
    ///////////////////////////////////////////////////////////////////////////
    // a list of names for all supported parser primitives taking no parameters
    static char const* const primitives0[] = 
    {
      // character parsers
        "char_"
      , "alnum", "alpha", "blank", "cntrl", "digit", "graph"
      , "print", "punct"
      , "space", "xdigit"
      , "lower", "upper"

      // numerics
      , "long_long", "long_", "int_", "short_"
      , "ulong_long", "ulong_", "uint_", "ushort_"
      , "bin", "oct", "hex"
      , "bool_", "true_", "false_"
      , "long_double", "double_", "float_"

      // binary
      , "qword", "dword", "word", "byte_"
      , "little_qword", "little_dword", "little_word"
      , "big_qword", "big_dword", "big_word"

      // auxiliary
      , "eol", "eoi", "eps"
      , 0
    };

    // a list of names for all supported parser primitives taking 1 parameter
    static char const* const primitives1[] = 
    {
        // character parsers
        "char_", "lit", "string"
      , 0
    };

    // a list of names for all supported parser primitives taking 2 parameter
    static char const* const primitives2[] = 
    {
        "char_"
      , 0
    };

    // a list of names for all supported parser directives taking 0 parameter
    static char const* const directives0[] = 
    {
        // manage skip parser
        "lexeme", "skip", "no_skip"

        // case management
      , "no_case"

        // auxiliary
      , "omit", "raw"

        // encoding
      , "ascii", "standard", "standard_wide", "iso8859_1"
#if defined BOOST_SPIRIT_UNICODE
      , "unicode"
#endif
      , 0
    };

    // a list of names for all supported unary parser operators 
    static char const* const unary_names[] = 
    {
        "*", "+", "-", "!", "&"
      , 0
    };
}}

#endif
