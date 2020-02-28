/*
    File: character.h
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
#ifndef _core_character_H
#define _core_character_H

#include <clasp/core/object.h>
#include <clasp/core/array.fwd.h>
#include <clasp/core/character.fwd.h>

#define NULL_CHAR 0
#define BELL_CHAR 7
#define BACKSPACE_CHAR 8
#define TAB_CHAR 9
#define NEWLINE_CHAR 10
#define LINE_FEED_CHAR 10
#define PAGE_CHAR 12
#define RETURN_CHAR 13
#define ESCAPE_CHAR 27
#define SPACE_CHAR 32
#define RUBOUT_CHAR 127

namespace core {

  // Utility
  void notCharacterError();
  void handleWideCharactersError(claspCharacter cc);

Character_sp clasp_make_standard_character(claspCharacter c);
inline claspCharacter unbox_character(Character_sp c) {
  return c.unsafe_character();
};

SimpleBaseString_sp cl__char_name(Character_sp och);

int clasp_string_case(String_sp s);
Fixnum clasp_digitp(claspCharacter ch, int basis);

bool cl__standard_char_p(Character_sp ch);
bool clasp_islower_wide(claspCharacter code);
bool clasp_isupper_wide(claspCharacter code);

class Character_dummy_O : public General_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Character_dummy_O, "character",core::General_O);
};

#if 0
 class CPointer_dummy_O : public T_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, CPointer_dummy_O, "cpointer",core::T_O);
};
#endif
};



namespace core {
inline short clasp_digit_char(Fixnum w, Fixnum r) {
  if (r < 2 || r > 36 || w < 0 || w >= r)
    return (-1);
  if (w < 10)
    return (w + '0');
  else
    return (w - 10 + 'A');
}

 inline bool clasp_is_character_type( T_sp the_type) {
   return ( the_type == cl::_sym_character ||
            the_type == cl::_sym_base_char ||
            the_type == cl::_sym_extended_char ||
            the_type == cl::_sym_standard_char );
 }

}; /* core */

namespace translate {

  template <>
    struct from_object< char, std::true_type >
  {
    typedef char DeclareType;

    DeclareType _v;
  from_object( core::T_sp o ) : _v( o.unsafe_fixnum() ){};
  };

  template <>
    struct to_object<char> {
    typedef uint GivenType;
    static core::T_sp convert(GivenType v) {
      _G();
      return core::clasp_make_fixnum(v);
    }
  };
};

namespace core {
//claspCharacter clasp_charCode(T_sp elt); // like ecl__char_code

inline bool clasp_invalid_base_char_p(int c) {
  return (c <= 32) || (c == 127);
}

inline claspCharacter claspCharacter_upcase(claspCharacter code) {
  // Should probably be towupper, but doesn't work. not even in Xcode
  claspCharacter uc = toupper(code);
  return uc;
}

inline claspCharacter claspCharacter_downcase(claspCharacter code) {
  // Should probably be towlower, but doesn't work. not even in Xcode
  claspCharacter uc = tolower(code);
  return uc;
}

inline bool clasp_alphanumericp(claspCharacter i) {
  return isalnum(i);
}


// See character.fwd.h for the following
#if 0
 Character_sp clasp_make_character(claspCharacter c) {
  return gc::make_tagged_character(c);
}
#endif

 inline bool clasp_base_char_p(claspCharacter c) {
   return c<=255;
 }

 inline bool clasp_base_char_p(Character_sp c) {
   return c.unsafe_character()>=0 && c.unsafe_character()<=255;
 }

 inline bool clasp_isupper(claspCharacter cc) {
   if (cc>255) {
     return clasp_isupper_wide(cc);
   }
   else return isupper(cc);
 }

inline bool clasp_islower(claspCharacter cc) {
   if (cc>255) {
     return clasp_islower_wide(cc);
   }
   else return islower(cc);
 }

inline Character_sp clasp_make_standard_character(claspCharacter c) {
  return gc::make_tagged_character(c);
}

Character_sp clasp_character_create_from_name(string const &name);
};

#endif
