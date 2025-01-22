#pragma once
/*
    File: character.fwd.h
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
namespace core {

bool clasp_charEqual2(T_sp x, T_sp y);

struct CharacterInfo {
  HashTableEqual_sp _NamesToCharacterIndex;
  gctools::Vec0<T_sp> gIndexedCharacters;
  gctools::Vec0<T_sp> gCharacterNames;
  const char* repr() const { return "CharacterInfo"; };
  CharacterInfo(){};
  void initialize();
};

inline Character_sp clasp_make_character(claspCharacter c) { return gc::make_tagged_character(c); }

inline claspCharacter clasp_as_claspCharacter(Character_sp c) { return c.unsafe_character(); }

claspCharacter char_upcase(claspCharacter);

} // namespace core
