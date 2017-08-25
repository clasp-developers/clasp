/*
    File: load.h
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
#ifndef core_load_H
#define core_load_H


namespace kw {
extern core::Symbol_sp& _sym_default;
}

namespace core {

T_sp core__load_source(T_sp source, bool verbose, bool print, T_sp externalFormat);

T_sp core__load_no_package_set(T_sp source,
                               T_sp verbose = _Nil<T_O>(),
                               T_sp print = _Nil<T_O>(),
                               T_sp if_does_not_exist = _Nil<T_O>(),
                               T_sp external_format = kw::_sym_default,
                               T_sp search_list = _Nil<T_O>());

 T_sp cl__load(T_sp source,
             T_sp verbose = _Nil<T_O>(),
             T_sp print = _Nil<T_O>(),
             T_sp if_does_not_exist = _Nil<T_O>(),
             T_sp external_format = kw::_sym_default,
             T_sp search_list = _Nil<T_O>());

};

#endif
