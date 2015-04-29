/*
    File: cons_funcs.h
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


#if 0
inline T_sp ocar(T_sp obj) {
	if (obj.nilp()) return _Nil<T_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cobj->_Car;
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }
    inline T_sp ocdr(T_sp obj) {
	if (obj.nilp()) return _Nil<T_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cobj->_Cdr;
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }
    inline Cons_sp ccar(T_sp obj) {
	if (obj.nilp()) return _Nil<T_O>();
	if (Cons_sp cobj = obj.asOrNull<T_O>()) {
	    return cobj->_Car;
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }
    inline Cons_sp ccdr(T_sp obj) {
	if (obj.nilp()) return _Nil<Cons_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cobj->_Cdr.as_or_nil<Cons_O>();
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }

inline T_sp ocaar(T_sp o) { return ocar(ccar(o.as_or_nil<Cons_O>()));};
inline T_sp ocadr(T_sp o) { return ocar(ccdr(o.as_or_nil<Cons_O>()));};
inline T_sp ocdar(T_sp o) { return ocdr(ccar(o.as_or_nil<Cons_O>()));};
inline T_sp ocddr(T_sp o) { return ocdr(ccdr(o.as_or_nil<Cons_O>()));};
inline T_sp ocaaar(T_sp o)  { return ocar(ccar(ccar(o.as_or_nil<Cons_O>())));};
inline T_sp ocaadr(T_sp o)  { return ocar(ccar(ccdr(o.as_or_nil<Cons_O>())));};
inline T_sp ocadar(T_sp o)  { return ocar(ccdr(ccar(o.as_or_nil<Cons_O>())));};
inline T_sp ocaddr(T_sp o)  { return ocar(ccdr(ccdr(o.as_or_nil<Cons_O>())));};
inline T_sp ocdaar(T_sp o)  { return ocdr(ccar(ccar(o.as_or_nil<Cons_O>())));};
inline T_sp ocdadr(T_sp o)  { return ocdr(ccar(ccdr(o.as_or_nil<Cons_O>())));};
inline T_sp ocddar(T_sp o)  { return ocdr(ccdr(ccar(o.as_or_nil<Cons_O>())));};
inline T_sp ocdddr(T_sp o)  { return ocdr(ccdr(ccdr(o.as_or_nil<Cons_O>())));};
inline T_sp ocaaaar(T_sp o) { return ocar(ccar(ccar(o.as_or_nil<Cons_O>())));};
inline T_sp ocaadar(T_sp o) { return ocar(ccar(ccdr(ccar(o.as_or_nil<Cons_O>()))));};
inline T_sp ocadaar(T_sp o) { return ocar(ccdr(ccar(ccar(o.as_or_nil<Cons_O>()))));};
inline T_sp ocaddar(T_sp o) { return ocar(ccdr(ccdr(ccar(o.as_or_nil<Cons_O>()))));};
inline T_sp ocdaaar(T_sp o) { return ocdr(ccar(ccar(ccar(o.as_or_nil<Cons_O>()))));};
inline T_sp ocdadar(T_sp o) { return ocdr(ccar(ccdr(ccar(o.as_or_nil<Cons_O>()))));};
inline T_sp ocddaar(T_sp o) { return ocdr(ccdr(ccar(ccar(o.as_or_nil<Cons_O>()))));};
inline T_sp ocdddar(T_sp o) { return ocdr(ccdr(ccdr(ccar(o.as_or_nil<Cons_O>()))));};
inline T_sp ocaaadr(T_sp o) { return ocar(ccar(ccar(ccar(o.as_or_nil<Cons_O>()))));};
inline T_sp ocaaddr(T_sp o) { return ocar(ccar(ccdr(ccdr(o.as_or_nil<Cons_O>()))));};
inline T_sp ocadadr(T_sp o) { return ocar(ccdr(ccar(ccdr(o.as_or_nil<Cons_O>()))));};
inline T_sp ocadddr(T_sp o) { return ocar(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>()))));};
inline T_sp ocdaadr(T_sp o) { return ocdr(ccar(ccar(ccdr(o.as_or_nil<Cons_O>()))));};
inline T_sp ocdaddr(T_sp o) { return ocdr(ccar(ccdr(ccdr(o.as_or_nil<Cons_O>()))));};
inline T_sp ocddadr(T_sp o) { return ocdr(ccdr(ccar(ccdr(o.as_or_nil<Cons_O>()))));};
inline T_sp ocddddr(T_sp o) { return ocdr(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>()))));};
inline T_sp ofirst(T_sp o)  { return ocar(o.as_or_nil<Cons_O>()); };
inline T_sp osecond(T_sp o) { return ocar(ccdr(o.as_or_nil<Cons_O>()));};
inline T_sp othird(T_sp o)  { return ocar(ccdr(ccdr(o.as_or_nil<Cons_O>())));};
inline T_sp ofourth(T_sp o) { return ocar(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>()))));};
inline T_sp ofifth(T_sp o)  { return ocar(ccdr(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>())))));};
inline T_sp osixth(T_sp o)  { return ocar(ccdr(ccdr(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>()))))));};
inline T_sp oseventh(T_sp o){ return ocar(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>())))))));};
inline T_sp oeighth(T_sp o) { return ocar(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>()))))))));};
inline T_sp oninth(T_sp o)  { return ocar(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>())))))))));};
inline T_sp otenth(T_sp o)  { return ocar(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(ccdr(o.as_or_nil<Cons_O>()))))))))));};
