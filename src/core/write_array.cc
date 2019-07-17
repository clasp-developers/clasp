/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    write_array.d -- File interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/print.h>
#include <clasp/core/array.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/numberToString.h>

namespace core {
    static void
    write_array_inner(bool vector, Array_sp x, T_sp stream) {
	//	printf("%s:%d write_array_inner\n", __FILE__, __LINE__ );
	//cl_env_ptr env = ecl_process_env();
	std::vector<size_t> adims;
	cl_index subscripts[CLASP_ARRAY_RANK_LIMIT];
	Fixnum n, j, m, k, i;
	Fixnum print_length;
	Fixnum print_level;
	bool readably = clasp_print_readably();
	if (vector) {
                adims.push_back(x->length());
                n = 1;
	} else {
	    adims = x->arrayDimensionsAsVector();
	    n = x->rank();
	}
	if (readably) {
	    print_length = MOST_POSITIVE_FIXNUM;
	    print_level = MOST_POSITIVE_FIXNUM;
	} else {
	    if (!clasp_print_array()) {
		writestr_stream(vector ? "#<vector " : "#<array ", stream);
		clasp_write_addr(x, stream);
		clasp_write_char('>', stream);
		return;
	    }
	    print_level = clasp_print_level();
	    print_length = clasp_print_length();
	}
	clasp_write_char('#', stream);
	if (print_level == 0)
	    return;
	if (readably) {
                {
                        clasp_write_char('A', stream);
                        clasp_write_char('(', stream);
                        write_object(x->element_type(), stream);
                        clasp_write_char(' ', stream);
                        if (n > 0) {
                                clasp_write_char('(', stream);
                                for (j = 0; j < n; j++) {
                                        write_object(clasp_make_fixnum(adims[j]), stream);
                                        if (j < n - 1)
                                                clasp_write_char(' ', stream);
                                }
                                clasp_write_char(')', stream);
                        } else {
                                write_object(_Nil<T_O>(), stream);
                        }
                        clasp_write_char(' ', stream);
                }
	} else if (!vector) {
	    _clasp_write_fixnum(n, stream);
	    clasp_write_char('A', stream);
	}
	DynamicScopeManager scope;
	if (print_level >= n) {
	    /* We can write the elements of the array */
	    print_level -= n;
	    scope.pushSpecialVariableAndSet(cl::_sym_STARprint_levelSTAR, clasp_make_fixnum(print_level));
	} else {
	    /* The elements of the array are not printed */
	    n = print_level;
	    print_level = -1;
	}
	for (j = 0; j < n; j++)
	    subscripts[j] = 0;
	for (m = 0, j = 0;;) {
	    for (i = j; i < n; i++) {
		if (subscripts[i] == 0) {
		    clasp_write_char('(', stream);
		    if (adims[i] == 0) {
			clasp_write_char(')', stream);
			j = i - 1;
			k = 0;
			goto INC;
		    }
		}
		if (subscripts[i] > 0)
		    clasp_write_char(' ', stream);
		if (subscripts[i] >= print_length) {
		    writestr_stream("...)", stream);
		    k = adims[i] - subscripts[i];
		    subscripts[i] = 0;
		    for (j = i + 1; j < n; j++)
			k *= adims[j];
		    j = i - 1;
		    goto INC;
		}
	    }
	    /* FIXME: This conses! */
	    if (print_level >= 0)
		write_object(x->rowMajorAref(m), stream);
	    else
		clasp_write_char('#', stream);
	    j = n - 1;
	    k = 1;

	INC:
	    while (j >= 0) {
		if (++subscripts[j] < adims[j])
		    break;
		subscripts[j] = 0;
		clasp_write_char(')', stream);
		--j;
	    }
	    if (j < 0)
		break;
	    m += k;
	}
	if (readably) {
	    clasp_write_char(')', stream);
	}
    }

    void MDArray_O::__write__(T_sp stream) const {
            if (this->rank() == 0) {
                    if (!clasp_print_array() && !clasp_print_readably()) {
                            writestr_stream("#<ARRAY ", stream);
                            clasp_write_addr(this->asSmartPtr(), stream);
                            clasp_write_char('>', stream);
                    } else {
                            writestr_stream("#0A", stream);
                            write_object(this->rowMajorAref(0), stream);
                    }
            } else if (this->rank() == 1) {
                    write_array_inner(true, this->asSmartPtr(), stream);
            } else {
                    write_array_inner(0, this->asSmartPtr(), stream);
	}
    }
    
    void SimpleVector_O::__write__(T_sp stream) const {
            if (this->rank() == 0) {
                    if (!clasp_print_array() && !clasp_print_readably()) {
                            writestr_stream("#<SIMPLE-VECTOR ", stream);
                            clasp_write_addr(this->asSmartPtr(), stream);
                            clasp_write_char('>', stream);
                    } else {
                            writestr_stream("#0A", stream);
                            write_object(this->rowMajorAref(0), stream);
                    }
            } else if (this->rank() == 1) {
                    write_array_inner(true, this->asSmartPtr(), stream);
            } else {
                    write_array_inner(0, this->asSmartPtr(), stream);
            }
    }
    
#if 0
#ifdef CLASP_UNICODE
    void _clasp_write_string(T_sp x, T_sp stream) {
	cl_index ndx;
	if (!clasp_print_escape() && !clasp_print_readably()) {
	    for (ndx = 0; ndx < x->string.fillp; ndx++)
		clasp_write_char(x->string.self[ndx], stream);
	} else {
	    clasp_write_char('"', stream);
	    for (ndx = 0; ndx < x->string.fillp; ndx++) {
		clasp_character c = x->string.self[ndx];
		if (c == '"' || c == '\\')
		    clasp_write_char('\\', stream);
		clasp_write_char(c, stream);
	    }
	    clasp_write_char('"', stream);
	}
    }
#endif
#endif

    void SimpleBitVector_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    writestr_stream("#<simple-bit-vector ", stream);
                    clasp_write_addr(this->asSmartPtr(), stream);
                    clasp_write_char('>', stream);
            } else {
                    writestr_stream("#*", stream);
                    for (cl_index ndx=0; ndx<this->length(); ++ndx)
                            if (this->testBit(ndx))
                                    clasp_write_char('1', stream);
                            else
                                    clasp_write_char('0', stream);

            }	
    }

    void BitVector_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    writestr_stream("#<bit-vector ", stream);
                    clasp_write_addr(this->asSmartPtr(), stream);
                    clasp_write_char('>', stream);
            } else {
                    writestr_stream("#*", stream);
                    for (cl_index ndx = 0; ndx < this->length(); ndx++)
                            if (this->testBit(ndx))
                                    clasp_write_char('1', stream);
                            else
                                    clasp_write_char('0', stream);
            }
    }

    static void write_simple_vector_simple (const char *title, Array_sp x, T_sp stream) {
            writestr_stream(title, stream);
            SafeBufferStr8Ns buffer;
            clasp_write_string(" length:",stream);
            stringstream slen;
            slen << x->length();
            clasp_write_characters(slen.str().c_str(),slen.str().size(),stream);
            if  (clasp_print_array()) {
                    clasp_write_string(" data: ",stream);
                    int print_base = clasp_print_base();
                    for (size_t ndx=0; ndx<x->length(); ++ndx) {
                            core__integer_to_string(buffer._Buffer, clasp_make_fixnum(x->rowMajorAref(ndx)),
                                                    make_fixnum(print_base),
                                                    cl::_sym_STARprint_radixSTAR->symbolValue().isTrue(),
                                                    true);
                            cl__write_sequence(buffer._Buffer,stream,make_fixnum(0),_Nil<T_O>());
                            clasp_write_string(" ",stream);
                            buffer._Buffer->fillPointerSet(0);
                    }
            }
            clasp_write_string(">",stream);
    }

    static void write_simple_vector_simple_float (const char *title, Array_sp x, T_sp stream) {
            writestr_stream(title, stream);
            clasp_write_string(" length:",stream);
            stringstream slen;
            slen << x->length();
            clasp_write_characters(slen.str().c_str(),slen.str().size(),stream);
            if (clasp_print_array()) {
                    clasp_write_string(" data: ",stream);
                    for (size_t ndx=0; ndx<x->length(); ++ndx) {
                            write_float(gc::As<Float_sp>(x->rowMajorAref(ndx)), stream);
                            clasp_write_string(" ",stream);
                    }
            }
            clasp_write_string(">",stream);
    }
    
    void SimpleVector_byte8_t_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title = "#<SIMPLE-VECTOR-BYTE8-T ";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
            }
            else
                    write_array_inner(true, this->asSmartPtr(), stream);

    }

    void SimpleVector_byte16_t_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title = "#<SimpleVector_byte16_t_O>";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
            }
            else 
                    write_array_inner(true, this->asSmartPtr(), stream);
    }

    void SimpleVector_byte32_t_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title = "#<SimpleVector_byte32_t_O>";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
             }
            else
                    write_array_inner(true, this->asSmartPtr(), stream);
    }

    void SimpleVector_byte64_t_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title = "#<SimpleVector_byte64_t_O>";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
            }
            else
                    write_array_inner(true, this->asSmartPtr(), stream);
    }

    void   SimpleVector_fixnum_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title = "#<SimpleVector_fixnum_O>";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
            }
            else write_array_inner(true, this->asSmartPtr(), stream);
    }

    void SimpleVector_int8_t_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title = "#<SimpleVector_int8_t_O>";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
            }
            else
                    write_array_inner(true, this->asSmartPtr(), stream);
    }

    void SimpleVector_int16_t_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title = "#<SimpleVector_int16_t_O>";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
            }
            else
                    write_array_inner(true, this->asSmartPtr(), stream);
    }

    void SimpleVector_int32_t_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title = "#<SimpleVector_int32_t_O>";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
            }
            else
                    write_array_inner(true, this->asSmartPtr(), stream);
    }

    void SimpleVector_int64_t_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title ="#<SimpleVector_int64_t_O>";
                    write_simple_vector_simple(title, this->asSmartPtr(), stream);
            }
            else
                    write_array_inner(true, this->asSmartPtr(), stream);
    }

    void SimpleVector_float_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title ="#<SimpleVector_float_O>";
                    write_simple_vector_simple_float(title, this->asSmartPtr(), stream);
            }
            else
                    write_array_inner(true, this->asSmartPtr(), stream);
    }

    void SimpleVector_double_O::__write__(T_sp stream) const {
            if (!clasp_print_array() && !clasp_print_readably()) {
                    const char *title ="#<SimpleVector_double_O>";
                    write_simple_vector_simple_float(title, this->asSmartPtr(), stream);
             }
            else 
                    write_array_inner(true, this->asSmartPtr(), stream);
    }
    
    void unsafe_write_SimpleBaseString(SimpleBaseString_sp str, size_t start, size_t end, T_sp stream) {
	cl_index ndx;
	if (!clasp_print_escape() && !clasp_print_readably()) {
	    for (ndx = start; ndx < end; ndx++) {
		clasp_write_char((*str)[ndx], stream);
	    }
	} else {
	    clasp_write_char('"', stream);
	    for (ndx = start; ndx < end; ndx++) {
		char c = (*str)[ndx];
		if (c == '"' || c == '\\')
		    clasp_write_char('\\', stream);
		clasp_write_char(c, stream);
	    }
	    clasp_write_char('"', stream);
	}
    }

    void unsafe_write_SimpleCharacterString(SimpleCharacterString_sp str, size_t start, size_t end, T_sp stream) {
	cl_index ndx;
	if (!clasp_print_escape() && !clasp_print_readably()) {
	    for (ndx = start; ndx < end; ndx++) {
		clasp_write_char((*str)[ndx],stream);
	    }
	} else {
	    clasp_write_char('"', stream);
	    for (ndx = start; ndx < end; ndx++) {
		claspCharacter c = (*str)[ndx];
		if (c == '"' || c == '\\')
		    clasp_write_char('\\', stream);
		clasp_write_char((*str)[ndx],stream);
	    }
	    clasp_write_char('"', stream);
	}
    }
    
    void SimpleBaseString_O::__write__(T_sp stream) const {
	unsafe_write_SimpleBaseString(this->asSmartPtr(),0,this->length(),stream);
    }

    void Str8Ns_O::__write__(T_sp stream) const {
	size_t start, end;
	AbstractSimpleVector_sp str;
	this->asAbstractSimpleVectorRange(str,start,end);
	SimpleBaseString_sp sb = gc::As<SimpleBaseString_sp>(str);
	unsafe_write_SimpleBaseString(sb,start,end,stream);
    }
    
    void SimpleCharacterString_O::__write__(T_sp stream) const {
	unsafe_write_SimpleCharacterString(this->asSmartPtr(),0,this->length(),stream);
    }
    
    void StrWNs_O::__write__(T_sp stream) const {
	size_t start, end;
	AbstractSimpleVector_sp str;
	this->asAbstractSimpleVectorRange(str,start,end);
	SimpleCharacterString_sp sc = gc::As<SimpleCharacterString_sp>(str);
	unsafe_write_SimpleCharacterString(sc,start,end,stream);
    }

};
