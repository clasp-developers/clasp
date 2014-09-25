/*
    File: sequence.h
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
#ifndef	_core_Sequence_H
#define _core_Sequence_H

#include "core/foundation.h"
#include "core/object.h"
#include "corePackage.fwd.h"

namespace core
{


#if 0
    FORWARD(Sequence);
    c l a s s Sequence_O : virtual public T_O
    {
	L I S P_BASE1(T_O);
	L I S P_CLASS(core,ClPkg,Sequence_O,"Sequence");
    public:
	void archiveBase(SNode_sp node);
    public:
	explicit Sequence_O(): T_O() {}
	virtual ~Sequence_O() {}

    private: // instance variables here


    public: // Functions here

	virtual uint length() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};

	virtual T_sp reverse() {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual T_sp nreverse() {_OF(); SUBCLASS_MUST_IMPLEMENT();};

	virtual T_sp elt(int index) const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual T_sp setf_elt(int index, T_sp value);


	virtual T_sp subseq(int start, T_sp end) const {_G(); SUBCLASS_MUST_IMPLEMENT();};
	virtual T_sp setf_subseq(int start, T_sp end, T_sp new_subseq) {_G(); SUBCLASS_MUST_IMPLEMENT();};
    };
#endif

}; /* core */

//TRANSLATE(core::Sequence_O);



namespace core {
    uint cl_length(T_sp arg);

    T_sp cl_reverse(T_sp obj);
    T_sp cl_nreverse(T_sp obj);

    T_sp cl_elt(T_sp sequence, int index);
    T_sp core_setf_elt(T_sp sequence, int index, T_sp value);

    T_sp cl_subseq(T_sp sequence, int start, T_sp end);
    T_sp core_setf_subseq(T_sp sequence, int start, T_sp end, T_sp newSubseq);


    T_sp cl_copySeq(T_sp seq);

    /* Return a valid start/end/length of a sequence or throw an error if invalid values are provided */
    size_t_pair sequenceStartEnd(const char* sourcefile, uint lineno, const char* functionName,
				 const string& packageName,
				 T_sp seq, Fixnum_sp start, Fixnum_sp end);


    void initialize_sequence();

};
#endif /* _core_Sequence_H */
