/*
    File: multiple_value_pointers.h
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
#ifndef gctools_multiple_value_pointers_H
#define gctools_multiple_value_pointers_H


namespace gctools
{



    template <class T>
    class multiple_values : public smart_ptr<T>
    {
    private:
	size_t 	_number_of_values;
    public:
	multiple_values() : smart_ptr<T>(_Nil<T>()), _number_of_values(0) {};
    multiple_values(void* p, size_t num) : smart_ptr<T>((T*)p), _number_of_values(num) {};
	multiple_values(const smart_ptr<T>& v,int num) : smart_ptr<T>(v), _number_of_values(num) {};
	multiple_values(const smart_ptr<T>& v) : smart_ptr<T>(v), _number_of_values(1) {};
	template <class Y> multiple_values(const multiple_values<Y>& yy) : smart_ptr<T>(yy), _number_of_values(yy.number_of_values()) {};



        static multiple_values<T> createFromValues() {
	    core::MultipleValues& mv = core::lisp_multipleValues();
            multiple_values<T> result( mv.getSize()==0 ? _Nil<core::T_O>() : mv.valueGet(0,mv.getSize()), mv.getSize());
            return result;
        }

#if 0
        static multiple_values<T> createFromVec0(const Vec0<core::T_sp>& vec) {
	    core::MultipleValues& mv = core::lisp_multipleValues();
            mv.loadFromVec0(vec);
            return multiple_values<T>::createFromValues();
        }
#endif

        void saveToMultipleValue0() const {
	    core::MultipleValues& mv = core::lisp_multipleValues();
            mv.valueSet(0,*this);
        };

        void saveToVec0(::gctools::Vec0<core::T_sp>& values) {
            values.resize(this->_number_of_values);
            values[0] = *this;
            for ( int i(1); i<this->_number_of_values; ++i ) {
                values[i] = this->valueGet(i);
            }
        }

	void loadFromVec0(const ::gctools::Vec0<core::T_sp>& values) {
            for ( size_t i(1),iEnd(values.size()); i<iEnd; ++i ) {
                this->valueSet(i,values[i]);
            }
	    this->_number_of_values = values.size();
	    if ( this->_number_of_values > 0 ) {
		this->px = gctools::DynamicCast<T*,core::T_O*>::castOrNULL(values[0].px);
		GCTOOLS_ASSERT(this->px!=NULL);
	    } else {
		this->px = _Nil<T>().px;
	    }
	}
	    




#ifdef POLYMORPHIC_SMART_PTR
	virtual
#endif
	int number_of_values() const { return this->_number_of_values;};

	void valueSet(int idx, core::T_sp val)
	{
	    core::MultipleValues& mv = core::lisp_multipleValues();
	    mv.valueSet(idx,val);
	}
	    
	core::T_sp valueGet(int idx) const
	{
	    core::MultipleValues& mv = core::lisp_multipleValues();
	    return mv.valueGet(idx,this->_number_of_values);
	};


        core::T_sp second() const {
            return this->valueGet(1);
        }
        core::T_sp third() const {
            return this->valueGet(1);
        }

	void dump() 
	{
	    if (this->_number_of_values > 0 )
	    {
		string ts = (*this)->__repr__();
		printf(" %s\n", ts.c_str() );
		for (int i(1); i<this->_number_of_values; ++i )
		{
		    string ts = _rep_(this->valueGet(i));
		    printf(" %s\n", ts.c_str() );
		}
	    } else
	    {
		printf( "---No values---\n");
	    }
	}


            



    };

#if defined(USE_MPS)
    template <class TO, class FROM>
    multiple_values<TO> dynamic_pointer_cast(const multiple_values<FROM>& ptr)
    {
	smart_ptr<FROM> sp = ptr;
	return multiple_values<TO>(gctools::dynamic_pointer_cast<TO>(sp),ptr.number_of_values());
    };
#else
    template <class TO, class FROM>
    multiple_values<TO> dynamic_pointer_cast(const multiple_values<FROM>& ptr)
    {
	smart_ptr<FROM> sp = ptr;
	return multiple_values<TO>(boost::dynamic_pointer_cast<TO>(sp),ptr.number_of_values());
    };
#endif


};


#endif
