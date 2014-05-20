       

#define	DEBUG_LEVEL_NONE

#include "lisp.h"
#include "intArray.h"
#include "wrappers.h"




namespace core {


    REGISTER_CLASS(core,IntArray_O);


//
// Constructor
//

void	IntArray_O::initialize()
{
    this->Base::initialize();
    this->_Ints.clear();
}

#if defined(XML_ARCHIVE)
void	IntArray_O::archive(core::ArchiveP node)
{
    node->archiveVectorInt( "values", this->_Ints );
}
#endif // defined(XML_ARCHIVE)

IntArray_sp IntArray_O::create(uint sz,Lisp_sp env)
{
    IntArray_sp ir = IntArray_O::create();
    ir->resize(sz);
    return ir;
}


void	IntArray_O::clear()
{_G();
    this->_Ints.clear();
}


void	IntArray_O::resize(uint sz)
{_G();
    ASSERT(sz>0);
    this->_Ints.resize(sz);
}


void	IntArray_O::append(int val)
{
    this->_Ints.push_back(val);
}



void	IntArray_O::put(uint idx, int val)
{_G();
    ASSERT_lessThan(idx,this->_Ints.size());
    this->_Ints[idx] = val;
}



int	IntArray_O::get(uint idx)
{_G();
    ASSERT_lessThan(idx,this->_Ints.size());
    return this->_Ints[idx];
}


};
