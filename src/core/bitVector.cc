#define	DEBUG_LEVEL_NONE


#include "core/common.h"
#include "bitVector.h"
#include "hashTable.h"
#include "wrappers.h"


namespace core {


    void BitVector_O::exposeCando(Lisp_sp lisp)
{
    class_<BitVector_O>()
	;

}
    void BitVector_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,BitVector,"","",_lisp)
	;
//    boost::python::def("create_BitVector",&BitVector_O::create);
#endif
}


    EXPOSE_CLASS(core, BitVector_O);



    SimpleBitVector_sp SimpleBitVector_O::create(int size)
    {_G();
        GC_ALLOCATE(SimpleBitVector_O,sbv );
	sbv->_Length = size;
	sbv->bits.resize(((size-1)/CHAR_BIT)+1,0);
	return((sbv));
    }







//
// Constructor
//

    SimpleBitVector_O::SimpleBitVector_O(const SimpleBitVector_O& bv) : BitVector_O(bv)
    {_OF();
	uint	i;
	this->_Length = bv._Length;
	this->bits.resize(bv.bits.size());
	for (i=0; i<this->bits.size(); i++ ) {
	    this->bits[i] = bv.bits[i];
	}
    }

//
// Destructor
//




    void SimpleBitVector_O::rowMajorAset(int idx, T_sp value)
    {_G();
	ASSERTF(idx<this->length(),BF("Index %d is out of range (<%d)") % idx % this->length() );
	this->setBit(idx,value.isTrue());
    }

    T_sp SimpleBitVector_O::rowMajorAref(int idx) const
    {_G();
	ASSERTF(idx<this->length(),BF("Index %d is out of range (<%d)") % idx % this->length() );
	uint val = this->testBit(idx);
	return (val!=0) ? _lisp->_true() : _Nil<T_O>();
    }



void SimpleBitVector_O::getOnIndices(vector<uint>& res)
{_G();
uint		i;
    res.clear();
    for ( i=0; i!=this->vector_length(); i++ )
    {
	if ( this->testBit(i) ) {
	    res.push_back(i);
	}
    }
}
    


bool	SimpleBitVector_O::equal(T_sp obv) const
{
    uint		i;
    if ( !obv.isA<SimpleBitVector_O>() ) return((false));
    SimpleBitVector_sp bv = obv.as<SimpleBitVector_O>();
    if ( this->vector_length() != bv->vector_length() ) return((false));
    for ( i=0; i<this->bits.size(); i++ ) {
	if ( this->bits[i] != bv->bits[i] ) {
	    return((false));
	}
    }
    return((true));
}





void	SimpleBitVector_O::erase()
{
vector<BitBlockType>::iterator	vi;
    for ( vi=this->bits.begin(); vi!=this->bits.end(); vi++ ) {
	(*vi) = 0;
    }
}


void	SimpleBitVector_O::setBit(uint i, uint v)
{_OF();
uint		block;
uint		offset;
BitBlockType	packedVal;
BitBlockType	mask;
BitBlockType	omask;
if ( i>=this->vector_length() ) {
    SIMPLE_ERROR(BF("BitVector index overflow"));
    }
    block = i / CHAR_BIT;
    offset = i % CHAR_BIT;
    omask = ~0;
    mask = (1 << offset)^omask;
    packedVal = v << offset;
    this->bits[block] = (this->bits[block] & mask )|packedVal;
}

uint	SimpleBitVector_O::testBit(uint i) const
{_OF();
    uint		block;
    uint		offset;
    BitBlockType	mask;
    block = i / CHAR_BIT;
    offset = i % CHAR_BIT;
    mask = (1 << offset);

    LOG(BF("testBit i=%u CHAR_BIT=%d block=%d offset=%d") % (i) % (CHAR_BIT) % (block) % (offset) );
    LOG(BF("      mask = |%lx|") % mask  );
    LOG(BF("bits[%04d] = |%lx|") % block % this->bits[block]  );
    BitBlockType result = (this->bits[block]&mask);
    LOG(BF("    result = |%lx|") % result  );
    return((result?1:0));
}


void	SimpleBitVector_O::inPlaceOr(SimpleBitVector_sp bv)
{_OF();
    uint	i;
    if (this->vector_length() != bv->vector_length()) {
	SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
    }
    for ( i = 0; i!= this->bits.size(); i++ ) {
	this->bits[i] |= bv->bits[i];
    }
}


void	SimpleBitVector_O::inPlaceAnd(SimpleBitVector_sp bv)
{_OF();
uint	i;
    if (this->vector_length() != bv->vector_length()) {
	SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
    }
    for ( i = 0; i!= this->bits.size(); i++ ) {
	this->bits[i] &= bv->bits[i];
    }
}

void	SimpleBitVector_O::inPlaceXor(SimpleBitVector_sp bv)
{_OF();
uint	i;
    if (this->vector_length() != bv->vector_length()) {
	SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
    }
    for ( i = 0; i!= this->bits.size(); i++ ) {
	this->bits[i] ^= bv->bits[i];
    }
}


SimpleBitVector_sp	SimpleBitVector_O::bitOr(SimpleBitVector_sp bv)
{
    SimpleBitVector_sp	res = this->deepCopy().as<SimpleBitVector_O>();
    res->inPlaceOr(bv);
    return((res));
}


SimpleBitVector_sp	SimpleBitVector_O::bitAnd(SimpleBitVector_sp bv)
{
    SimpleBitVector_sp	res = this->deepCopy().as<SimpleBitVector_O>();
    res->inPlaceAnd(bv);
    return((res));
}

SimpleBitVector_sp	SimpleBitVector_O::bitXor(SimpleBitVector_sp bv)
{
    SimpleBitVector_sp	res = this->deepCopy().as<SimpleBitVector_O>();
    res->inPlaceXor(bv);
    return((res));
}




uint	SimpleBitVector_O::countSet()
{
uint			i;
uint			c;
    c = 0;
    for ( i=0; i<this->vector_length(); i++ ) {
	if( this->testBit(i) ) c++;
    }
    return((c));
}


string	SimpleBitVector_O::asString()
{
uint			i;
stringstream		s;

    s.str("");
    for ( i=0; i<this->vector_length(); i++ ) {
	s << this->testBit(i);
    }
    return((s.str()));
}


T_sp	SimpleBitVector_O::deepCopy() const
{_OF();
    LOG(BF("About to copy BitVector@%p") % this  );
    GC_COPY(SimpleBitVector_O,n,*this);
    return(n);
}



//
//	dumpToStream
//
//	Dump the BitVector to a stream
//
std::ostream&	SimpleBitVector_O::dumpToStream( std::ostream& out )
{
    uint			i;
    for ( i=0; i<this->vector_length(); i++ ) {
	out << this->testBit(i);
    }
    out << std::endl;
    RET_POD((out));
}


//
//	dump
//
//	Dump the BitVector to a stream
//
void	SimpleBitVector_O::dump()
{
    this->dumpToStream(std::cout);
}





void SimpleBitVector_O::sxhash(HashGenerator& hg) const
    {_OF();
	Bignum bn;
	for ( int i=0; i<this->vector_length(); i++ )
	{
	    if (this->testBit(i))
	    {
		mpz_setbit(bn.get_mpz_t(),i);
	    }
	}
	hg.addPart(bn);
    }


uint	SimpleBitVector_O::lowestIndex()
{
    uint		i;
    for ( i=0; i<this->vector_length(); i++ ) {
	if ( this->testBit(i) ) {
	    return((i));
	}
    }
    return((i));
}


    void SimpleBitVector_O::exposeCando(Lisp_sp lisp)
{
    class_<SimpleBitVector_O>()
//	.def("equal",&SimpleBitVector_O::equal)
	.def("core:setBit", &SimpleBitVector_O::setBit)
	.def("core:testBit", &SimpleBitVector_O::testBit)
	.def("core:inPlaceOr", &SimpleBitVector_O::inPlaceOr)
	.def("core:inPlaceAnd", &SimpleBitVector_O::inPlaceAnd)
	.def("core:inPlaceXor", &SimpleBitVector_O::inPlaceXor)
	.def("core:bitOr", &SimpleBitVector_O::bitOr)
	.def("core:bitAnd", &SimpleBitVector_O::bitAnd)
	.def("core:bitXor", &SimpleBitVector_O::bitXor)
	.def("core:countSet", &SimpleBitVector_O::countSet)
	.def("core:isZero", &SimpleBitVector_O::isZero)
	.def("core:lowestIndex", &SimpleBitVector_O::lowestIndex)
	.def("core:dump", &SimpleBitVector_O::dump)
	.def("core:SimpleBitVector-asString",&SimpleBitVector_O::asString)
	;

}
    void SimpleBitVector_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,BitVector,"","",_lisp)
//	.def("equal",&SimpleBitVector_O::equal)
	.def("setBit", &SimpleBitVector_O::setBit)
	.def("testBit", &SimpleBitVector_O::testBit)
	.def("inPlaceOr", &SimpleBitVector_O::inPlaceOr)
	.def("inPlaceAnd", &SimpleBitVector_O::inPlaceAnd)
	.def("inPlaceXor", &SimpleBitVector_O::inPlaceXor)
	.def("bitOr", &SimpleBitVector_O::bitOr)
	.def("bitAnd", &SimpleBitVector_O::bitAnd)
	.def("bitXor", &SimpleBitVector_O::bitXor)
	.def("countSet", &SimpleBitVector_O::countSet)
	.def("isZero", &SimpleBitVector_O::isZero)
	.def("lowestIndex", &SimpleBitVector_O::lowestIndex)
	.def("dump", &SimpleBitVector_O::dump)
	.def("asString",&SimpleBitVector_O::asString)
	;
//    boost::python::def("create_BitVector",&SimpleBitVector_O::create);
#endif
}


    EXPOSE_CLASS(core, SimpleBitVector_O);








};

