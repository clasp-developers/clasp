       
       
#ifndef	IntArray_H //[
#define IntArray_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"


namespace core {



SMART(IntArray);
SMART(IntArray);
class IntArray_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,IntArray_O,"IntArray");

public:
	void initialize();
public:
#if defined(XML_ARCHIVE)
	void	archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)
private:
	vector<int>	_Ints;

public:
	typedef	vector<int>::iterator	iterator;

public:
	static IntArray_sp create(uint sz,Lisp_sp);
public:

	iterator begin() { return this->_Ints.begin(); };
	iterator end() { return this->_Ints.end(); };

	void	resize(unsigned sz);
	void	clear();

	void	append(int val);

	uint	size() { return this->_Ints.size(); };

	int	get(unsigned idx);
	void	put(unsigned idx, int val);


	IntArray_O( const IntArray_O& ss ); //!< Copy constructor


	DEFAULT_CTOR_DTOR(IntArray_O);
};




};

TRANSLATE(core::IntArray_O);

#endif //]
