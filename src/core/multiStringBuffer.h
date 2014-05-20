#ifndef	MultiStringBuffer_H //[
#define MultiStringBuffer_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "numerics.h"
#include "object.h"


namespace core {



static const uint MultiStringBlockPowerOf2 = 22;	 // Four megabyte blocks for strings
static const uint MultiStringBlockSize = (1<<MultiStringBlockPowerOf2);
static const uint MultiStringBlockLocalMask = MultiStringBlockSize-1;

class MultiStringBlock
{
private:
    uint	_BlockId;
    char*	_BlockStart;
    uint	_NextStringIndex;
    uint	_NumberOfStrings;
public:
	/*! Return a pointer to the string with localIndex
	 */
    const char*	getString(uint localIndex);

    	/*! Return the index if the string was added and
	 * UndefinedUnsignedInt if there wasn't enough room.
	 */
    uint addCharacters(const char* str);

		/*! Add only the number of characters from the string 
		 * specified.
		 */
    uint addNumberOfCharacters(const char* str, uint num);

    void dump(uint startIdx,std::ostream& ss);

    MultiStringBlock(uint blockId);
    virtual ~MultiStringBlock();
};




SMART(MultiStringBuffer );
class MultiStringBuffer_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,MultiStringBuffer_O,"MultiStringBuffer");

public: // virtual functions inherited from Object
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
//	string	__repr__() const;

private: // instance variables
	vector<MultiStringBlock*>	_Blocks;

public:	// Creation class functions

public:

		/*! Add the string and return the index for the string
		 */
	uint	addCharacters(const char* str);


		/*! Add only the number of characters from the string 
		 * specified.
		 */
	uint	addNumberOfCharacters(const char* str, uint num);

		/*! Add the string and return the index for the string
		 */
	uint	addString(const string& str);

		/*! Return a pointer to the string
		 * This is fast.
		 */
	const char*	getCharacters(uint index);

		/*! Return a string that contains a copy
		 * of the characters.
		 * This is slow and should only be used for scripting.
		 */
	string	getString(uint index);

    void dumpToStream(std::ostream& ss);
    void dump();

	/*! Dump the memory usage and return the total
	 * number of bytes
	 */
    LongLongInt describeMemoryUsage();

    DEFAULT_CTOR_DTOR(MultiStringBuffer_O);
};


};
TRANSLATE(core::MultiStringBuffer_O);
#endif //]
