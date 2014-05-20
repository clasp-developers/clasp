#ifndef	StandardObject_H //[
#define StandardObject_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
//#include "model.h"
#include "environment.h"

namespace core {

SMART(StandardClass);


// Set up this class differently

SMART(StandardObject);
class StandardObject_O : public T_O
{
    LISP_META_CLASS(StandardClass);
    LISP_BASE1(T_O);
    LISP_CLASS(core,ClPkg,StandardObject_O,"standard-object");
    DECLARE_INIT();
public:
#if defined(OLD_SERIALIZE)
    void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
	void	initialize();
private:
//	StandardClass_sp	_InstanceClass;
//	Vector0<T_O>		_Slots;
public:
	static StandardObject_sp create(StandardClass_sp instanceClass);
public:

//	static bool static_supportsSlots() {return true;};
//	T_sp& slot_ref( Symbol_sp sym ) throw(SlotRefFailed);

//	void allocate_slot_storage(uint numberOfSlots, T_sp initialValue);


	string __repr__() const;

//	void setInstanceVariableValue(Symbol_sp sym, T_sp obj);
//	T_sp getInstanceVariableValue(Symbol_sp sym);
	
	explicit StandardObject_O();
	virtual ~StandardObject_O();
};


};
TRANSLATE(core::StandardObject_O);
#endif //]
