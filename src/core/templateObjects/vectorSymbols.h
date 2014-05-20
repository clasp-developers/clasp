#ifndef	_core_VectorSymbols_H
#define _core_VectorSymbols_H

#include "core/object.h"
#include "core/lispVector.h"
#include "core/vectorTemplate.h"
#include "corePackage.fwd.h"

namespace core
{

    FORWARD(VectorSymbols);
    class VectorSymbols_O : public Vector_template_O<Symbol_O>
    {
	LISP_TEMPLATE_BASE1(Vector_template_O<Symbol_O>);
	LISP_CLASS(CorePkg,VectorSymbols_O,"VectorSymbols");
    public:
	VectorSymbols_O() : T_O(), Vector_template_O<Symbol_O>() {};
	virtual ~VectorSymbols_O() {};
    public:
	static VectorSymbols_sp make(Symbol_sp initial_element, Cons_sp initial_values, int dimension );

    public: // Functions here
	bool arraySymbolsP() const { return this->isObject();};
	bool vectorSymbolsP() const { return this->isObject();};
	T_sp elementType() const;

    };

}; /* core */

TRANSLATE(core::VectorSymbols_O);

#endif /* _core_VectorSymbols_H */


