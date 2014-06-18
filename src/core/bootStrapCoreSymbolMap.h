#ifndef	_core_bootStrapCoreSymbolMap_H
#define _core_bootStrapCoreSymbolMap_H

namespace core
{

    struct SymbolStorage
    {
	string		_PackageName;
	string		_SymbolName;
	Symbol_sp	_Symbol;
	bool 		_Export;
	SymbolStorage() : _PackageName(""), _SymbolName(""), _Export(false)
	{
	    this->_Symbol.reset();
	}

	SymbolStorage(string const& pkgName, string const& symbolName,Symbol_sp sym, bool exportp)
	    : _PackageName(pkgName), _SymbolName(symbolName), _Symbol(sym), _Export(exportp) {};

	SymbolStorage(SymbolStorage const& orig)
	{
	    this->_PackageName = orig._PackageName;
	    this->_SymbolName = orig._SymbolName;
	    this->_Symbol = orig._Symbol;
	    this->_Export = orig._Export;
	}

        DECLARE_onHeapScanGCRoots();
    };
};




namespace core {

    class BootStrapCoreSymbolMap // : public gctools::StackRoot
    {
    private:
	map<string,int>                 _SymbolNamesToIndex;
        gctools::Vec0<SymbolStorage>	_IndexToSymbol;
    private:
	static string fullSymbolName(string const& packageName, string const& symbolName);
    public:
	BootStrapCoreSymbolMap();

	void finish_setup_of_symbols();

	Symbol_sp allocate_unique_symbol(string const& pkgName,string const& symbolName, bool exportp=false);

	/*! Throw an exception if symbol not found */
        Symbol_sp lookupSymbol(string const& packageName, string const& symbolName) const;

	void dump();

    };


};



#endif

