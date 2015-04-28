/*
    File: bootStrapCoreSymbolMap.h
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
#ifndef	_core_bootStrapCoreSymbolMap_H
#define _core_bootStrapCoreSymbolMap_H

namespace core
{

    class SymbolStorage
    {
    public:
	string		_PackageName;
	string		_SymbolName;
	Symbol_sp	_Symbol;
	bool 		_Export;
	SymbolStorage() : _PackageName(""), _SymbolName(""), _Export(false)
	{
	    this->_Symbol.reset_();
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
