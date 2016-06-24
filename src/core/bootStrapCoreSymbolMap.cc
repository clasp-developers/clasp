/*
    File: bootStrapCoreSymbolMap.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/extensionPackage.fwd.h>
#include <clasp/core/cleavirPrimopsPackage.fwd.h>
#include <clasp/core/cleavirEnvPackage.fwd.h>
#include <clasp/core/package.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/bootStrapCoreSymbolMap.h>

namespace core {

string BootStrapCoreSymbolMap::fullSymbolName(string const &packageName, string const &symbolName) {
  string fullName = packageName + "::" + symbolName;
  return ((fullName));
}

BootStrapCoreSymbolMap::BootStrapCoreSymbolMap() {
//	this->attachToGCRoot();
//#define LOOKUP_SYMBOL(pkgName,symName) bootStrapSymbolMap.lookupSymbol(pkgName,symName)

//printf("%s:%d BootStrapCoreSymbolMap\n", __FILE__, __LINE__ );

#define ClPkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) cl::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef ClPkg_SYMBOLS
#define CorePkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) core::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef CorePkg_SYMBOLS

#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) kw::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS

#define ExtPkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) ext::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef ExtPkg_SYMBOLS

#define ClosPkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) clos::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef ClosPkg_SYMBOLS

#define CleavirPrimopsPkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) cleavirPrimops::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef CleavirPrimopsPkg_SYMBOLS

#define CleavirEnvPkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) cleavirEnv::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef CleavirEnvPkg_SYMBOLS

#define GrayPkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) gray::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef GrayPkg_SYMBOLS

#define CompPkg_SYMBOLS
#define DO_SYMBOL(cname, rsid, pkgName, symName, exportp) comp::cname = this->allocate_unique_symbol(pkgName, core::lispify_symbol_name(symName), exportp);
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef CompPkg_SYMBOLS
};

Symbol_sp BootStrapCoreSymbolMap::allocate_unique_symbol(string const &pkgName, string const &symbolName, bool exportp) {
  string name = BootStrapCoreSymbolMap::fullSymbolName(pkgName, symbolName);
  map<string, int>::iterator it = this->_SymbolNamesToIndex.find(name);
  if (it != this->_SymbolNamesToIndex.end()) {
    return ((this->_IndexToSymbol[it->second]._Symbol));
  }
  Symbol_sp sym = Symbol_O::create(symbolName);
  SymbolStorage store(pkgName, symbolName, sym, exportp);
  int index = this->_IndexToSymbol.size();
  this->_IndexToSymbol.push_back(store);
  this->_SymbolNamesToIndex[name] = index;
  return (sym);
}

Symbol_sp BootStrapCoreSymbolMap::lookupSymbol(string const &packageName, string const &rawSymbolName) const {
  string symbolName = rawSymbolName;
  string fullName = BootStrapCoreSymbolMap::fullSymbolName(packageName, symbolName);
  map<string, int>::const_iterator it = this->_SymbolNamesToIndex.find(fullName);
  if (it == this->_SymbolNamesToIndex.end()) {
    THROW_HARD_ERROR(BF("In BootStrapCoreSymbolMap::lookupSymbol Unknown symbolName[%s]") % fullName);
  }
  return ((this->_IndexToSymbol[it->second]._Symbol));
}

void BootStrapCoreSymbolMap::finish_setup_of_symbols() {
  _G();
  //printf("%s:%d finish_setup_of_symbols\n", __FILE__, __LINE__ );
  int idxEnd = this->_SymbolNamesToIndex.size();
  for (map<string, int>::const_iterator it = this->_SymbolNamesToIndex.begin();
       it != this->_SymbolNamesToIndex.end(); it++) {
    int idx = it->second;
    SymbolStorage &ss = this->_IndexToSymbol[idx];
    string packageName = ss._PackageName;
    //    printf("%s:%d  Adding symbol[%d/%d] to package: %s\n", __FILE__, __LINE__, idx, idxEnd, packageName.c_str());
    Package_sp pkg = gc::As<Package_sp>(_lisp->findPackage(packageName, true));
    //    printf("%s:%d  The package most derived pointer base address adding symbol to: %p\n", __FILE__, __LINE__, pkg.raw_());
    //            printf("%s:%d  The symbol index is %d\n", __FILE__, __LINE__, idx );
    ss._Symbol->finish_setup(pkg, ss._Export);
  }
}

void BootStrapCoreSymbolMap::dump() {
  for (map<string, int>::const_iterator it = this->_SymbolNamesToIndex.begin();
       it != this->_SymbolNamesToIndex.end(); it++) {
    string ts = it->first;
    printf("%s\n", ts.c_str());
    SymbolStorage &ss = this->_IndexToSymbol[it->second];
    printf("    _PackageName: %s   _SymbolName: %s   _Export: %d\n",
           ss._PackageName.c_str(), ss._SymbolName.c_str(), ss._Export);
  }
}
};
