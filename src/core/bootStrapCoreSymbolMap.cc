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

BootStrapCoreSymbolMap::BootStrapCoreSymbolMap() {};

void BootStrapCoreSymbolMap::add_package_info(std::string const& pkg, list<std::string> const& used)
{
  this->_PackageUseInfo[pkg] = used;
}

Symbol_sp BootStrapCoreSymbolMap::maybe_allocate_unique_symbol(string const &pkgName, string const &symbolName, bool exportp) {
  Symbol_sp found = this->lookupSymbol(pkgName,symbolName);
  if ( found ) return found;
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

Symbol_sp BootStrapCoreSymbolMap::lookupSymbol(string const &packageName, string const &rawSymbolName, bool inherited) const {
  map<string,list<string>>::const_iterator found = this->_PackageUseInfo.find(packageName);
  for ( auto used_pkg : found->second ) {
      Symbol_sp found = this->lookupSymbol(used_pkg,rawSymbolName,true);
      if (found) return found;
  }
  string fullName = BootStrapCoreSymbolMap::fullSymbolName(packageName, rawSymbolName);
  map<string, int>::const_iterator it = this->_SymbolNamesToIndex.find(fullName);
  if (it == this->_SymbolNamesToIndex.end()) {
    Symbol_sp null;
    return null;
  }
  if ( inherited ) {
    if (this->_IndexToSymbol[it->second]._Export == false) {
      Symbol_sp null;
      return null;
    }
  }
  return ((this->_IndexToSymbol[it->second]._Symbol));
}

void BootStrapCoreSymbolMap::finish_setup_of_symbols() {
  //printf("%s:%d finish_setup_of_symbols\n", __FILE__, __LINE__ );
  int idxEnd = this->_SymbolNamesToIndex.size();
  for (map<string, int>::const_iterator it = this->_SymbolNamesToIndex.begin();
       it != this->_SymbolNamesToIndex.end(); it++) {
    int idx = it->second;
    SymbolStorage &ss = this->_IndexToSymbol[idx];
    string packageName = ss._PackageName;
//    printf("%s:%d  Adding symbol(%s)[%d/%d] to package: %s\n", __FILE__, __LINE__, ss._SymbolName.c_str(), idx, idxEnd, packageName.c_str());
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
