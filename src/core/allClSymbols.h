#ifndef core_allClSymbols_H
#define core_allClSymbols_H

#ifdef DEBUG_CL_SYMBOLS
namespace core {

    void throwIfNotValidClSymbol(const string& name);
    void initializeAllClSymbols();
    void initializeAllClSymbolsFunctions();

};
#endif
#endif
