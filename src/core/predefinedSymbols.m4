
changecom(`//')

//
// m4 macro declarations here
//
//
define(`_symbolCount',0)
define(`fExterns',1)
define(`fStorage',2)
define(`fCode',3)



define(`basicSymbol',`
divert(fExterns)
const uint $1 = _symbolCount;
divert(fStorage)
 /* const uint $1 = _symbolCount; */
divert(fCode)
    LOG(BF("predefining symbol(%s) in package(%s)") % "$3" % $2 );
    lisp->createPredefinedSymbol($1,$2,"$3");
divert
    define(`_symbolCount',incr(_symbolCount))
')
define(`kwSym',`basicSymbol'(_sym_kw_`$1',":",$1))
define(`packagedSymbol',`basicSymbol'(_sym_`'$1`_'$2,$1,$2))
define(`specialPackagedSymbol',`basicSymbol'(_sym_`'$1`'_`'$2,$1,$3))


// --------------------------------------------------------
// --------------------------------------------------------
// --------------------------------------------------------
// --------------------------------------------------------
// --------------------------------------------------------
// --------------------------------------------------------
//
//
// Declare symbols here
//

//
// lisp symbols
//
# specialPackagedSymbol(ClPackage,StarPATHStar,*PATH*)
# specialPackagedSymbol(ClPackage,StarARGSStar,*ARGS*)
# packagedSymbol(ClPackage,foreach)
# kwSym(debug)
# kwSym(lambda)


// energyFunction symbols
kwSym(nonbondTerm)
kwSym(restraintAnchor)

//
// Render symbols
//
packagedSymbol(MbbPackage,elementColors)


packagedSymbol(MbbPackage,bondOrderToSymbolConverter)
packagedSymbol(MbbPackage,elementToSymbolConverter)
packagedSymbol(MbbPackage,hybridizationToSymbolConverter)
packagedSymbol(MbbPackage,scorerOperationsToSymbolConverter)
packagedSymbol(MbbPackage,scorerOperationsUseArgs)
packagedSymbol(MbbPackage,simulatedAnnealingSearchTypeConverter)
packagedSymbol(MbbPackage,atomFlagSymbolConverter)
packagedSymbol(MbbPackage,iterateMatterSymbolConverter)
packagedSymbol(MbbPackage,preconditionerTypeConverter)
packagedSymbol(MbbPackage,minimizerStatusConverter)
packagedSymbol(MbbPackage,bondOrderConverter)
packagedSymbol(MbbPackage,configurationEnumConverter)
packagedSymbol(MbbPackage,stereochemistryTypeConverter)


//
// superposeEngine.cc
//
kwSym(superpose)

// chemdraw symbols
kwSym(name)

// rendering symbols
kwSym(renderStyle)
kwSym(ballAndStick)
kwSym(line)
kwSym(cpk)
kwSym(showLabel)
kwSym(label)
kwSym(colorByElement)
kwSym(color)


//
// m4 code to generate .inc file here
//

#ifdef	PredefinedSymbol_externs
undivert(fExterns)
#undef PredefinedSymbol_externs
#endif
#ifdef	PredefinedSymbol_storage
undivert(fStorage)
#undef PredefinedSymbol_storage
#endif
#ifdef	PredefinedSymbol_code
    lisp->allocatePredefinedSymbols(_symbolCount);
undivert(fCode)
#undef PredefinedSymbol_code
#endif
