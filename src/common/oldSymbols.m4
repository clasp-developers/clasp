//
// Set the current package
//
changecom(`/*',`*/')
define(`currentPackage',`
define(`_currentPackage_',$1)
')


//
// Divert code
//
define(`extern',1)
define(`static',2)
define(`create',3)


//
// expand expands a symbol name
// Form: expand(PPP,XXX)
// where PPP is the package name and XXX is the symbol name
//
define(`expand',`_sym_$1_$2')

define(`kw_expand',`_kw_$2')

//define(`sym_expand',`_sym_$2')


//
// Define a symbol with this macro
// form: symbol(XXX)
//
// This will generate the code:
// EXTERN_PREDEFINED_SYMBOL(_sym_PPP_XXX)
// and
// STATIC_PREDEFINED_SYMBOL(_sym_PPP_XXX)
// and
// CREATE_PREDEFINED_SYMBOL(_sym_PPP_XXX,PPP,"_sym_PPP_XXX")

define(`symbolBase',`
divert(extern)
EXTERN_PREDEFINED_SYMBOL(expand($1,$2));
divert(static)
STATIC_PREDEFINED_SYMBOL(expand($1,$2));
divert(create)
CREATE_PREDEFINED_SYMBOL(expand($1,$2)`,'$3`,"'$4`"');
divert
')


define(`sym_Base',`
divert(extern)
EXTERN_PREDEFINED_SYMBOL(sym_expand($1,$2));
divert(static)
STATIC_PREDEFINED_SYMBOL(sym_expand($1,$2));
divert(create)
CREATE_PREDEFINED_SYMBOL(sym_expand($1,$2)`,'$3`,"'$4`"');
divert
')



define(`symbol',`symbolBase('_currentPackage_`,'$1`,'_currentPackage_`,'$1`)')
define(`symbol2',`symbolBase('_currentPackage_`,'$1`,'_currentPackage_`,'$2`)')
define(`keyword',`symbolBase(kw,'$1`,KeywordPackage,'$1`)')


//define(`sym',`sym_Base('_currentPackage_`,'$1`,'_currentPackage_`,'$1`)')
//define(`sym2',`sym_Base('_currentPackage_`,'$1`,'_currentPackage_`,'$2`)')
//define(`kw',`kw_Base(kw,'$1`,KeywordPackage,'$1`)')



define(`finish',`
#ifdef SYMBOLS_CREATE
undivert(create)
#undef SYMBOLS_CREATE
#endif

#ifdef SYMBOLS_EXTERN
undivert(extern)
#undef SYMBOLS_EXTERN
#endif

#ifdef SYMBOLS_STATIC
undivert(static)
#undef SYMBOLS_STATIC
#endif
')










/* currentPackage(KinPkg) - set the package name */
/* symbol(AAAA)  - define a package symbol */
/* keyword(BBBB) - define a keyword symbol */
/* finish()  - write everything out */