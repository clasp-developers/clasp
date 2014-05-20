


//
// m4 macro declarations here
//
//













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
# 

    

# 

    

# 

    

# 

    

# 

    



// energyFunction symbols


    



    


//
// Render symbols
//


    





    



    



    



    



    



    



    



    



    



    



    



    



    



//
// superposeEngine.cc
//


    


// chemdraw symbols


    


// rendering symbols


    



    



    



    



    



    



    



    



//
// m4 code to generate .inc file here
//

#ifdef	PredefinedSymbol_externs

const uint _sym_ClPackage_StarPATHStar = 0;

const uint _sym_ClPackage_StarARGSStar = 1;

const uint _sym_ClPackage_foreach = 2;

const uint _sym_kw_debug = 3;

const uint _sym_kw_lambda = 4;

const uint _sym_kw_nonbondTerm = 5;

const uint _sym_kw_restraintAnchor = 6;

const uint _sym_MbbPackage_elementColors = 7;

const uint _sym_MbbPackage_bondOrderToSymbolConverter = 8;

const uint _sym_MbbPackage_elementToSymbolConverter = 9;

const uint _sym_MbbPackage_hybridizationToSymbolConverter = 10;

const uint _sym_MbbPackage_scorerOperationsToSymbolConverter = 11;

const uint _sym_MbbPackage_scorerOperationsUseArgs = 12;

const uint _sym_MbbPackage_simulatedAnnealingSearchTypeConverter = 13;

const uint _sym_MbbPackage_atomFlagSymbolConverter = 14;

const uint _sym_MbbPackage_iterateMatterSymbolConverter = 15;

const uint _sym_MbbPackage_preconditionerTypeConverter = 16;

const uint _sym_MbbPackage_minimizerStatusConverter = 17;

const uint _sym_MbbPackage_bondOrderConverter = 18;

const uint _sym_MbbPackage_configurationEnumConverter = 19;

const uint _sym_MbbPackage_stereochemistryTypeConverter = 20;

const uint _sym_kw_superpose = 21;

const uint _sym_kw_name = 22;

const uint _sym_kw_renderStyle = 23;

const uint _sym_kw_ballAndStick = 24;

const uint _sym_kw_line = 25;

const uint _sym_kw_cpk = 26;

const uint _sym_kw_showLabel = 27;

const uint _sym_kw_label = 28;

const uint _sym_kw_colorByElement = 29;

const uint _sym_kw_color = 30;

#undef PredefinedSymbol_externs
#endif
#ifdef	PredefinedSymbol_storage

 /* const uint _sym_ClPackage_StarPATHStar = 0; */

 /* const uint _sym_ClPackage_StarARGSStar = 1; */

 /* const uint _sym_ClPackage_foreach = 2; */

 /* const uint _sym_kw_debug = 3; */

 /* const uint _sym_kw_lambda = 4; */

 /* const uint _sym_kw_nonbondTerm = 5; */

 /* const uint _sym_kw_restraintAnchor = 6; */

 /* const uint _sym_MbbPackage_elementColors = 7; */

 /* const uint _sym_MbbPackage_bondOrderToSymbolConverter = 8; */

 /* const uint _sym_MbbPackage_elementToSymbolConverter = 9; */

 /* const uint _sym_MbbPackage_hybridizationToSymbolConverter = 10; */

 /* const uint _sym_MbbPackage_scorerOperationsToSymbolConverter = 11; */

 /* const uint _sym_MbbPackage_scorerOperationsUseArgs = 12; */

 /* const uint _sym_MbbPackage_simulatedAnnealingSearchTypeConverter = 13; */

 /* const uint _sym_MbbPackage_atomFlagSymbolConverter = 14; */

 /* const uint _sym_MbbPackage_iterateMatterSymbolConverter = 15; */

 /* const uint _sym_MbbPackage_preconditionerTypeConverter = 16; */

 /* const uint _sym_MbbPackage_minimizerStatusConverter = 17; */

 /* const uint _sym_MbbPackage_bondOrderConverter = 18; */

 /* const uint _sym_MbbPackage_configurationEnumConverter = 19; */

 /* const uint _sym_MbbPackage_stereochemistryTypeConverter = 20; */

 /* const uint _sym_kw_superpose = 21; */

 /* const uint _sym_kw_name = 22; */

 /* const uint _sym_kw_renderStyle = 23; */

 /* const uint _sym_kw_ballAndStick = 24; */

 /* const uint _sym_kw_line = 25; */

 /* const uint _sym_kw_cpk = 26; */

 /* const uint _sym_kw_showLabel = 27; */

 /* const uint _sym_kw_label = 28; */

 /* const uint _sym_kw_colorByElement = 29; */

 /* const uint _sym_kw_color = 30; */

#undef PredefinedSymbol_storage
#endif
#ifdef	PredefinedSymbol_code
    lisp->allocatePredefinedSymbols(31);

    LOG(BF("predefining symbol(%s) in package(%s)") % "*PATH*" % ClPackage );
    lisp->createPredefinedSymbol(_sym_ClPackage_StarPATHStar,ClPackage,"*PATH*");

    LOG(BF("predefining symbol(%s) in package(%s)") % "*ARGS*" % ClPackage );
    lisp->createPredefinedSymbol(_sym_ClPackage_StarARGSStar,ClPackage,"*ARGS*");

    LOG(BF("predefining symbol(%s) in package(%s)") % "foreach" % ClPackage );
    lisp->createPredefinedSymbol(_sym_ClPackage_foreach,ClPackage,"foreach");

    LOG(BF("predefining symbol(%s) in package(%s)") % "debug" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_debug,":","debug");

    LOG(BF("predefining symbol(%s) in package(%s)") % "lambda" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_lambda,":","lambda");

    LOG(BF("predefining symbol(%s) in package(%s)") % "nonbondTerm" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_nonbondTerm,":","nonbondTerm");

    LOG(BF("predefining symbol(%s) in package(%s)") % "restraintAnchor" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_restraintAnchor,":","restraintAnchor");

    LOG(BF("predefining symbol(%s) in package(%s)") % "elementColors" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_elementColors,MbbPackage,"elementColors");

    LOG(BF("predefining symbol(%s) in package(%s)") % "bondOrderToSymbolConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_bondOrderToSymbolConverter,MbbPackage,"bondOrderToSymbolConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "elementToSymbolConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_elementToSymbolConverter,MbbPackage,"elementToSymbolConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "hybridizationToSymbolConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_hybridizationToSymbolConverter,MbbPackage,"hybridizationToSymbolConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "scorerOperationsToSymbolConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_scorerOperationsToSymbolConverter,MbbPackage,"scorerOperationsToSymbolConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "scorerOperationsUseArgs" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_scorerOperationsUseArgs,MbbPackage,"scorerOperationsUseArgs");

    LOG(BF("predefining symbol(%s) in package(%s)") % "simulatedAnnealingSearchTypeConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_simulatedAnnealingSearchTypeConverter,MbbPackage,"simulatedAnnealingSearchTypeConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "atomFlagSymbolConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_atomFlagSymbolConverter,MbbPackage,"atomFlagSymbolConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "iterateMatterSymbolConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_iterateMatterSymbolConverter,MbbPackage,"iterateMatterSymbolConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "preconditionerTypeConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_preconditionerTypeConverter,MbbPackage,"preconditionerTypeConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "minimizerStatusConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_minimizerStatusConverter,MbbPackage,"minimizerStatusConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "bondOrderConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_bondOrderConverter,MbbPackage,"bondOrderConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "configurationEnumConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_configurationEnumConverter,MbbPackage,"configurationEnumConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "stereochemistryTypeConverter" % MbbPackage );
    lisp->createPredefinedSymbol(_sym_MbbPackage_stereochemistryTypeConverter,MbbPackage,"stereochemistryTypeConverter");

    LOG(BF("predefining symbol(%s) in package(%s)") % "superpose" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_superpose,":","superpose");

    LOG(BF("predefining symbol(%s) in package(%s)") % "name" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_name,":","name");

    LOG(BF("predefining symbol(%s) in package(%s)") % "renderStyle" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_renderStyle,":","renderStyle");

    LOG(BF("predefining symbol(%s) in package(%s)") % "ballAndStick" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_ballAndStick,":","ballAndStick");

    LOG(BF("predefining symbol(%s) in package(%s)") % "line" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_line,":","line");

    LOG(BF("predefining symbol(%s) in package(%s)") % "cpk" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_cpk,":","cpk");

    LOG(BF("predefining symbol(%s) in package(%s)") % "showLabel" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_showLabel,":","showLabel");

    LOG(BF("predefining symbol(%s) in package(%s)") % "label" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_label,":","label");

    LOG(BF("predefining symbol(%s) in package(%s)") % "colorByElement" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_colorByElement,":","colorByElement");

    LOG(BF("predefining symbol(%s) in package(%s)") % "color" % ":" );
    lisp->createPredefinedSymbol(_sym_kw_color,":","color");

#undef PredefinedSymbol_code
#endif
