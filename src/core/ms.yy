       
       
       
/*!
__BEGIN_DOC( msmarts, MSMARTS chemical pattern matching)

Based on SMARTS documentation at 
\begin{verbatim}
http://www.daylight.com/dayhtml/doc/theory/theory.smarts.html
\end{verbatim}

MSMARTS is similar to SMARTS with the following differences.
\begin{itemize}
	\item MSMARTS supports atom tags: numerical labels that can be attached to atoms as an MSMARTS substructure is matched to a molecule. The tagged atoms can then be referenced after the substructure is matched.

	For example: the MSMARTS string "[N\&H1]1C2(=O3)" will recognize a secondary amide and the amide nitrogen, carbonyl carbon and carbonyl oxygen can be obtained using the tags "1", "2" and "3" after a successful match.
	\item The syntax for identifying rings is different. Rings are recognized with strings like: "C1CCC[C\&?1]".  The first "1" assigns a tag "1" to the first carbon, The "[C\&?1]" atom tests if the atom is carbon and has the tag "1".
\end{itemize}

   Substructure searching, the process of finding a particular pattern (subgraph) in a molecule (graph), is one of the most important tasks for computers in chemistry. It is used in virtually every application that employs a digital representation of a molecule, including depiction (to highlight a particular functional group), drug design (searching a database for similar structures and activity), analytical chemistry (looking for previously-characterized structures and comparing their data to that of an unknown), and a host of other problems.

   MSMARTS expressions allow a chemist to specify substructures using rules that 
   are straightforward extensions of SMILES. For example: to search a database 
   for phenol-containing structures, one would use the SMARTS string 
   [OH]c1cccc[c\&?1], which is similar to SMILES (Note: the [c\&?1] atom 
   primative test is used to identify rings in MSMARTS). In fact, almost all 
   SMILES specifications are valid SMARTS targets. Using SMARTS, flexible 
   and efficient substructure-search specifications can be made in terms 
   that are meaningful to chemists.

   In the SMILES language, there are two fundamental types of symbols: atoms and bonds. Using these SMILES symbols, once can specify a molecule's graph (its "nodes" and "edges") and assign "labels" to the components of the graph (that is, say what type of atom each node represents, and what type of bond each edge represents).

   The same is true in SMARTS: One uses atomic and bond symbols to specify a graph. However, in SMARTS the labels for the graph's nodes and edges (its "atoms" and "bonds") are extended to include "logical operators" and special atomic and bond symbols; these allow SMARTS atoms and bonds to be more general. For example, the SMARTS atomic symbol [C,N] is an atom that can be aliphatic C or aliphatic N; the SMARTS bond symbol ~ (tilde) matches any bond.

Below is example code that uses SMARTS to find every amide bond in a molecule:
\begin{verbatim}
#
# Define a ChemInfo object that can carry out
# substructure searches
#
( amideSmarts = [ new ChemInfo] )
#
# Compile a substructure pattern using SMARTS code
#
( amideSmarts compileSmarts "N1~C2=O3" )
# 
# Load a molecule
#
( p53 = [ loadMol2 "p53.mol2" ] )
#
# Iterate through every atom and if it matches
# the substructure search then extract the tagged
# atoms and print their names
#
[foreach a [ atoms p53 ] [block
    [ if ( amideSmarts matches a ) [block 
        [println ( "-----Matching atom: %s" % ( a getName ) ) ]
        ( coAtom = ( amideSmarts getAtomWithTag "2" ) )
        ( oAtom = ( amideSmarts getAtomWithTag "3" ) )
        [ println ( "    Carbonyl carbon: %s" % ( coAtom getName ) ) ]
        [ println ( "    Carbonyl oxygen: %s" % ( oAtom getName ) ) ]
    ] ]
] ]
\end{verbatim}
__END_DOC
*/



%{
#define DEBUG_LEVEL_FULL

#include<iostream>
#include<string>
#include<vector>
#include <istream>

#include "foundation.h"
#include "object.h"
#include "aggregate.h"
#include "molecule.h"
#include "residue.h"
#include "atom.h"

#include "chemInfo.h"
#include "hold.h"


using namespace std;
using namespace mbb;

// This defines the stream that we are reading from

#include "msmarts_TypeParser.h"
#include "msmarts_ParserParam.h"
#include "msmarts_LocationType.h"

#define	LEXDEBUG	1

#ifdef	LEXDEBUG
#define	LEXPRINT(l,x) {lisp_LOG(l,BF( "Token: %s") %(x));}
#define	LEXDPRINT(l,x) {lisp_LOG(l,x)};
#else
#define	LEXPRINT(l,x)
#define	LEXDPRINT(l,x)
#endif
#define	MAX_CVAL	16

#define MP() (data)

void msmarts_error(YYLTYPE* yyloc, msmarts_SParserParam* data, const char* message );
int	msmarts_lex(YYSTYPE* yylval, YYLTYPE* yylloc, msmarts_SParserParam* data);

%}

%name-prefix="msmarts_"
%define api.pure
%locations
%lex-param {msmarts_SParserParam* data}
%parse-param {msmarts_SParserParam* data}


%token APEndOfLine
%token <benum> APBond
%token APAtomicNumber
%token APChiralityAntiClockwise
%token APChiralityClockwise
%token APCloseBracket
%token APCloseParenthesis
%token APConnectivity
%token APDegree
%token APRingTest
%token APResidueTest
//%token APDirectionalSingleDown
//%token APDirectionalSingleUp
%token APDollarSign
%token APDoubleBond
%token <eval> APOrganicElement
%token <eval> APInorganicElement
%token APElectronegativeElement
%token APError
%token APGroupNumber  
%token APHeavyAtomTotalBond
%token APImplicitHCount
%token APLonePair
%token APNegativeCharge
%token APNegativeFormalCharge   
%token <cval> APNumber
%token APOpenBracket
%token APOpenParenthesis
%token APOperatorAndHigh
%token APOperatorAndLow
%token APOperatorNot
%token APOperatorOr
%token APPiBondOrbital
%token APAromaticPiElectron
%token APPositiveCharge
%token APPositiveFormalCharge
%token APRingMembershipCount
%token APRingSize
//%token APSingleBond
//%token APSingleDownOrUnspecified
//%token APSingleUpOrUnspecified
%token APTotalHCount
%token APTotalBondNumber		//Explicit + Implicit bonds 
%token APTotalExplicitBondNumber	//Explicit bonds only
%token APTransitionMetal
//%token APTripleBond
%token APValence
%token APWildCard

%left	APOperatorAndLow
%left	APOperatorOr
%left	APOperatorAndHigh
%left	APOperatorNot



//%type <smartsRoot>		input
%type <smartsRoot>		chemInfo
%type <root>			recursiveChemInfo
%type <ival>		intNumber
%type <bondListMatchNode>	chain branch
%type <atomOrBondMatchNode>	atomTest atomPrimativeTest
%type <bondTest>		bondAtomTest
%type <atomOrBondMatchNode>	acyclicAtomTest
%type <logical>			logOp

//%destructor	{ if ($$!=NULL) delete ($$);} 	input
%destructor	{ if ($$!=NULL) delete ($$);} 	chemInfo
%destructor { if ($$!=NULL) delete ($$); } 	recursiveChemInfo
%destructor { if ($$!=NULL) delete ($$); } 	chain
%destructor { if ($$!=NULL) delete ($$); } 	branch
%destructor { if ($$!=NULL) delete ($$); } 	atomTest
%destructor { if ($$!=NULL) delete ($$); } 	atomPrimativeTest
%destructor { if ($$!=NULL) delete ($$); } 	bondAtomTest
%destructor { if ($$!=NULL) delete ($$); } 	acyclicAtomTest
%destructor { if ($$!=NULL) delete ($$); } 	logOp
%%


input:  chemInfo	
	{ _lisp_BLOCK_TRACE(MP()->lisp,"input: chemInfo");
	    data->expression = $1->_obj; 
	};

chemInfo:	/* empty */ { $$ = new Hold<chemInfo::O_SmartsRoot>(MP()->lisp); }
	    | atomTest 	
	    	{ _lisp_BLOCK_TRACE(MP()->lisp,"chemInfo: atomTest");
		    $$ = new Hold<chemInfo::O_SmartsRoot>(chemInfo::O_SmartsRoot::create($1->_obj,MP()->lisp)); 
		};
	    | atomTest chain 
	    	{ _lisp_BLOCK_TRACE(MP()->lisp,"chemInfo:atomTest chain");
		    $$ = new Hold<chemInfo::O_SmartsRoot>(chemInfo::O_SmartsRoot::create($1->_obj,$2->_obj,MP()->lisp)); 
		}
	    ;


chain: bondAtomTest chain 
    { _lisp_BLOCK_TRACE(MP()->lisp,"chain:bondAtomTest chain");
	$$ = new Hold<chemInfo::O_BondListMatchNode>(chemInfo::O_Chain::create( $1->_obj, $2->_obj,MP()->lisp)); 
    }
| branch chain	   
    { _lisp_BLOCK_TRACE(MP()->lisp,"chain: branch chain");
	$$ = new Hold<chemInfo::O_BondListMatchNode>(chemInfo::O_Branch::create( $1->_obj, $2->_obj,MP()->lisp)); 
    }
| bondAtomTest	   
    { _lisp_BLOCK_TRACE(MP()->lisp,"chain: bondAtomTest");
	$$ = new Hold<chemInfo::O_BondListMatchNode>(chemInfo::O_Chain::create( $1->_obj,MP()->lisp)); 
    }
| branch  
    { _lisp_BLOCK_TRACE(MP()->lisp,"chain: branch");
	$$ = new Hold<chemInfo::O_BondListMatchNode>(chemInfo::O_Branch::create( $1->_obj,MP()->lisp)); 
    } /* handle C(C=O) */
;

bondAtomTest:	APBond atomTest 
			{ _lisp_BLOCK_TRACE(MP()->lisp,"bondAtomTest:APBond atomTest");
			    $$ = new Hold<chemInfo::O_BondTest>(chemInfo::O_BondTest::create( $1, $2->_obj,MP()->lisp )); 
			}
	    |	atomTest 
	    		{ _lisp_BLOCK_TRACE(MP()->lisp,"bondAtomTest: atomTest");
			    $$ = new Hold<chemInfo::O_BondTest>(chemInfo::O_BondTest::create( chemInfo::SABSingleOrAromaticBond, $1->_obj,MP()->lisp)); 
			} 
	    ;
    //	|	APSingleBond atomTest { $$ = new Holder(chemInfo::O_BondTest::create( chemInfo::SABSingleBond, $2->_obj,MP()->lisp)); }
    //	|	APDoubleBond atomTest { $$ = new Holder(chemInfo::O_BondTest::create( chemInfo::SABDoubleBond, $2)->_obj); }
    // 	|	APTripleBond atomTest { $$ = new Holder(chemInfo::O_BondTest::create( chemInfo::SABTripleBond, $2)->_obj,MP()->lisp); }
    //	|	APDirectionalSingleUp atomTest { 
    //			$$ = chemInfo::O_BondTest::create( chemInfo::SABDirectionalSingleUp, $2);  }
    //	|	APSingleUpOrUnspecified atomTest { 
    //			$$ = chemInfo::O_BondTest::create(chemInfo::SABDirectionalSingleUpOrUnspecified, $2);}
    //	|       APDirectionalSingleDown atomTest { 
    //			$$ = chemInfo::O_BondTest::create( chemInfo::SABDirectionalSingleDown, $2); } 
    //	|	APSingleDownOrUnspecified atomTest { 
    //			$$ = chemInfo::O_BondTest::create( chemInfo::SABDirectionalSingleDownOrUnspecified,$2);}
    //	|	APAromaticBond atomTest { 
    //			$$ = chemInfo::O_BondTest::create( chemInfo::SABAromaticBond, $2);}



acyclicAtomTest: APOpenBracket logOp APCloseBracket 
	{ _lisp_BLOCK_TRACE(MP()->lisp,"acyclicAtomTest: APOpenBracket logOp APCloseBracket");
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>($2->_obj);
	} 
| APOrganicElement 
 	{ _lisp_BLOCK_TRACE(MP()->lisp,"acyclicAtomTest: APOrganicElement");
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>((chemInfo::O_AtomTest::create(chemInfo::SAPElement, 0, 0, $1 ,MP()->lisp))); 
	} 
| recursiveChemInfo 
	    		{ _lisp_BLOCK_TRACE(MP()->lisp,"acyclicAtomTest: recursiveChemInfo");
	    		    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>($1->_obj); 
			}
	    ;
    /*	|	APOrganicElement { $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>((chemInfo::Logical( chemInfo::logIdentity, 
						    chemInfo::AtomTest(chemInfo::SAPElement, $1->_obj)))); } 
    */

atomTest: acyclicAtomTest APNumber 
    { _lisp_BLOCK_TRACEF(MP()->lisp,BF("atomTest: acyclicAtomTest APNumber   // atomTag==(%s)  ") % ($2));
	$$=new Hold<chemInfo::O_AtomOrBondMatchNode>(chemInfo::O_TagSet::create(chemInfo::SABAnyBond,$1->_obj,$2,MP()->lisp));
    }
| acyclicAtomTest 
    { _lisp_BLOCK_TRACE(MP()->lisp,"atomTest: acyclicAtomTest");
	$$ = new Hold<chemInfo::O_AtomOrBondMatchNode>($1->_obj); 
    };

branch:	APOpenParenthesis chain APCloseParenthesis { $$ = new Hold<chemInfo::O_BondListMatchNode>($2->_obj); };


 /* 
__BEGIN_DOC( msmarts.atomics, Atomic Primitives)
   SMARTS provides a number of primitive symbols describing atomic properties beyond those used in SMILES (atomic symbol, charge, and isotopic specifications). The following tables list the atomic primitives used in SMARTS (all SMILES atomic symbols are also legal). In these tables <n> stands for a digit, <c> for chiral class.

   Note that atomic primitive H can have two meanings, implying a property or the element itself. [H] means hydrogen atom. [*H2] means any atom with exactly two hydrogens attached
   
   	\begin{tabular}{| l | l | l | l |}
	\hline
	Symbol & Symbol name & Atomic property requirements & Default \\ \hline
__END_DOC
 */

 /*
__APPEND_DOC(msmarts.atomics)
	* & wildcard & any atom & (no default) \\ \hline
__END_DOC
 */
atomPrimativeTest :	APWildCard 
	{ 
		$$ = new Hold<chemInfo::O_AtomOrBondMatchNode>(
				chemInfo::O_AtomTest::create(chemInfo::SAPWildCard,MP()->lisp)); 
	}

/*
__APPEND_DOC(msmarts.atomics)
	D\emph{n} & APDegree & explicit connections & exactly one \\ \hline
__END_DOC
*/
| APDegree 
    	{ 
		$$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( 
				chemInfo::O_AtomTest::create( chemInfo::SAPDegree, 1,MP()->lisp)); 
	}
| APDegree intNumber 
	{ 
		$$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( 
			chemInfo::O_AtomTest::create( chemInfo::SAPDegree,$2,MP()->lisp)); 
	}


/*
__APPEND_DOC(msmarts.atomics)
	H\emph{n} & APTotalHCount& \emph{n} attached hydrogens & exactly one \\ \hline
__END_DOC
*/
| APTotalHCount 
	{ 
		$$ = new Hold<chemInfo::O_AtomOrBondMatchNode>(
				chemInfo::O_AtomTest::create( chemInfo::SAPTotalHCount, 1,MP()->lisp));
	}
| APTotalHCount intNumber 
	{ 
		$$ = new Hold<chemInfo::O_AtomOrBondMatchNode>(
				chemInfo::O_AtomTest::create( chemInfo::SAPTotalHCount, $2,MP()->lisp));
	}

/*
__APPEND_DOC(msmarts.atomics)
	h\emph{n} & APImplicitHCount& \emph{n} implicit attached hydrogens & at least one\\ \hline
__END_DOC
*/
| APImplicitHCount 
	{
		$$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( 
			chemInfo::O_AtomTest::create( chemInfo::SAPImplicitHCount, 1,MP()->lisp)); 
	}
| APImplicitHCount intNumber 
	{ 
		$$ = new Hold<chemInfo::O_AtomOrBondMatchNode>(
					chemInfo::O_AtomTest::create( 
						chemInfo::SAPImplicitHCount, $2,MP()->lisp));
	}

/*
__APPEND_DOC(msmarts.atomics)
	?\emph{n} & APRingTest& Atom is matched to atom tagged with \emph{n}& (no default)\\ \hline
__END_DOC
*/
| APRingTest APNumber
	{
		$$=new Hold<chemInfo::O_AtomOrBondMatchNode>(
					chemInfo::O_AtomTest::create(chemInfo::SAPRingTest,$2,MP()->lisp));
	}

/*
__APPEND_DOC(msmarts.atomics)
	U\emph{n} & APResidueTest& Atom must be in same residue as atom tagged \emph{n}& (no default)\\ \hline
__END_DOC
*/
| APResidueTest APNumber
	{
		$$=new Hold<chemInfo::O_AtomOrBondMatchNode>(
					chemInfo::O_AtomTest::create(chemInfo::SAPResidueTest,$2,MP()->lisp));
	}


/*
__APPEND_DOC(msmarts.atomics)
	R\emph{n} & APRingMemberCount & is in \emph{n} SSSR rings \par(WORKS?)& any ring atom\\ \hline
__END_DOC
*/
| APRingMembershipCount 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPRingMembershipCount, 1,MP()->lisp)); 
	}
| APRingMembershipCount intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPRingMembershipCount, $2,MP()->lisp));
	}


/*
__APPEND_DOC(msmarts.atomics)
	r\emph{n} & APRingSize & is in smallest SSSR size \emph{n} & any ring atom\\ \hline
__END_DOC
*/
| APRingSize 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPRingSize, 1,MP()->lisp)); 
	}	
| APRingSize intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPRingSize, $2,MP()->lisp));
	}
/*
__APPEND_DOC(msmarts.atomics)
	v\emph{n} & APValence & total bond order \emph{n} & exactly 1 \\ \hline
__END_DOC
*/
| APValence 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPValence, 1,MP()->lisp)); 
	}
| APValence intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPValence, $2,MP()->lisp));
	}

/*
__APPEND_DOC(msmarts.atomics)
	X\emph{n} & APConnectivity& \emph{n} total connections & exactly 1 \\ \hline
__END_DOC
*/
| APConnectivity 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPConnectivity, 1,MP()->lisp)); 
	}
| APConnectivity intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPConnectivity, $2,MP()->lisp));
	}


| APGroupNumber intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPGroupNumber, $2,MP()->lisp)); 
	} 
| APElectronegativeElement 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPElectronegativeElement,MP()->lisp)); 
	} 
| APHeavyAtomTotalBond 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPHeavyAtomTotalBond, 1,MP()->lisp)); 
	}
| APHeavyAtomTotalBond intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPHeavyAtomTotalBond, $2,MP()->lisp)); 
	} 
/*| APNegativeCharge APWildcardOrAtomicMass intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeFormalCharge, $3 ,MP()->lisp)); 
	}  */
/*
__APPEND_DOC(msmarts.atomics)
	-\emph{n} & APNegativeCharge & -\emph{n} charge & exactly -1 \\ \hline
__END_DOC
*/
| APNegativeCharge intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeCharge, $2,MP()->lisp));
	}
| APNegativeCharge 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeCharge, -1,MP()->lisp));
	}

/*
__APPEND_DOC(msmarts.atomics)
	-- & APNegativeCharge 2x & -2 charge & exactly -2 \\ \hline
__END_DOC
*/
| APNegativeCharge APNegativeCharge
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeCharge, -2,MP()->lisp));
	}
/*
__APPEND_DOC(msmarts.atomics)
	--- & APNegativeCharge 3x & -3 charge & exactly -3 \\ \hline
__END_DOC
*/
| APNegativeCharge APNegativeCharge APNegativeCharge 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPNegativeCharge, -3,MP()->lisp));
	}
/*| APPositiveCharge APWildcardOrAtomicMass intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveFormalCharge, $3 ,MP()->lisp)); 
	}  */



/*
__APPEND_DOC(msmarts.atomics)
	+\emph{n} & APPositiveCharge & +\emph{n} charge & exactly +1 \\ \hline
__END_DOC
*/
| APPositiveCharge intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveCharge, $2,MP()->lisp));
	}
| APPositiveCharge 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveCharge, 1,MP()->lisp));
	}
/*
__APPEND_DOC(msmarts.atomics)
	++ & APPositiveCharge 2x & +2 charge & exactly +2 \\ \hline
__END_DOC
*/
| APPositiveCharge APPositiveCharge
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveCharge, 2,MP()->lisp));
	}
/*
__APPEND_DOC(msmarts.atomics)
	+++ & APPositiveCharge 3x & +3 charge & exactly +3 \\ \hline
__END_DOC
*/
| APPositiveCharge APPositiveCharge APPositiveCharge 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPositiveCharge, 3,MP()->lisp));
	}
| APTransitionMetal intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPTransitionMetal, $2,MP()->lisp)); 
	}
| APAromaticPiElectron intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPAromaticPiElectron , $2,MP()->lisp));
	}
| APPiBondOrbital 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPPiBondOrbital,MP()->lisp)); 
	}
/*
__APPEND_DOC(msmarts.atomics)
	\#\emph{n} & APAtomicNumber & atomic number \emph{n} & (no default) \\ \hline
__END_DOC
*/
| APAtomicNumber intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPAtomicNumber, $2,MP()->lisp));
	}
| APChiralityAntiClockwise 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPChiralityAntiClockwise,MP()->lisp));
	}
| APChiralityClockwise 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPChiralityClockwise,MP()->lisp));
	}
/*
__APPEND_DOC(msmarts.atomics)
	\emph{n} & APAtomicMass & atomic mass \emph{n} & (no default) \\ \hline
__END_DOC
*/
| intNumber 
	{
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPAtomicMass, $1,MP()->lisp));
	}
| APOrganicElement 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPElement, $1 ,MP()->lisp)); 
	}
| APInorganicElement 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPElement, $1 ,MP()->lisp)); 
	}
| APLonePair 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPLonePair, 1,MP()->lisp)); 
	}
| APLonePair intNumber 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>( chemInfo::O_AtomTest::create( chemInfo::SAPLonePair, $2,MP()->lisp));
	}
/*
__APPEND_DOC(msmarts.atomics)
	\$(\emph{\tiny MSMARTS}) & recursive MSMARTS & match recursive MSMARTS & (no default) \\ \hline
__END_DOC
*/
| recursiveChemInfo 
	{ 
	    $$ = new Hold<chemInfo::O_AtomOrBondMatchNode>($1->_obj); 
	}
    ;
/*
__APPEND_DOC(msmarts.atomics)
\end{tabular}

Some of these have not been debugged. Test before you trust them.

Examples: \par
\begin{tabular}{| l | l |}
\hline
  [CH2] & aliphatic carbon with two hydrogens (methylene carbon) \\ \hline
  [!C;R] & ( NOT aliphatic carbon ) AND in ring \\ \hline
  [!C;!R0] & same as above ("!R0" means not in zero rings) \\ \hline
  [n;H1] & H-pyrrole nitrogen \\ \hline
  [n\&H1] & same as above \\ \hline
  [nH1] & same as above \\ \hline
  [c,n\&H1] & any arom carbon OR H-pyrrole nitrogen \\ \hline
  [X3\&H0] & atom with 3 total bonds and no H's \\ \hline
  [c,n;H1] & (arom carbon OR arom nitrogen)  and exactly one H \\ \hline
  [Cl] & any chlorine atom \\ \hline
  [35*] & any atom of mass 35 \\ \hline
  [35Cl] & chlorine atom of mass 35 \\ \hline
  [F,Cl,Br,I] & the 1st four halogens. \\ \hline
\end{tabular}
__END_DOC
*/

/*
__BEGIN_DOC(msmarts.recursive,Recursive MSMARTS)

     Any MSMARTS expression may be used to define an atomic environment by writing a SMARTS starting with the atom of interest in this form:
     \$(\emph{MSMARTS})
     Such definitions may be considered atomic properties. These expressions can be used in same manner as other atomic primitives (also, they can be nested). Recursive SMARTS expressions are used in the following manner:

	\begin{tabular}{| l | l |}
	\hline
	*C & atom connected to methyl or methylene carbon \\ \hline
	*CC & atom connected to ethyl carbon \\ \hline
	[\$(*C);\$(*CC)] & Atom in both above environments (matches CCC) \\ \hline
	\end{tabular}

The additional power of such expressions is illustrated by the following
example which derives an expression for methyl carbons which are ortho to
oxygen and meta to a nitrogen on an aromatic ring.


\begin{tabular}{| l | l | }
\hline
CaaO & C ortho to O \\ \hline
CaaaN & C meta to N \\ \hline
Caa(O)aN & C ortho to O and meta to N (but 2O,3N only) \\ \hline
Ca(aO)aaN & C ortho to O and meta to N (but 2O,5N only) \\ \hline
C[\$( aaO);\$( aaaN)] & C ortho to O and meta to N (all cases) \\ \hline
\end{tabular}
__END_DOC
 */


 /*!
__BEGIN_DOC(msmarts.logical,Logical Operators)

 Atom and bond primitive specifications may be combined to form expressions by using logical operators. In the following table, e is an atom or bond SMARTS expression (which may be a primitive). The logical operators are listed in order of decreasing precedence (high precedence operators are evaluated first).

All atomic expressions which are not simple primitives must be enclosed in brackets. The default operation is \& (high precedence "and"), i.e., two adjacent primitives without an intervening logical operator must both be true for the expression (or subexpression) to be true.

The ability to form expressions gives the SMARTS user a great deal of power to specify exactly what is desired. The two forms of the AND operator are used in SMARTS instead of grouping operators.

\begin{tabular}{|l|l|l|}
\hline
\textbf{Symbol} & \textbf{Expression} & \textbf{Meaning} \\ \hline
__END_DOC
*/

logOp:	atomPrimativeTest 	
	{ 
	    $$ = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logIdentity, $1->_obj ,MP()->lisp)); 
	}
/*!
__APPEND_DOC(msmarts.logical)
exclamation & !e1 & not e1 \\ \hline
__END_DOC
*/
| APOperatorNot logOp 
	{ 
	    $$ = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logNot, $2->_obj ,MP()->lisp)); 
	}
/*
__APPEND_DOC(msmarts.logical)
ampersand & e1\&e2 & e1 and e2 (high precedence) \\ \hline
__END_DOC
*/
| logOp APOperatorAndHigh logOp 
	{ 
	    $$ = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logHighPrecedenceAnd, $1->_obj, $3->_obj ,MP()->lisp)); 
	}
/*
__APPEND_DOC(msmarts.logical)
comma & e1,e2  & e1 or e2 \\ \hline
__END_DOC
*/
| logOp APOperatorOr logOp 
	{ 
	    $$ = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logOr, $1->_obj, $3->_obj ,MP()->lisp)); 
	}
/*
__APPEND_DOC(msmarts.logical)
semicolon & e1;e2  & e1 and e2 (low precedence) \\ \hline
\end{tabular}
__END_DOC
*/
| logOp APOperatorAndLow logOp 
	{ 
	    $$ = new Hold<chemInfo::O_Logical>(
	    		chemInfo::O_Logical::create( chemInfo::logLowPrecedenceAnd, $1->_obj, $3->_obj ,MP()->lisp)); 
	}
;

recursiveChemInfo:	APDollarSign APOpenParenthesis chemInfo APCloseParenthesis { $$ = new Hold<chemInfo::O_Root>($3->_obj); }
	    ;

intNumber: APNumber { $$ = atoi($1); };




    %%

    //
    //
    //	Epilogue
    //
    //


    typedef	struct {
    char	rchar;  
    char* name ; 
    int	token ; 
    }	SpecialCharacterEntry;

    #if 0 
    SpecialCharacterEntry specialCharacters[] = {
    { ';', "APOperatorAndLow", APOperatorAndLow },
    { ',', "APOperatorOr", APOperatorOr },
    { '!', "APOperatorNot", APOperatorNot },
    { '&', "APOPeratorAndHigh", APOperatorAndHigh },
    { '+', "APPositiveCharge", APPositiveCharge },
    //{ '*', "APWildcardOrAtomicMass", APWildcardOrAtomicMass },   
    { '*', "APWildCard", APWildCard},   
    { '=', "APDoubleBond", APDoubleBond },
    { ':', "APAromaticBond", APAromaticBond },
    { '~', "APAnyBond", APAnyBond },
    { '$', "APDollarSign", APDollarSign},
    { '(', "APOpenParenthesis", APOpenParenthesis },
    { ')', "APCloseParenthesis", APCloseParenthesis },
    { '[', "APOpenBracket", APOpenBracket },
{ ']', "APCloseBracket", APCloseBracket },
{ 'D', "APDegree", APDegree },
{ 'H', "APTotalHCount", APTotalHCount },
{ 'h', "APImplicitHCount", APImplicitHCount },
{ 'i', "APPiBondOrbital", APPiBondOrbital },
{ 'Q', "APHeavyAtomTotalBond", APHeavyAtomTotalBond },
{ 'R', "APRingMembershipCount", APRingMembershipCount }, 
{ 'r', "APRingSize", APRingSize },
{ 'v', "APValence", APValence },
{ 'X', "APConnectivity", APConnectivity },
{ '\0', "", 0 }
};
#endif


const char*	elementTable[] = 
{		//First the two character Inorganic elements
"Al",
"Ca",		
"Co",
"Cu",
"Fe",
""		// Last entry
};


const char*	organicElementTable[] = 
{
"Br",       	//Put 2 chars elements on top, or element won't match correctly 
"Cl",
"B",
"b",
"C",
"c",
"F",
"I",
"N",
"n",
"O",
"o",
"P",
"p",
"S",
"s",
""
};

int	msmarts_lex(YYSTYPE* yylval, YYLTYPE* yylloc, msmarts_SParserParam* data)
{
char	ichar;
    do 
    {
	ichar = data->lexStream->get();	 // Read one character
	if (ichar=='\n') 
	return APEndOfLine;
    } while (!data->lexStream->eof() && ichar < ' ' );
    if ( data->lexStream->eof() ) return 0;
#ifdef	DEBUG_ON
    switch (ichar)
    {
        case '&':
	    LEXDPRINT(MP()->lisp,BF("next char: &amp;"));
	    break;
	case '<':
	    LEXDPRINT(MP()->lisp,BF("next char: &lt;"));
	    break;
	case '>':
	    LEXDPRINT(MP()->lisp,BF("next char: &gt;"));
	    break;
	default:
            LEXDPRINT(MP()->lisp,BF("next char: |%c|") % ichar);
	    break;
    }
#endif


		    //
		    // Handle two and one character elements
		    //
    LEXDPRINT(MP()->lisp,BF("Checking 1 and 2 chars inorganic elements"));
    for ( int i=0; elementTable[i][0]!='\0'; i++ )
    {
	if ( elementTable[i][0] == ichar ) 
	{
	    if (elementTable[i][1] != '\0') 
	    {
		if ( elementTable[i][1] == data->lexStream->peek() ) 
		    {
		    data->lexStream->get();
//			strcpy( yylval->eval ,  elementTable[i] );
		    yylval->eval = elementTable[i];
		    LEXDPRINT(MP()->lisp,BF("Matched the 2 char inorganic element: |%s|")%elementTable[i]);	
		    return APInorganicElement;
		    }
	    } 
	    else 
	    {

		yylval->eval = elementTable[i];
//		    strcpy( yylval->eval , elementTable[i]);
		LEXDPRINT(MP()->lisp,BF("Matched the 1 char inorganic element: |%s|")%elementTable[i]);	
		return APInorganicElement;
	    }
       }
   }            



// 	Process Both Aliphatic and Aromatic Organic ElementS	
    LEXDPRINT(MP()->lisp,BF("Checking 1 and 2 chars organic elements"));	
    for (int i=0; organicElementTable[i][0] != '\0' ; i++)
    {	
	if (organicElementTable[i][0] == ichar )   
	{
	    LEXDPRINT(MP()->lisp,BF("Matched the 1st char in organicElementTable[]"));
	    if (organicElementTable[i][1] != '\0')
	    {
		if ( organicElementTable[i][1] == data->lexStream->peek() )
		{
		    cout<<"Peek char: "<<data->lexStream->peek()<<endl;
		    data->lexStream->get();
//			 strcpy(yylval->eval , organicElementTable[i] );
		    yylval->eval = organicElementTable[i];
		    LEXDPRINT(MP()->lisp,BF("Matched the 2 char organic element: |%s|")%elementTable[i]);
		    return APOrganicElement;
		}
	    }	
	    else 
	    {
		yylval->eval = organicElementTable[i];
//		     strcpy( yylval->eval, organicElementTable[i];
		LEXDPRINT(MP()->lisp,BF("Matched the 1 char organic element: |%s|\n")%organicElementTable[i]);
		return APOrganicElement;
	    }	
	}
    } 





		    //	
		    // Handle special characters that are one
		    // character long
		    //

    LEXDPRINT(MP()->lisp,BF("Checking special characters"));	
    switch (ichar) 
    {
	    case '*':
		    LEXPRINT(MP()->lisp,"APWildCard");
		    return APWildCard;
		    break;
	    case '$':
		    LEXPRINT(MP()->lisp,"APDollarSign");
		    return APDollarSign;
		    break;
	    case '(':
		    LEXPRINT(MP()->lisp,"APOpenParenthesis");
		    return APOpenParenthesis;
		    break;
	    case ')':
		    LEXPRINT(MP()->lisp,"APCloseParenthesis");
		    return APCloseParenthesis;
		    break;
	    case '[':
		    LEXPRINT(MP()->lisp,"APOpenBracket");
		    return APOpenBracket;
		    break;
	    case ']':
		    LEXPRINT(MP()->lisp,"APCloseBracket");
		    return APCloseBracket;
		    break;
	    case ';':
		    LEXPRINT(MP()->lisp,"APOperatorAndLow");
		    return APOperatorAndLow;
		    break;
	    case '?' :
	    	    LEXPRINT(MP()->lisp,"APRingTest");
		    return APRingTest;
		    break;
	    case 'U':	
	    		// Test if residue of this atom
			// is the same as a previously tagged atom
		    LEXPRINT(MP()->lisp,"APResidueTest");
		    LEXDPRINT(MP()->lisp,BF("Identified as APResidueTest"));
		    return APResidueTest;
		    break;
	    case ',':
		    LEXPRINT(MP()->lisp,"APOperatorOr");
		    return APOperatorOr;
		    break;
	    case '!':
		    LEXPRINT(MP()->lisp,"APOperatorNot");
		    return APOperatorNot;
		    break;
	    case '&':
		    LEXPRINT(MP()->lisp,"APOpeartorAndHigh");
		    return APOperatorAndHigh;
		    break;
	    case '~':
		    LEXPRINT(MP()->lisp,"APAnyBond");   
                    yylval->benum = chemInfo::SABAnyBond;
		    return APBond;
		    break;	

	    case ':' :
		    LEXPRINT(MP()->lisp,"APAromaticBond");
                    yylval->benum = chemInfo::SABAromaticBond;
		    return APBond;
		    break;


	    case '#':
		    if (data->lexStream->peek() >= '0' && data->lexStream->peek() <= '9' ) 
		    {
			    LEXPRINT(MP()->lisp,"APAtomicNumber");
			    return APAtomicNumber;
			    break;
		    } else if (data->lexStream->peek() == 'G')
		    {
			    data->lexStream->get();
			    if (data->lexStream->peek() >= '0' && data->lexStream->peek() <= '9')
			    {
			        LEXPRINT(MP()->lisp,"APGroupNumber");
				return APGroupNumber;
				break;
			    }
			    return APError;
		    } else if (data->lexStream->peek() == 'T')
		    {
			    data->lexStream->get();
			    if (data->lexStream->peek() >= '0' && data->lexStream->peek() <= '9')
				    {
				    LEXPRINT(MP()->lisp,"APTransitionMetal");
				    return APTransitionMetal;
				    break;
				    }
			    return APError;
		    } else if (data->lexStream->peek() == 'N')
		    {
			    data->lexStream->get();
			    if (data->lexStream->peek() >= '0' && data->lexStream->peek() <= '9')
				    {
				    LEXPRINT(MP()->lisp,"APElectronegativeElement");
				    return APElectronegativeElement;
				    break;
				    }
			    return APError;
		    } 
		    LEXPRINT(MP()->lisp,"APTripleBond");
                    yylval->benum = chemInfo::SABTripleBond;
		    return APBond;
		    break;
	    case '/':
		    if (data->lexStream->peek() == '?') 
		    {
			data->lexStream->get();
			LEXPRINT(MP()->lisp,"APSingleUpOrUnspecified");
			yylval->benum = chemInfo::SABDirectionalSingleUpOrUnspecified;
			return  APBond;
			break;
		    }
		    LEXPRINT(MP()->lisp,"APDirectionalSingleUp");
		    yylval->benum = chemInfo::SABDirectionalSingleUp;
		    return  APBond;
		    break;
	    case '-':
		    if (data->lexStream->peek() >= '0' && data->lexStream->peek() <= '9'  ) 
		    {	
			    LEXPRINT(MP()->lisp,"APNegativeCharge");
			    return APNegativeCharge;
			    break; 
		    }
		    LEXPRINT(MP()->lisp,"APSingleBond");
		    yylval->benum = chemInfo::SABSingleBond;
		    return  APBond;
		    break;
	    case '=':
		    LEXPRINT(MP()->lisp,"APDoubleBond");
                    yylval->benum = chemInfo::SABDoubleBond;
		    return APBond;
		    break;	
	    case '\\':
		    if (data->lexStream->peek() == '?') 
		    {
			data->lexStream->get();
			LEXPRINT(MP()->lisp,"APSingleDownOrUnspecified");
			yylval->benum = chemInfo::SABDirectionalSingleDownOrUnspecified;
			return  APBond;
			break; 
		    }
		    LEXPRINT(MP()->lisp,"APDirectionalSingleDown");
		    yylval->benum = chemInfo::SABDirectionalSingleDown;
		    return  APBond;
		    break;
	    case '@':
		    if ( data->lexStream->peek() == '@' ) {
			data->lexStream->get(); // pull the second @ out of the stream
			LEXPRINT(MP()->lisp,"APChiralityClockwise");
			LEXDPRINT(MP()->lisp,BF("Identified as APChiralityClockwise"));	
			return  APChiralityClockwise;
			break;
		    }
		    LEXPRINT(MP()->lisp,"APChiralityAntiClockwiseOrAnyRingBond");
		    LEXDPRINT(MP()->lisp,BF("Identified as APChiralityAntiClockwise"));
		    return  APChiralityAntiClockwise;
                    break;
	    case 'D':
		    if ( data->lexStream->peek() >= '0' && data->lexStream->peek() <= '9') {
		        LEXPRINT(MP()->lisp,"APDegree");
		        LEXDPRINT(MP()->lisp,BF("Identified as APDegree"));
		        return APDegree;
		        break;
		    }
		    return APError;
		    break;
	    case 'H':
		    LEXPRINT(MP()->lisp,"APTotalHCount");
		    LEXDPRINT(MP()->lisp,BF("Identified as APTotalHCount"));
		    return  APTotalHCount;
		    break; 
	    case 'h':
		    LEXPRINT(MP()->lisp,"APImplicitHCount");
		    LEXDPRINT(MP()->lisp,BF("Identifed as APImplicitHCount"));
		    return  APImplicitHCount;
		    break;
	    case 'i':
		    if (data->lexStream->peek() >= '0' && data->lexStream->peek() <= '9')
		    {
			LEXPRINT(MP()->lisp,"APAromaticPiElectron");
			LEXDPRINT(MP()->lisp,BF("Identified as APAromaticPiElectron"));
			return APAromaticPiElectron;
			break;
		    }
		    LEXPRINT(MP()->lisp,"APPiBondOrbital");
		    LEXDPRINT(MP()->lisp,BF("Identified as APPiBondOrbital"));
		    return APPiBondOrbital;
		    break;
	    case 'Q':
		    LEXPRINT(MP()->lisp,"APHeavyAtomTotalBond");
		    LEXDPRINT(MP()->lisp,BF("Identified as APHeavyAtomTotalBond"));
		    return APHeavyAtomTotalBond;
		    break;  
	    case 'R':
		    LEXPRINT(MP()->lisp,"APRingMembershipCount");
		    LEXDPRINT(MP()->lisp,BF("Identified as APRingMembership"));
		    return APRingMembershipCount;
		    break;
	    case 'r':
		    LEXPRINT(MP()->lisp,"APRingSize");
		    LEXDPRINT(MP()->lisp,BF("Identified as APRingSize"));
		    return APRingSize;
		    break;
	    case 'v':
		    LEXPRINT(MP()->lisp,"APValence");
		    LEXDPRINT(MP()->lisp,BF("Identified as APValence"));	
		    return APValence;
		    break;
	    case 'X':
		    LEXPRINT(MP()->lisp,"APConnectivity");  
		    LEXDPRINT(MP()->lisp,BF("Identified as APConnectivity"));	
		    return APConnectivity;
		    break;
    }
		    //	
		    // Handle special characters that are two 
		    // characters long
		    //
    if (ichar == 'L' && data->lexStream->peek() == 'p')
    {
        data->lexStream->get();	
        LEXPRINT(MP()->lisp,"APLonePair");
        return APLonePair;
    }

			    // Parse a number
    if ( ichar>='0' && ichar<='9' ) 
    {
	    //Create a string for number	
	    int digitCount = 0;
	    yylval->cval[digitCount] = ichar;
	    digitCount++;
	    while ( data->lexStream->peek()>='0' && data->lexStream->peek()<='9' ) 
	    {
		    ichar = data->lexStream->get();
		    if ( digitCount >= MAX_CVAL )
		    {
		        lisp_THROW(MP()->lisp,O_LispError::create(BF("Exceeded max number of digits in Number"),MP()->lisp));
		    }
		    yylval->cval[digitCount] = ichar;
		    digitCount++;
	    }
	    yylval->cval[digitCount] = '\0';
	    lisp_LOG(MP()->lisp,BF("APNumber = (%s)") % (yylval->cval) );
	    return  APNumber;
    }
    

    LEXPRINT(MP()->lisp,"APError");
    return APError;


}



#if 0
int main()
{
    cout<<"Input CHEM_INFO : ";
    data->lexStream = &cin;

    yyparse();


return 0;
}
#endif


chemInfo::RPSmartsRoot smarts_compile(const string& input, RPLisp lisp, stringstream& errorStream)
{_F(lisp);
    msmarts_SParserParam p;
    stringstream sin(input);
    p.lisp = lisp;
    p.expression = lisp->nil<O_SmartsRoot>();
    p.lexStream = &sin;
    p.msmartsErrorStream = &errorStream;
    lisp_LOG(lisp,BF("Entering msmarts_parse"));
    if ( msmarts_parse(&p) )
    {
	THROW(_lisp->create<O_LispError>(BF("%s") % (p.msmartsErrorStream->str())));
    }
    return p.expression;
}


void msmarts_error(YYLTYPE* yyloc, msmarts_SParserParam* data, const char* message )
{
    *(data->msmartsErrorStream) << "Error: " << message << endl;
}
    

