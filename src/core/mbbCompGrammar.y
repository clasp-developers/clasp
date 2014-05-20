

%{

/* C declarations */

#define	DEBUG_LEVEL_NONE

#include	<string>
#include	<stdio.h>
#include	"matrix.h"
#include	"transform.h"
#include	"conformation.h"
#include	"container.h"
#include	"atom.h"
#include	"residue.h"
#include	"molecule.h"
#include	"aggregate.h"
#include	"cyclomer.h"
#include	"cyclomerDatabase.h"


//
// Specify the parameter that will be passed to yyparse
//
#define	YYPARSE_PARAM	PDatabase

#if	0
#define	GRAMMAR_DEBUG
#define	YYERROR_VERBOSE
#endif

static	vector<int>		vsIdList;
static	string			sCurName;
static	string			sCurHybridization;
static	Residue*		rPCurResidue;
static	Cyclomer*		cPCurCyclomer;
static	Conformation*		cPCurConformation;
static	Transform*		tPCurTransform;
static	Matrix			mScratchMatrix;
static	Matrix			mHeadTransform;
static	Matrix			mTailTransform;
static	Matrix			mIdentity;	// automatically set to identity

static	Cyclomer*	cPCur;
static	AtomHandle	ah;
static	Vector3		vPos;

extern	"C"	int	yylex();
extern	"C"	int	yyerror(char* s);

#include	"bond.h"

%}


%union	{
	double		dval;
	char		str[1024];
	double		pos[3];
	int		ival;
	BondOrder	bOrder;
}

%type	<dval>	number
%type	<str>	atomName;
%type	<str>	element;
%type	<str>	atomType;
%type	<ival>	atomId;

%token	T_ATOM
%token	<ival>	T_INTEGER
%token	<dval>	T_FLOAT
%token	<str>	T_STRING
%token	T_CHAR
%token	T_OPENBLOCK
%token	T_CLOSEBLOCK
%token	T_SEMICOLON
%token	T_CYCLOMER
%token	T_RESIDUE
%token	T_STAR
%token	T_BACKWARDTRANSFORM
%token	T_CENTER
%token	T_BOND
%token	T_CONFORMATION
%token	T_DERIVATIVEBONDS
%token	T_DOUBLE
%token	T_TRIPLE
%token	T_FORWARDTRANSFORM
%token	T_HEAD
%token	T_HEADAXES
%token	T_HEADTRANSFORM
%token	T_INLINK
%token	T_OUTLINK
%token	T_POSITIONS
%token	T_SINGLE
%token	T_START
%token	T_STOP
%token	T_TAIL
%token	T_TAILAXES
%token	T_TAILTRANSFORM
%token	T_TERMINATOR
%token	T_OPENPAREN
%token	T_CLOSEPAREN
%token	T_AT
%token	T_PARAMETERSET
%token	T_TYPE
%token	T_ANGLE
%token	T_TORSION
%token	T_IMPROPER
%token	T_NONBOND
%token	T_COMMENT


%% /* Grammar */


cyclomerDatabase: grammarStmts;

grammarStmts :	grammarStmts grammarStmt		{}
| grammarStmt				{};

grammarStmt :	parameterSet T_SEMICOLON {}
| forwardTransform T_SEMICOLON
| backwardTransform T_SEMICOLON 
| conformation T_SEMICOLON
| cyclomer T_SEMICOLON
| error T_SEMICOLON { printf( "ERROR Line: %d | Unknown object, only parameterSet, forwardTransform\n backwardTransform, conformation, cyclomer allowed\n", @1.first_line ); };


parameterSet :	T_PARAMETERSET T_OPENBLOCK parmSetEntries T_CLOSEBLOCK ;

parmSetEntries : parmSetEntry parmSetEntries
| parmSetEntry;

parmSetEntry : atomTypeDef T_SEMICOLON
| bondParmDef T_SEMICOLON
| angleParmDef T_SEMICOLON
| torsionParmDef T_SEMICOLON
| improperParmDef T_SEMICOLON
| nonBondParmDef T_SEMICOLON
| error T_SEMICOLON { printf( "ERROR Line: %d | Expecting a type, bond, angle, torsion, improper, nonbond parameter\n", @1.first_line ); };

atomTypeDef : T_TYPE atomType
    {
	((CyclomerDatabase*)PDatabase)->parameterSet()->addType($<str>2);
    };

bondParmDef : T_BOND atomType atomType number number
    {
    int	t1, t2;
    double	kb, r0;
	t1 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>2);
	t2 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>3);
	kb = $<dval>4;
	r0 = $<dval>5;
	((CyclomerDatabase*)PDatabase)->parameterSet()->addBond(t1,t2,kb,r0);
    };

angleParmDef : T_ANGLE atomType atomType atomType number number
    {
    int	t1, t2, t3;
    double	k, v;
	t1 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>2);
	t2 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>3);
	t3 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>4);
	k = $<dval>5;
	v = $<dval>6;
	((CyclomerDatabase*)PDatabase)->parameterSet()->addAngle(t1,t2,t3,k,v*0.0174533);
    };

torsionParmDef : T_TORSION atomType atomType atomType atomType number number
    {
    int	t1, t2, t3, t4;
    double	n, v, comp;
	t1 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>2);
	t2 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>3);
	t3 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>4);
	t4 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>5);
	v = $<dval>6;
	comp = $<dval>7;
	((CyclomerDatabase*)PDatabase)->parameterSet()->addTorsion(t1,t2,t3,t4,v,comp);
    };
improperParmDef : T_IMPROPER atomType atomType atomType atomType number 
    {
    int	t1, t2, t3, t4;
    double	n, v;
	t1 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>2);
	t2 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>3);
	t3 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>4);
	t4 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>5);
	v = $<dval>6;
	((CyclomerDatabase*)PDatabase)->parameterSet()->addImproper(t1,t2,t3,t4,v);
    };

nonBondParmDef : T_NONBOND atomType number number
    {
    int	t1, t2;
    double	rStar, epsilon;
	t1 = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>2);
	rStar = $<dval>3;
	epsilon = $<dval>4;
	((CyclomerDatabase*)PDatabase)->parameterSet()->addNonBond(t1,rStar,epsilon);
    };


forwardTransform : T_FORWARDTRANSFORM 
			cyclomerName cyclomerName cyclomerName 
			transformDefinition 
    {
		// process mScratchMatrix;
	tPCurTransform = new Transform();
	tPCurTransform->setContext( $<str>2, $<str>3, $<str>4 );
	tPCurTransform->setIsForward();
	tPCurTransform->setTransform(&mScratchMatrix);
	((CyclomerDatabase*)PDatabase)->addForwardTransform(tPCurTransform);
	
    };

backwardTransform : T_BACKWARDTRANSFORM 
			cyclomerName cyclomerName cyclomerName 
			transformDefinition 
    {
		// process mScratchMatrix;
	tPCurTransform = new Transform();
	tPCurTransform->setContext( $<str>2, $<str>3, $<str>4 );
	tPCurTransform->setIsBackward();
	tPCurTransform->setTransform(&mScratchMatrix);
	((CyclomerDatabase*)PDatabase)->addBackwardTransform(tPCurTransform);
    };

transformDefinition : transformMatrix;

transformMatrix :	T_OPENBLOCK number number number number
				number number number number
				number number number number
				number number number number  T_CLOSEBLOCK 
    {
	mScratchMatrix.atRowColPut( 0, 0, $2 );
	mScratchMatrix.atRowColPut( 1, 0, $3 );
	mScratchMatrix.atRowColPut( 2, 0, $4 );
	mScratchMatrix.atRowColPut( 3, 0, $5 );
	mScratchMatrix.atRowColPut( 0, 1, $6 );
	mScratchMatrix.atRowColPut( 1, 1, $7 );
	mScratchMatrix.atRowColPut( 2, 1, $8 );
	mScratchMatrix.atRowColPut( 3, 1, $9 );
	mScratchMatrix.atRowColPut( 0, 2, $10 );
	mScratchMatrix.atRowColPut( 1, 2, $11 );
	mScratchMatrix.atRowColPut( 2, 2, $12 );
	mScratchMatrix.atRowColPut( 3, 2, $13 );
	mScratchMatrix.atRowColPut( 0, 3, $14 );
	mScratchMatrix.atRowColPut( 1, 3, $15 );
	mScratchMatrix.atRowColPut( 2, 3, $16 );
	mScratchMatrix.atRowColPut( 3, 3, $17 );
    }
| T_OPENBLOCK error T_CLOSEBLOCK { printf( "ERROR Line: %d | Error in specifying transformation matrix\n", @1.first_line ); };



conformation :	inContextConformation				;



inContextConformation :	T_CONFORMATION 
    {
	cPCurConformation = new Conformation();
    }
   cyclomerName cyclomerName cyclomerName 
    {
	cPCurConformation->setContext( $<str>3, $<str>4, $<str>5 );
    }
   conformationBody
    {
	((CyclomerDatabase*)PDatabase)->addConformation(cPCurConformation);
    };



conformationBody :	T_OPENBLOCK conformationEntries T_CLOSEBLOCK;



conformationEntries :	headTransformEntry  tailTransformEntry conformationPositions	
    {
	cPCurConformation->setHeadTransform( mHeadTransform );
	cPCurConformation->setTailTransform( mTailTransform );
    }
| tailTransformEntry headTransformEntry conformationPositions	
    {
	cPCurConformation->setTailTransform( mTailTransform );
	cPCurConformation->setHeadTransform( mHeadTransform );
    }
| headTransformEntry conformationPositions 
    {
	cPCurConformation->setTailTransform( mIdentity );
	cPCurConformation->setHeadTransform( mHeadTransform );
    }
| tailTransformEntry conformationPositions 
    {
	cPCurConformation->setHeadTransform( mIdentity );
	cPCurConformation->setTailTransform( mTailTransform );
    };



headTransformEntry :	T_HEADTRANSFORM transformMatrix	
    {
	mHeadTransform = mScratchMatrix;
    };



tailTransformEntry :	T_TAILTRANSFORM transformMatrix			
    {
	mTailTransform = mScratchMatrix;
    };



conformationPositions :	T_POSITIONS T_OPENBLOCK positionEntries T_CLOSEBLOCK		;



positionEntries :	posEntry positionEntries
| posEntry;



posEntry :	T_OPENBLOCK atomId position T_CLOSEBLOCK	
    {
	string sName;
	sName = cPCurConformation->getCurName();
	cPCur = ((CyclomerDatabase*)PDatabase)->cyclomerWithName(sName);
	ah = cPCur->getResidue()->getHandleForAtomId($<ival>2);
	vPos = Vector3();
        vPos.set( $<pos>3[0], $<pos>3[1], $<pos>3[2] );
	cPCurConformation->addPosition( ah, vPos );
    }
| T_OPENBLOCK error T_CLOSEBLOCK { printf( "ERROR Line: %d | Error in specifying position of atom\n", @1.first_line ); };




cyclomer	:	T_CYCLOMER cyclomerName 
    {
	sCurName = $<str>2;
	cPCurCyclomer = new Cyclomer();
	cPCurCyclomer->setName( sCurName );
		// default chainType=chainBody
	cPCurCyclomer->setChainType( chainBody );
	((CyclomerDatabase*)PDatabase)->addCyclomer( cPCurCyclomer );
#ifdef	GRAMMAR_DEBUG
	cout << "Defining cyclomer: " << sCurName << endl;
#endif
    }
    cyclomerBody 
    { 
		/* remember to account for mid-rule */ 
    };



cyclomerName : T_STRING	
    { 
#if 0
    char sTemp[256];
	strcpy( sTemp, &$1[1] );
	sTemp[strlen(sTemp)-1] = '\0';
	strcpy($<str>$,sTemp);
#endif
        strcpy($<str>$,$1);
    }
| T_STAR 
    { 
	strcpy( $<str>$, "-" ); 
    };



cyclomerBody :	T_OPENBLOCK 
    {
#ifdef	GRAMMAR_DEBUG
	cout << "openblock" << endl;
#endif
    } 
    residueStructure cyclomerAttrs T_CLOSEBLOCK ;



residueStructure :	T_RESIDUE 
    {
#ifdef	GRAMMAR_DEBUG
	cout << "Defining residue" << endl;
#endif
	rPCurResidue = new Residue();
	cPCurCyclomer->setResidue( rPCurResidue );
	rPCurResidue->setName( sCurName );
    } 
    T_OPENBLOCK resStmts T_CLOSEBLOCK 
    {
#ifdef	GRAMMAR_DEBUG
	cout << "Defined residue" << endl;
#endif
    };



resStmts :	resStmts resStmt
| resStmt;



resStmt :	atomStructStmt T_SEMICOLON
| bondStructStmt T_SEMICOLON	;

bondStructStmt :	T_BOND order atomId atomId
    {
    Atom*	aP1;
    Atom*	aP2;

#ifdef	GRAMMAR_DEBUG
	cout << "Defined bond order: " << $<bOrder>2 << " [ " 
			<< $<str>3 << "-" << $<str>4 << "]" << endl;
#endif
	aP1 = rPCurResidue->atomWithId( $<ival>3 );	
	aP2 = rPCurResidue->atomWithId( $<ival>4 );	
//	aP1->bondTo( aP2, static_cast<BondOrder>($<bOrder>2) );
	aP1->bondTo( aP2, &($<bOrder>2) );
    };



order :		T_SINGLE 
    { 
	$<bOrder>$ = SingleBond; 
    }
| T_DOUBLE 
    { 
	$<bOrder>$ = DoubleBond; 
    }
| T_TRIPLE 
    { 
	$<bOrder>$ = TripleBond; 
    };



atomStructStmt : T_ATOM atomId atomName element T_STRING atomType charges 
    {
    Atom* a;
    int	i;
	a = new Atom();
	a->setName( $<str>3 );
	a->setElement( $<str>4 );
	a->setHybridization($<str>5);
	i = ((CyclomerDatabase*)PDatabase)->parameterSet()->getTypeIndex($<str>6);
	a->setTypeString($<str>6);
	a->setType(i);
	a->setCharge($<dval>7);
	rPCurResidue->addAtom( a );
	a->setId($<ival>2);
#ifdef	GRAMMAR_DEBUG
	cout << "Defined atom: " << $<str>2 << endl;
#endif
    };



cyclomerAttrs :	cyclomerAttrs attr			{}
| attr 					{ };



attr	:	headAttr T_SEMICOLON
| centerAttr T_SEMICOLON
| commentAttr T_SEMICOLON
| tailAttr T_SEMICOLON
| headAxesAttr T_SEMICOLON
| tailAxesAttr T_SEMICOLON
| outLinkAttr T_SEMICOLON
| inLinkAttr T_SEMICOLON
| derivativeBondsAttr T_SEMICOLON
| terminatorStartAttr T_SEMICOLON
| terminatorStopAttr T_SEMICOLON;



commentAttr	: T_COMMENT T_STRING
    {
	cPCurCyclomer->setComment( $<str>2 );
    };


headAxesAttr 	: T_HEADAXES atomIdList
    {
	cPCurCyclomer->setHeadAxesFromIds(vsIdList);
    };



tailAxesAttr 	: T_TAILAXES atomIdList		
    {
	cPCurCyclomer->setTailAxesFromIds(vsIdList);
    };



headAttr 	: T_HEAD atomIdList
    {
	cPCurCyclomer->setHeadFromIds(vsIdList);
    };



centerAttr 	: T_CENTER atomIdList		
    {
	cPCurCyclomer->setCenterFromIds(vsIdList);
    };



tailAttr	: T_TAIL atomIdList	
    {
	cPCurCyclomer->setTailFromIds(vsIdList);
    };



inLinkAttr :	T_INLINK atomIdList			
    {
	cPCurCyclomer->setInLinkFromIds(vsIdList);
    };



outLinkAttr :	T_OUTLINK atomIdList		
    {
	cPCurCyclomer->setOutLinkFromIds(vsIdList);
    };



derivativeBondsAttr :	T_DERIVATIVEBONDS T_OPENBLOCK atomPairList T_CLOSEBLOCK		{};



terminatorStartAttr :	T_TERMINATOR T_START 	
    { 
	cPCurCyclomer->setChainType(chainBegin);
    };



terminatorStopAttr :	T_TERMINATOR T_STOP 	
    {
	cPCurCyclomer->setChainType(chainEnd);
    };



atomIdList :	atomId atomIdList			
    {
	vsIdList.insert( vsIdList.begin(), $<ival>1 );
    }
| atomId			
    {
	vsIdList.clear();
	vsIdList.insert( vsIdList.end(), $<ival>1 );
    };


atomPairList :	atomPair atomPairList				{}
| atomPair					{};



atomPair :	T_OPENBLOCK atomName atomName T_CLOSEBLOCK	{};



charges	:	number number	{ $<dval>$ = $1; };



atomName :	T_STRING;



element	:	T_STRING;



atomType :	T_INTEGER 
    {
	sprintf( $<str>$, "%d", $1 );
    }
| T_STRING;
   




position	:	T_OPENPAREN number T_AT number T_AT number T_CLOSEPAREN	{
			$<pos>$[0] = $<dval>2;
			$<pos>$[1] = $<dval>4;
			$<pos>$[2] = $<dval>6;
		};


atomId :	T_INTEGER;

number	:	T_INTEGER 
    { 
	$<dval>$ = $<ival>1; 
    };
| T_FLOAT;


%%

#undef	yyFlexLexer
#define	yyFlexLexer mcFlexLexer
#include	<FlexLexer.h>

mcFlexLexer	GmcLex;

int	yylex()
{
int	ret;
	ret = GmcLex.yylex();
	yylloc.first_line = GmcLex.lineno();
	return ret;
}

int	yyerror( char* s ){
    fprintf( stderr, "%s\n", s );
    return 0;
}
