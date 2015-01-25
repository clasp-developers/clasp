/*
    File: mbbComp.cc
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
       
//
// (C) 2004 Christian E. Schafmeister
//




#include <iostream>

#include <stdio.h>
#include <clasp/core/foundation.h>
#include <vector3.h>
#include <matrix.h>
#include <fragments.h>
#include <trimerDatabase.h>
#include <residue.h>
#include <loop.h>
#include <polymer.h>
#include <parmSet.h>
#include <amber.h>
#include <moe.h>

using namespace core;
extern	int	yyparse();


TrimerDatabase	GcdbDatabase;



bool	minimizerCallback(void* p) {
    printf( "Energy = %f\n", ((Minimizer*)p)->energy() );
    return true;
}

main( int argc, char* argv[] )
{
TrimerDatabase	cb;
Loop			l;
SpanningLoop		sl;
Fragment*		c;
Residue*		r;
Atom*			a;
Atom*			a1;
Atom*			a2;
Atom*			a3;
Atom*			a4;
Bond*			b;
string			sName;
VectorStrings		names;
Polymer*		pc;
Molecule*		mol;
BondOrder		order;
Amber			m;
vector<Atom*>		atoms;
vector<Atom*>::iterator	aa;
Aggregate		agg;
int			iii;
VectorFragments		vc;
    cb.readFromFile("test.db");

    vc = cb.bodyFragments();

    names.push_back("strt");
    names.push_back("zuu");
    names.push_back("zuu");
    names.push_back("zuu");
    names.push_back("end");

for (iii=0; iii<100; iii++ ) {
    moeReadAggregate( cb.parameterSet(), "test.moe", &agg );
    l.loopTopGoal( agg.firstMolecule()->firstResidue(), ATOMS );
    cout << "Atoms in |test.moe|-------------------" << std::endl;
    while ( l.advanceLoopAndProcess() ) {
	a = l.getAtom();
	cout << "Atom: ";
	a->dump();
	cout << std::endl;
    }

    
    sName = "end";
    c = cb.fragmentWithName(sName);
    r = c->getResidue();

    sl = _lisp->create<SpanningLoop_O>();
    sl->setTop(r->firstAtom());
//    sl.loopTopAtomGoal( r->firstAtom(), S_PANNINGTREE );
    cout << "Atoms in |end|-------------------" << std::endl;
    while ( sl->advanceLoopAndProcess() ) {
	a = sl->getAtom();
	atoms.push_back(a);
	cout << "Atom: ";
	a->dump();
	cout << std::endl;
    }

#if 0		// We don't need this test anymore
		// Delete all atoms
    for (aa=atoms.begin();aa!=atoms.end();aa++) {
	r->removeAtomDeleteBonds((*aa));
    }
#endif
    



    l.loopTopResidueGoal( r, ATOMS );
    cout << "Atoms in |end|-------------------" << std::endl;
    while ( l.advanceLoopAndProcess() ) {
	a = l.getAtom();
	cout << "Atom: ";
	a->dump();
	cout << std::endl;
    }

    l.loopTopResidueGoal(r,BONDS);
    cout << "Bonds-------------------" << std::endl;
    while ( l.advanceLoopAndProcess() ) {
	l.getBond(&a1,&a2,&order);
	cout << "Bond " << a1->getName() << "-" << a2->getName() << std::endl;
    }

    l.loopTopResidueGoal(r,ANGLES);
    cout << "Angles-------------------" << std::endl;
    while ( l.advanceLoopAndProcess() ) {
	a = l.getAtom();
	l.getAngle(&a1,&a2,&a3);
	cout << "Angle " << a1->getName() << "-" << a2->getName() << "-" << a3->getName() << std::endl;
    }

    pc = new Polymer();
    pc->setSophisticationLevelCrude();
    pc->buildSequence( &cb, names );
    cout << "Polymer= " << pc << std::endl;
    mol = pc->getMolecule();
    l.loopTopMoleculeGoal( mol, PROPERS );
    cout << "Atoms in polymer -------------------" << std::endl;
    while (l.advanceLoopAndProcess() ) {
	l.getProper(&a1,&a2,&a3,&a4);
	cout << "Proper " << a1->getName() << "-" << a2->getName() << "-" << a3->getName() << "-" << a4->getName() << std::endl;
    }


    m.useParmSetOldAndMatter( cb.parameterSet(), mol );
//    m.minimize();    
//    cout << "Minimized energy of molecule" << std::endl;

}

}
