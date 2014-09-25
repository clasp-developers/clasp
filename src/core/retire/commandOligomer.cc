/*
    File: commandOligomer.cc
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
       
#define	DEBUG_LEVEL_FULL

#define	TEST_OLIGOMER


#include "lisp.h"
#include "commandOligomer.h"
#include "archiveNode.h"
#include "archive.h"
#include "oligomer.h"
#include "monomer.h"
#include "coupling.h"
#include "command.h"
#include "builderDatabaseReference.h"
#include "stringSet.h"
#include "stringList.h"
#include "builderDatabase.h"
#include "wrappers.h"


namespace mbb {


REGISTER_CLASS(O_CommandOligomer);
REGISTER_CLASS(O_CommandOligomer_AddMultiMonomerAsLeaf);
REGISTER_CLASS(O_CommandOligomer_DeleteMonomer);
REGISTER_CLASS(O_CommandOligomer_InsertBeforeMonomer);
REGISTER_CLASS(O_CommandOligomer_MutateAllSelectedMonomers);
REGISTER_CLASS(O_CommandOligomer_ModifyMonomer);
REGISTER_CLASS(O_CommandOligomer_MutateAllSelectedCouplings);
REGISTER_CLASS(O_CommandOligomer_ModifyCoupling);




void	O_CommandOligomer::setup(RPOligomer olig, RPBuilderDatabase cdb)
{_G();
    this->_Oligomer = olig;
//    this->_BuilderDatabaseRef = O_BuilderDatabaseReference::create(this->lisp(),cdb,"");
}



void	O_CommandOligomer::archiveBase(::mbb::RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveObject("oligomer",this->_Oligomer);
//    node->archiveObject("bdbRef",this->_BuilderDatabaseRef);
}


void	O_CommandOligomer::doIt()
{_G();
    ASSERT_NOT_NULL(this->_Oligomer);
//    ASSERT_NOT_NULL(this->_BuilderDatabaseRef);
}


void	O_CommandOligomer::undoIt()
{_G();
    ASSERT_NOT_NULL(this->_Oligomer);
//    ASSERT_NOT_NULL(this->_BuilderDatabaseRef);
}



// -----------------------------------------------------------
// -----------------------------------------------------------
//  O_CommandOligomer_AddMultiMonomerAsLeaf
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------



void O_CommandOligomer_AddMultiMonomerAsLeaf::setup(
		RPOligomer olig, RPBuilderDatabase cdb,
		RPMonomer monomerFrom, const string& couplingName,
		const string& groupName, Vector2 pos2D )
{_G();
    this->O_CommandOligomer::setup(olig,cdb);
    this->_MonomerFrom = monomerFrom;
    this->_CouplingName = couplingName;
    this->_GroupName = groupName;
    this->_Pos2D = pos2D;
}

void	O_CommandOligomer_AddMultiMonomerAsLeaf::archive(::mbb::RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->archiveObject("monomerFrom",this->_MonomerFrom);
    node->archiveObject("monomerAdded",this->_MonomerAdded);
    node->attribute("couplingName",this->_CouplingName);
    node->attribute("groupName",this->_GroupName);
    node->archivePlainObject<Vector2>( "pos2","Vector2",
    					this->_Pos2D );
}


void	O_CommandOligomer_AddMultiMonomerAsLeaf::doIt()
{_G();
RPDirectionalCoupling 	coup;
RPMultiMonomer		monTo;
RPMonomer		monFrom;
    this->O_CommandOligomer::doIt();
    coup = O_DirectionalCoupling::create(this->lisp());
    coup->setName(this->_CouplingName);
    monTo = O_MultiMonomer::create(this->lisp());
    monTo->setGroupName(this->_GroupName);
    monTo->setPosition2D(this->_Pos2D);
    this->_MonomerAdded = monTo;
    coup->setOutMonomer(monTo);
    monTo->setInCoupling(coup);
    	// Now attach it
    monFrom = this->_MonomerFrom;
    monFrom->addOutCoupling(coup);
    coup->setInMonomer(monFrom);
    this->_Oligomer->addMonomer(monTo);
    this->_Oligomer->addCoupling(coup);
    monFrom->throwIfBadConnections();
    monTo->throwIfBadConnections();
    coup->throwIfBadConnections();
#ifdef	TEST_OLIGOMER
    	// Check the entire oligomer for problems.
	// Turn this off once we know this code is ok
    this->_Oligomer->throwIfBadConnections();
#endif
    set<RPMonomer>	changedMonomers;
    changedMonomers.insert(this->_MonomerFrom);
    changedMonomers.insert(this->_MonomerAdded);
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signalConnectivityChanged();
}

void	O_CommandOligomer_AddMultiMonomerAsLeaf::undoIt()
{_G();
int		couplingCount;
RPDirectionalCoupling 	couplingToRemove;
RPMonomer	monomerToRemove;
RPMonomer	monomerToRemain;
    this->O_CommandOligomer::undoIt();
    monomerToRemove = this->_MonomerAdded;
    if ( !monomerToRemove->hasInCoupling() )
    {
        TOSS(_lisp->create<O_ContentException>("Trying to remove a leaf monomer that has no in coupling! in "+monomerToRemove->description()));
    }
    couplingCount = monomerToRemove->numberOfCouplings();
    if ( couplingCount > 1 )
    {
        TOSS(_lisp->create<O_ContentException>("Trying to remove a leaf monomer but it has out couplings! in "+monomerToRemove->description()));
    }
    couplingToRemove = monomerToRemove->getInCoupling()->as<O_DirectionalCoupling>();
    monomerToRemain = this->_MonomerFrom;
    monomerToRemain->removeCoupling(couplingToRemove);
    this->_Oligomer->removeMonomer(monomerToRemove);
    this->_Oligomer->removeCoupling(couplingToRemove);
    monomerToRemain->throwIfBadConnections();
#ifdef	TEST_OLIGOMER
    	// Check the entire oligomer for problems.
	// Turn this off once we know this code is ok
    this->_Oligomer->throwIfBadConnections();
#endif
    set<RPMonomer>	changedMonomers;
    changedMonomers.insert(this->_MonomerFrom);
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signalConnectivityChanged();
}




// -----------------------------------------------------------
// -----------------------------------------------------------
//  O_CommandOligomer_DeleteMonomer
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------




void	O_CommandOligomer_DeleteMonomer::archive(::mbb::RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->archiveObject("monomerToDelete",this->_MonomerToDelete);
    node->archiveObject("couplingToDelete",this->_CouplingToDelete);
    node->archiveVector0("couplingsToRewire",this->_CouplingsToRewire);
    node->archiveObject("monomerToRewireTo",this->_MonomerToRewireTo);
}


void O_CommandOligomer_DeleteMonomer::setup(RPOligomer olig, RPBuilderDatabase cdb,
				RPMonomer monomerToDelete )
{_G();
    this->O_CommandOligomer::setup(olig,cdb);
    this->_MonomerToDelete = monomerToDelete;
    this->_CouplingToDelete = monomerToDelete->getInCoupling()->as<O_DirectionalCoupling>();
    this->_CouplingsToRewire = monomerToDelete->getOutCouplings();
    this->_MonomerToRewireTo = this->_CouplingToDelete->getInMonomer();
}

void	O_CommandOligomer_DeleteMonomer::doIt()
{_G();
set<RPMonomer>	changedMonomers;
    changedMonomers.clear();
    this->O_CommandOligomer::doIt();
    this->_Oligomer->removeMonomer(this->_MonomerToDelete);
    this->_Oligomer->removeCoupling(this->_CouplingToDelete);
    this->_MonomerToRewireTo->removeCouplingToMonomer(this->_MonomerToDelete);
    List<O_Coupling>::iterator	c;
    LOG(BF("status") ); // vp0(("status"));
    for ( c=this->_CouplingsToRewire.begin();
    	c!=this->_CouplingsToRewire.end(); c++ )
    {
	this->_MonomerToRewireTo->addOutCoupling(*c);
	RPDirectionalCoupling dirCoup = (*c)->as<O_DirectionalCoupling>();
	dirCoup->setInMonomer(this->_MonomerToRewireTo); // added
	dirCoup->throwIfBadConnections();
	changedMonomers.insert((dirCoup)->getOutMonomer());
    }
    changedMonomers.insert(this->_MonomerToRewireTo);

    LOG(BF("status") ); // vp0(("status"));
    this->_MonomerToRewireTo->throwIfBadConnections();
    LOG(BF("status") ); // vp0(("status"));
#ifdef	TEST_OLIGOMER
    	// Check the entire oligomer for problems.
	// Turn this off once we know this code is ok
    this->_Oligomer->throwIfBadConnections();
#endif

    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signalConnectivityChanged();
}

void	O_CommandOligomer_DeleteMonomer::undoIt()
{_G();
set<RPMonomer>	changedMonomers;
    changedMonomers.clear();
    this->O_CommandOligomer::undoIt();
    List<O_Coupling>::iterator	c;
    for ( c=this->_CouplingsToRewire.begin();
    	c!=this->_CouplingsToRewire.end(); c++ )
    {
	this->_MonomerToRewireTo->removeCoupling(*c);
	RPDirectionalCoupling dc = (*c)->as<O_DirectionalCoupling>();
	dc->setInMonomer(this->_MonomerToDelete);
	dc->throwIfBadConnections();
	changedMonomers.insert(dc->getOutMonomer());
    }
    changedMonomers.insert(this->_CouplingToDelete->getInMonomer() );
    changedMonomers.insert(this->_CouplingToDelete->getOutMonomer() );
    changedMonomers.insert(this->_MonomerToDelete);	// may be redundant _CouplingToDelete points to it
    this->_MonomerToRewireTo->addOutCoupling(this->_CouplingToDelete);
    this->_Oligomer->addMonomer(this->_MonomerToDelete);
    this->_Oligomer->addCoupling(this->_CouplingToDelete);
    this->_MonomerToRewireTo->throwIfBadConnections();
    this->_CouplingToDelete->throwIfBadConnections();
    this->_MonomerToDelete->throwIfBadConnections();
#ifdef	TEST_OLIGOMER
    	// Check the entire oligomer for problems.
	// Turn this off once we know this code is ok
    this->_Oligomer->throwIfBadConnections();
#endif
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signalConnectivityChanged();
}



// -----------------------------------------------------------
// -----------------------------------------------------------
//  O_CommandOligomer_InsertBeforeMonomer
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------



List<O_Monomer>	monomersBelow(RPMonomer top)
{
List<O_Monomer>			spanningList;
List<O_Coupling> 		outCouplings;
List<O_Coupling>::iterator	ic;
uint			spanIndex;
RPMonomer			spanNext;
RPMonomer			outMon;
    spanningList.clear();
    spanningList.append(top);
    spanIndex = 0;
    while ( spanIndex < spanningList.size() )
    {
        spanNext = spanningList[spanIndex];
	spanIndex++;
	outCouplings = spanNext->getOutCouplings();
	for ( ic=outCouplings.begin(); ic!=outCouplings.end(); ic++ )
	{
	    if ( (*ic)->isOfClass<O_DirectionalCoupling>() )
	    {
		RPDirectionalCoupling dc = (*ic)->as<O_DirectionalCoupling>();
	        outMon = dc->getOutMonomer();
	        spanningList.append(outMon);
	    }
	}
    }
    return spanningList;
}


void O_CommandOligomer_InsertBeforeMonomer::setup(
				RPOligomer olig,
				RPBuilderDatabase cdb,
				RPMonomer beforeMon,
				const string& groupName,
				const string& couplingName)
{_G();
RPDirectionalCoupling	inCoupling;
RPMonomer	inMonomer;
Vector2		start, stop;
List<O_Monomer>	tree;
RPMultiMonomer	mon;
RPDirectionalCoupling	coup;
    this->O_CommandOligomer::setup(olig,cdb);
    this->_MonomerToInsertBefore = beforeMon;
    inCoupling = beforeMon->getInCoupling()->as<O_DirectionalCoupling>();
    ASSERT_NOT_NULL(inCoupling);
    inMonomer = inCoupling->getInMonomer();
    ASSERT_NOT_NULL(inMonomer);
    start = inMonomer->getPosition2D();
    stop = beforeMon->getPosition2D();
    this->_Offset = Vector2( 0,stop.getY()-start.getY());
    this->_MonomersToMove = monomersBelow(beforeMon);
    mon = O_MultiMonomer::create(this->lisp());
    mon->setGroupName(groupName);
    coup = O_DirectionalCoupling::create(this->lisp());
    coup->setName(couplingName);
    mon->setPosition2D(stop);
    this->_CouplingToInsertAfter = beforeMon->getInCoupling()->as<O_DirectionalCoupling>();
    this->_MonomerNew = mon;
    this->_CouplingToInsertBeforeNew = coup;
}

void	O_CommandOligomer_InsertBeforeMonomer::archive(::mbb::RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->archiveObject("monomerToInsertBefore",this->_MonomerToInsertBefore);
    node->archiveVector0("monomersToMove",this->_MonomersToMove);
    node->archiveObject("CouplingToInsertAfter",this->_CouplingToInsertAfter);
    node->archiveObject("MonomerNew",this->_MonomerNew);
    node->archiveObject("CoupingToInsertBeforeNew",this->_CouplingToInsertBeforeNew);
    node->archiveObject("MonomerToInsertBefore",this->_MonomerToInsertBefore);
    node->archivePlainObject<Vector2>( "pos","Vector2", this->_Offset );
}


void	O_CommandOligomer_InsertBeforeMonomer::doIt()
{_G();
Vector2		pos;
    this->O_CommandOligomer::doIt();

    List<O_Monomer>::iterator	im;
    for ( im=this->_MonomersToMove.begin(); im!=this->_MonomersToMove.end(); im++ )
    {
	pos = (*im)->getPosition2D();
	pos = pos.add(this->_Offset);
	(*im)->setPosition2D(pos);
    }
    this->_Oligomer->addCoupling(this->_CouplingToInsertBeforeNew);
    this->_Oligomer->addMonomer(this->_MonomerNew);
    this->_CouplingToInsertAfter->setOutMonomer(this->_MonomerNew);
    this->_MonomerNew->setInCoupling(this->_CouplingToInsertAfter);
    this->_MonomerNew->addOutCoupling(this->_CouplingToInsertBeforeNew);
    this->_CouplingToInsertBeforeNew->setInMonomer(this->_MonomerNew);
    LOG(BF("Setting outMonomer for coupling: %s") % this->_CouplingToInsertBeforeNew->description().c_str() ); // vp0(("Setting outMonomer for coupling: %s",this->_CouplingToInsertBeforeNew->description().c_str()));
    this->_CouplingToInsertBeforeNew->setOutMonomer(this->_MonomerToInsertBefore);
    LOG(BF("Setting in coupling for monomerToInsertBefore: %s") % this->_MonomerToInsertBefore->description().c_str() ); // vp0(("Setting in coupling for monomerToInsertBefore: %s",this->_MonomerToInsertBefore->description().c_str()));
    this->_MonomerToInsertBefore->setInCoupling(this->_CouplingToInsertBeforeNew);
    this->_MonomerToInsertBefore->throwIfBadConnections();
    this->_MonomerNew->throwIfBadConnections();
    this->_CouplingToInsertAfter->throwIfBadConnections();
    this->_CouplingToInsertBeforeNew->throwIfBadConnections();
#ifdef	TEST_OLIGOMER
    	// Check the entire oligomer for problems.
	// Turn this off once we know this code is ok
    this->_Oligomer->throwIfBadConnections();
#endif
    set<RPMonomer>	changedMonomers;
    changedMonomers.insert(this->_MonomerNew);
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signalConnectivityChanged();
}

void	O_CommandOligomer_InsertBeforeMonomer::undoIt()
{_G();
Vector2		putBack, pos;
RPDirectionalCoupling	coupling;
RPMonomer	otherMonomer;
    this->O_CommandOligomer::undoIt();
    putBack = Vector2(-this->_Offset.getX(),-this->_Offset.getY());
    List<O_Monomer>::iterator	im;
    for ( im=this->_MonomersToMove.begin(); im!=this->_MonomersToMove.end(); im++ )
    {
	pos = (*im)->getPosition2D();
	pos = pos.add(this->_Offset);
	(*im)->setPosition2D(pos);
    }
    this->_CouplingToInsertAfter->setOutMonomer(this->_MonomerToInsertBefore);
    this->_MonomerToInsertBefore->setInCoupling(this->_CouplingToInsertAfter);
    this->_Oligomer->removeCoupling(this->_CouplingToInsertBeforeNew);
    this->_Oligomer->removeMonomer(this->_MonomerNew);
    this->_MonomerNew->resetInCoupling();
    this->_CouplingToInsertBeforeNew->resetOut();
    this->_MonomerToInsertBefore->throwIfBadConnections();
    this->_CouplingToInsertAfter->throwIfBadConnections();
#ifdef	TEST_OLIGOMER
    	// Check the entire oligomer for problems.
	// Turn this off once we know this code is ok
    this->_Oligomer->throwIfBadConnections();
#endif
    set<RPMonomer>	changedMonomers;
    O_Monomer::weakCouplingValueIterator	mmi;
    for ( mmi=this->_MonomerNew->begin_value_WeakCouplings(); mmi!=this->_MonomerNew->end_value_WeakCouplings(); mmi++ )
    {
	ASSERT_NOT_NULL(*mmi);
	coupling = (*mmi).lock()->as<O_DirectionalCoupling>();
	otherMonomer = coupling->getOtherSideMonomer(this->_MonomerNew);
	changedMonomers.insert(otherMonomer);
    }
    this->_Oligomer->checkMonomersAndNotNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signalConnectivityChanged();
}






// -----------------------------------------------------------
// -----------------------------------------------------------
//  O_CommandOligomer_MutateAllSelectedMonomers
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------



void	O_CommandOligomer_MutateAllSelectedMonomers::initialize()
{
    this->Base::initialize();
    this->_OldGroupNames = O_StringList::create(this->lisp());
}

//
// Destructor
//


void O_CommandOligomer_MutateAllSelectedMonomers::setup(
				RPOligomer olig,
				RPBuilderDatabase cdb,
				const string& newGroupName )
{_G();
O_Oligomer::monomerIterator	mi;
    this->O_CommandOligomer::setup(olig,cdb);
    this->_NewGroupName = newGroupName;
    this->_MonomersToMutate.clear();
    this->_OldGroupNames->clear();
    for (mi=olig->begin_Monomers(); mi!=olig->end_Monomers(); mi++ )
    {
        if ( (*mi)->isSelected() )
	{
	    this->_MonomersToMutate.append(*mi);
	    this->_OldGroupNames->append((*mi)->getGroupName());
	}
    }
}



void	O_CommandOligomer_MutateAllSelectedMonomers::archive(::mbb::RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->attribute("newGroupName",this->_NewGroupName);
    node->archiveVector0("monomersToMutate",this->_MonomersToMutate);
    node->archiveObject("oldGroupNames",this->_OldGroupNames);
}




void	O_CommandOligomer_MutateAllSelectedMonomers::doIt()
{_G();
List<O_Monomer>::iterator	it;
    this->O_CommandOligomer::doIt();
    LOG(BF("Setting new group names to(%s)") % this->_NewGroupName.c_str()  ); // vp0(("Setting new group names to(%s)",this->_NewGroupName.c_str() ));
    for ( it=this->_MonomersToMutate.begin(); it!=this->_MonomersToMutate.end(); it++ )
    {
        (*it)->setGroupName(this->_NewGroupName);
    }
    LOG(BF("Checking monomers for errors") ); // vp0(("Checking monomers for errors"));
    ASSERT_NOT_NULL(this->_Oligomer);
    set<RPMonomer>	changedMonomers(this->_MonomersToMutate.begin(),this->_MonomersToMutate.end());
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signal_monomerContentsChanged();
}






void	O_CommandOligomer_MutateAllSelectedMonomers::undoIt()
{_G();
List<O_Monomer>::iterator	it;
O_StringList::iterator		si;
    this->O_CommandOligomer::doIt();
    for ( it=this->_MonomersToMutate.begin(), si=this->_OldGroupNames->begin();
    	it!=this->_MonomersToMutate.end(); it++, si++ )
    {
        (*it)->setGroupName((*si));
    }

    set<RPMonomer>	changedMonomers(this->_MonomersToMutate.begin(),this->_MonomersToMutate.end());
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signal_monomerContentsChanged();
}





// -----------------------------------------------------------
// -----------------------------------------------------------
//  O_CommandOligomer_ModifyMonomer
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------




void O_CommandOligomer_ModifyMonomer::setup( RPOligomer olig,
		RPBuilderDatabase cdb, RPMonomer mon, const string& newGroupName,
		const string& newAliases, const string& newComment )
{_G();
O_Oligomer::monomerIterator	mi;
    ASSERT_NOT_NULL(olig);
    ASSERT_NOT_NULL(cdb);
    this->O_CommandOligomer::setup(olig,cdb);
    this->_Monomer = mon;
    this->_NewGroupName = newGroupName;
    this->_NewAliases = newAliases;
    this->_NewComment = newComment;
    this->_OldGroupName = mon->getGroupName();
    this->_OldAliases = mon->getAliasesAsString();
    this->_OldComment = mon->getComment();
}



void	O_CommandOligomer_ModifyMonomer::archive(::mbb::RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->archiveObject("monomer",this->_Monomer );
    node->attribute("newGroupName",this->_NewGroupName);
    node->attribute("newAliases",this->_NewAliases);
    node->attribute("newComment",this->_NewComment);
    node->attribute("oldGroupName",this->_OldGroupName);
    node->attribute("oldAliases",this->_OldAliases);
    node->attribute("oldComment",this->_OldComment);
}




void	O_CommandOligomer_ModifyMonomer::doIt()
{_G();
set<RPMonomer>		changedMonomers;
    this->O_CommandOligomer::doIt();
    LOG(BF("Setting new group names to(%s)") % this->_NewGroupName.c_str()  ); // vp0(("Setting new group names to(%s)",this->_NewGroupName.c_str() ));
    this->_Monomer->setGroupName(this->_NewGroupName);
    this->_Monomer->setAliasesFromSymbols(this->_NewAliases);
    this->_Monomer->setComment(this->_NewComment);

		// Check the monomers and their neighbors for errors
    changedMonomers.clear();
    changedMonomers.insert(this->_Monomer);
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signal_monomerContentsChanged();
}






void	O_CommandOligomer_ModifyMonomer::undoIt()
{_G();
set<RPMonomer>		changedMonomers;
    this->O_CommandOligomer::undoIt();
    LOG(BF("Setting old group names to(%s)") % this->_OldGroupName.c_str()  ); // vp0(("Setting old group names to(%s)",this->_OldGroupName.c_str() ));
    this->_Monomer->setGroupName(this->_OldGroupName);
    this->_Monomer->setAliasesFromString(this->_OldAliases);
    this->_Monomer->setComment(this->_OldComment);

		// Check the monomers and their neighbors for errors
    changedMonomers.clear();
    changedMonomers.insert(this->_Monomer);
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signal_monomerContentsChanged();
}




// -----------------------------------------------------------
// -----------------------------------------------------------
//  O_CommandOligomer_MutateAllSelectedCouplings
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------



void	O_CommandOligomer_MutateAllSelectedCouplings::initialize()
{
    this->Base::initialize();
    this->_OldCouplingNames = O_StringList::create(this->lisp());
}

//
// Destructor
//


void O_CommandOligomer_MutateAllSelectedCouplings::setup(
				RPOligomer olig,
				RPBuilderDatabase cdb,
				const string& newCouplingName )
{_G();
O_Oligomer::couplingIterator	mi;
    this->O_CommandOligomer::setup(olig,cdb);
    this->_NewCouplingName = newCouplingName;
    this->_CouplingsToMutate.clear();
    this->_OldCouplingNames->clear();
    for (mi=olig->begin_Couplings(); mi!=olig->end_Couplings(); mi++ )
    {
        if ( (*mi)->isSelected() )
	{
	    RPDirectionalCoupling dc = (*mi)->as<O_DirectionalCoupling>();
	    this->_CouplingsToMutate.append(dc);
	    this->_OldCouplingNames->append(dc->getName());
	}
    }
}



void	O_CommandOligomer_MutateAllSelectedCouplings::archive(::mbb::RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->attribute("newCouplingName",this->_NewCouplingName);
    node->archiveVector0("monomersToMutate",this->_CouplingsToMutate);
    node->archiveObject("oldCouplingNames",this->_OldCouplingNames);
}




void	O_CommandOligomer_MutateAllSelectedCouplings::doIt()
{_G();
List<O_DirectionalCoupling>::iterator	it;
set<RPMonomer>	changedMonomers;
    this->O_CommandOligomer::doIt();
    LOG(BF("Setting new group names to(%s)") % this->_NewCouplingName.c_str()  ); // vp0(("Setting new group names to(%s)",this->_NewCouplingName.c_str() ));
    for ( it=this->_CouplingsToMutate.begin(); it!=this->_CouplingsToMutate.end(); it++ )
    {
        (*it)->setName(this->_NewCouplingName);
	changedMonomers.insert((*it)->getInMonomer());
	changedMonomers.insert((*it)->getOutMonomer());
    }
    LOG(BF("Checking monomers for errors") ); // vp0(("Checking monomers for errors"));
    ASSERT_NOT_NULL(this->_Oligomer);
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signal_monomerContentsChanged();
}






void	O_CommandOligomer_MutateAllSelectedCouplings::undoIt()
{_G();
List<O_DirectionalCoupling>::iterator	it;
O_StringList::iterator		si;
set<RPMonomer>	changedMonomers;
    this->O_CommandOligomer::doIt();
    for ( it=this->_CouplingsToMutate.begin(), si=this->_OldCouplingNames->begin();
    	it!=this->_CouplingsToMutate.end(); it++, si++ )
    {
        (*it)->setName((*si));
	changedMonomers.insert((*it)->getInMonomer());
	changedMonomers.insert((*it)->getOutMonomer());
    }
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signal_monomerContentsChanged();
}





// -----------------------------------------------------------
// -----------------------------------------------------------
//  O_CommandOligomer_ModifyCoupling
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------




void O_CommandOligomer_ModifyCoupling::setup( RPOligomer olig,
		RPBuilderDatabase cdb, RPDirectionalCoupling mon, const string& newCouplingName )
{_G();
O_Oligomer::monomerIterator	mi;
    this->O_CommandOligomer::setup(olig,cdb);
    this->_Coupling = mon;
    this->_NewCouplingName = newCouplingName;
    this->_OldCouplingName = mon->getName();
}



void	O_CommandOligomer_ModifyCoupling::archive(::mbb::RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->archiveObject("monomer",this->_Coupling );
    node->attribute("newCouplingName",this->_NewCouplingName);
    node->attribute("oldCouplingName",this->_OldCouplingName);
}




void	O_CommandOligomer_ModifyCoupling::doIt()
{_G();
set<RPMonomer>		changedMonomers;
    this->O_CommandOligomer::doIt();
    LOG(BF("Setting new group names to(%s)") % this->_NewCouplingName.c_str()  ); // vp0(("Setting new group names to(%s)",this->_NewCouplingName.c_str() ));
    this->_Coupling->setName(this->_NewCouplingName);

		// Check the monomers and their neighbors for errors
    changedMonomers.insert(this->_Coupling->getInMonomer());
    changedMonomers.insert(this->_Coupling->getOutMonomer());
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signal_monomerContentsChanged();
}






void	O_CommandOligomer_ModifyCoupling::undoIt()
{_G();
set<RPMonomer>		changedMonomers;
    this->O_CommandOligomer::undoIt();
    LOG(BF("Setting old group names to(%s)") % this->_OldCouplingName.c_str()  ); // vp0(("Setting old group names to(%s)",this->_OldCouplingName.c_str() ));
    this->_Coupling->setName(this->_OldCouplingName);
		// Check the monomers and their neighbors for errors
    changedMonomers.insert(this->_Coupling->getInMonomer());
    changedMonomers.insert(this->_Coupling->getOutMonomer());
    this->_Oligomer->checkMonomersAndNeighborsForErrors(this->lisp()->getBuilderDatabase(), changedMonomers );
    this->_Oligomer->signal_monomerContentsChanged();
}








};




using namespace mbb;


#ifdef	USEBOOSTPYTHON //[
__INITIALIZE_PYTHON_AFTER(InitPython_CommandOligomer,InitPython_Command);
void InitPython_CommandOligomer()
{
    boost::python::class_<O_CommandOligomer,
	boost::shared_ptr<O_CommandOligomer>,
	boost::python::bases <mbb::O_Command>,
	boost::noncopyable> ("O_CommandOligomer", boost::python::no_init )
    ;

    boost::python::class_<O_CommandOligomer_AddMultiMonomerAsLeaf,
	boost::shared_ptr<O_CommandOligomer_AddMultiMonomerAsLeaf>,
	boost::python::bases <mbb::O_CommandOligomer>,
	boost::noncopyable> ("O_CommandOligomer_AddMultiMonomerAsLeaf", boost::python::no_init )
	.def("getMonomerFrom",&O_CommandOligomer_AddMultiMonomerAsLeaf::getMonomerFrom)
	.def("getMonomerAdded",&O_CommandOligomer_AddMultiMonomerAsLeaf::getMonomerAdded)
    ;
//    boost::python::def("create_CommandOligomer_AddMultiMonomerAsLeaf",&create_CommandOligomer_AddMultiMonomerAsLeaf);



    boost::python::class_<O_CommandOligomer_DeleteMonomer,
	boost::shared_ptr<O_CommandOligomer_DeleteMonomer>,
	boost::python::bases <mbb::O_CommandOligomer>,
	boost::noncopyable> ("O_CommandOligomer_DeleteMonomer", boost::python::no_init )
	.add_property("iterate_couplingsToRewire",
			boost::python::range(&O_CommandOligomer_DeleteMonomer::begin_CouplingsToRewire,
				&O_CommandOligomer_DeleteMonomer::end_CouplingsToRewire))
	.def("getMonomerToRewireTo",&O_CommandOligomer_DeleteMonomer::getMonomerToRewireTo)
	.def("getCouplingToDelete",&O_CommandOligomer_DeleteMonomer::getCouplingToDelete)
    ;
//    boost::python::def("create_CommandOligomer_DeleteMonomer",&create_CommandOligomer_DeleteMonomer);

    boost::python::class_<O_CommandOligomer_InsertBeforeMonomer,
	boost::shared_ptr<O_CommandOligomer_InsertBeforeMonomer>,
	boost::python::bases <mbb::O_CommandOligomer>,
	boost::noncopyable> ("O_CommandOligomer_InsertBeforeMonomer", boost::python::no_init )
	.add_property("iterate_MonomersToMove",
			boost::python::range(&O_CommandOligomer_InsertBeforeMonomer::begin_MonomersToMove,
				&O_CommandOligomer_InsertBeforeMonomer::end_MonomersToMove))
	.def("getMonomerNew",&O_CommandOligomer_InsertBeforeMonomer::getMonomerNew)
    ;
//    boost::python::def("create_CommandOligomer_InsertBeforeMonomer",&create_CommandOligomer_InsertBeforeMonomer);



    boost::python::class_<O_CommandOligomer_MutateAllSelectedMonomers,
	boost::shared_ptr<O_CommandOligomer_MutateAllSelectedMonomers>,
	boost::python::bases <mbb::O_CommandOligomer>,
	boost::noncopyable> ("O_CommandOligomer_MutateAllSelectedMonomers", boost::python::no_init )
    ;
//    boost::python::def("create_CommandOligomer_MutateAllSelectedMonomers", &create_CommandOligomer_MutateAllSelectedMonomers);

    boost::python::class_<O_CommandOligomer_ModifyMonomer,
	boost::shared_ptr<O_CommandOligomer_ModifyMonomer>,
	boost::python::bases <mbb::O_CommandOligomer>,
	boost::noncopyable> ("O_CommandOligomer_ModifyMonomer", boost::python::no_init )
    ;
//    boost::python::def("create_CommandOligomer_ModifyMonomer", &create_CommandOligomer_ModifyMonomer);



    boost::python::class_<O_CommandOligomer_MutateAllSelectedCouplings,
	boost::shared_ptr<O_CommandOligomer_MutateAllSelectedCouplings>,
	boost::python::bases <mbb::O_CommandOligomer>,
	boost::noncopyable> ("O_CommandOligomer_MutateAllSelectedCouplings", boost::python::no_init )
    ;
//    boost::python::def("create_CommandOligomer_MutateAllSelectedCouplings", &create_CommandOligomer_MutateAllSelectedCouplings);

    boost::python::class_<O_CommandOligomer_ModifyCoupling,
	boost::shared_ptr<O_CommandOligomer_ModifyCoupling>,
	boost::python::bases <mbb::O_CommandOligomer>,
	boost::noncopyable> ("O_CommandOligomer_ModifyCoupling", boost::python::no_init )
    ;
//    boost::python::def("create_CommandOligomer_ModifyCoupling", &create_CommandOligomer_ModifyCoupling);



}





#endif //]
