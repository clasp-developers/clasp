/*
    File: commandOligomer.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
       
       
#ifndef	CommandOligomer_H //[
#define CommandOligomer_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "command.h"
#include "vector2.h"
namespace mbb {

SMART(BuilderDatabase);
SMART(BuilderDatabaseReference);
SMART(Oligomer);
SMART(MultiMonomer);
SMART(Monomer);
SMART(Coupling);
SMART(DirectionalCoupling);
SMART(StringList);

SMART(CommandOligomer);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_CommandOligomer,CommandOligomer,O_Command) // {
public:
	void	archiveBase(RPNode node);
protected:
	RPOligomer			_Oligomer;
//	RPBuilderDatabaseReference	_BuilderDatabaseRef;
public:

	void setup(RPOligomer olig, RPBuilderDatabase cdb );

	virtual void	doIt();
	virtual void	undoIt();

	O_CommandOligomer( const O_CommandOligomer& ss ); //!< Copy constructor


__END_CLASS_DEFINITION(O_CommandOligomer) //}


SMART(CommandOligomer_AddMultiMonomerAsLeaf);
SMART(CommandOligomer_AddMultiMonomerAsLeaf);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_CommandOligomer_AddMultiMonomerAsLeaf,CommandOligomer_AddMultiMonomerAsLeaf,O_CommandOligomer) // {
public:
	void	archive(RPNode node);
private:
	RPMonomer		_MonomerFrom;
	string			_CouplingName;
	string			_GroupName;
	Vector2			_Pos2D;
	RPMonomer		_MonomerAdded;
public:
	void setup( RPOligomer olig, RPBuilderDatabase cdb,
		RPMonomer monomerFrom, const string& couplingName,
		const string& groupName, Vector2 pos2D );

	virtual void doIt();
	virtual void undoIt();

	RPMonomer	getMonomerFrom() {_OF(); ASSERT_NOT_NULL(this->_MonomerFrom);return this->_MonomerFrom;};
	RPMonomer	getMonomerAdded() {_OF(); ASSERT_NOT_NULL(this->_MonomerAdded);return this->_MonomerAdded;};
	O_CommandOligomer_AddMultiMonomerAsLeaf( const O_CommandOligomer_AddMultiMonomerAsLeaf& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_CommandOligomer_AddMultiMonomerAsLeaf) //}



// Constructor function

inline	RPCommandOligomer_AddMultiMonomerAsLeaf create_CommandOligomer_AddMultiMonomerAsLeaf(
		RPLisp e,
		RPOligomer olig, RPBuilderDatabase cdb,
		RPMonomer monomerFrom, const string& couplingName,
		const string& groupName, Vector2 pos2D )
{
    RPCommandOligomer_AddMultiMonomerAsLeaf	obj;
    obj = RP_Create<O_CommandOligomer_AddMultiMonomerAsLeaf>(e);
    obj->setup(olig,cdb,monomerFrom,couplingName,groupName,pos2D);
    return obj;
}






SMART(CommandOligomer_DeleteMonomer);
SMART(CommandOligomer_DeleteMonomer);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_CommandOligomer_DeleteMonomer,CommandOligomer_DeleteMonomer,O_CommandOligomer) // {
public:
	void	archive(RPNode node);
private:
	RPMonomer	_MonomerToDelete;
	RPDirectionalCoupling	_CouplingToDelete;
	List<O_Coupling>	_CouplingsToRewire;
	RPMonomer	_MonomerToRewireTo;
public:
	List<O_Coupling>::iterator	begin_CouplingsToRewire() { return this->_CouplingsToRewire.begin();};
	List<O_Coupling>::iterator	end_CouplingsToRewire() { return this->_CouplingsToRewire.end();};

	RPMonomer	getMonomerToRewireTo() {_OF(); ASSERT_NOT_NULL(this->_MonomerToRewireTo);return this->_MonomerToRewireTo;};
	RPDirectionalCoupling	getCouplingToDelete() {_OF(); ASSERT_NOT_NULL(this->_CouplingToDelete);return this->_CouplingToDelete;};
	void setup( RPOligomer olig, RPBuilderDatabase cdb, RPMonomer monomerToDelete );

	virtual void doIt();
	virtual void undoIt();

	O_CommandOligomer_DeleteMonomer( const O_CommandOligomer_DeleteMonomer& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_CommandOligomer_DeleteMonomer) //}



// Constructor function

inline	RPCommandOligomer_DeleteMonomer create_CommandOligomer_DeleteMonomer(
		RPLisp e,
		RPOligomer olig,
		RPBuilderDatabase	cdb,
		RPMonomer monomerToDelete )
{
    RPCommandOligomer_DeleteMonomer	obj;
    obj = RP_Create<O_CommandOligomer_DeleteMonomer>(e);
    obj->setup(olig, cdb, monomerToDelete);
    return obj;
}




SMART(CommandOligomer_InsertBeforeMonomer);
SMART(CommandOligomer_InsertBeforeMonomer);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_CommandOligomer_InsertBeforeMonomer,CommandOligomer_InsertBeforeMonomer,O_CommandOligomer) // {
public:
	void	archive(RPNode node);
private:
	RPMonomer	_MonomerToInsertBefore;
	List<O_Monomer>	_MonomersToMove;
	RPDirectionalCoupling	_CouplingToInsertAfter;
	RPMonomer	_MonomerNew;
	RPDirectionalCoupling	_CouplingToInsertBeforeNew;
	Vector2		_Offset;
public:
	List<O_Monomer>::iterator	begin_MonomersToMove() { return this->_MonomersToMove.begin();};
	List<O_Monomer>::iterator	end_MonomersToMove() { return this->_MonomersToMove.end();};

	RPMonomer	getMonomerNew() { return this->_MonomerNew;};
	void setup( RPOligomer olig,
				RPBuilderDatabase cdb,
				RPMonomer beforeMon,
				const string& groupName,
				const string& couplingName);

	virtual void doIt();
	virtual void undoIt();

	O_CommandOligomer_InsertBeforeMonomer( const O_CommandOligomer_InsertBeforeMonomer& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_CommandOligomer_InsertBeforeMonomer) //}



// Constructor function

inline	RPCommandOligomer_InsertBeforeMonomer create_CommandOligomer_InsertBeforeMonomer(
				RPLisp e,
				RPOligomer olig,
				RPBuilderDatabase cdb,
				RPMonomer beforeMon,
				const string& groupName,
				const string& couplingName)
{
    RPCommandOligomer_InsertBeforeMonomer	obj;
    obj = RP_Create<O_CommandOligomer_InsertBeforeMonomer>(e);
    obj->setup( olig, cdb, beforeMon, groupName, couplingName );
    return obj;
}







SMART(CommandOligomer_MutateAllSelectedMonomers);
SMART(CommandOligomer_MutateAllSelectedMonomers);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_CommandOligomer_MutateAllSelectedMonomers,CommandOligomer_MutateAllSelectedMonomers,O_CommandOligomer) // {
public:
	void initialize();
public:
	void	archive(RPNode node);
private:
	string		_NewGroupName;
	List<O_Monomer>	_MonomersToMutate;
	RPStringList	_OldGroupNames;
public:
	void setup( RPOligomer olig, RPBuilderDatabase cdb, const string& newGroupName );

	virtual void doIt();
	virtual void undoIt();

	O_CommandOligomer_MutateAllSelectedMonomers( const O_CommandOligomer_MutateAllSelectedMonomers& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_CommandOligomer_MutateAllSelectedMonomers) //}



// Constructor function

inline	RPCommandOligomer_MutateAllSelectedMonomers create_CommandOligomer_MutateAllSelectedMonomers(
				RPLisp e,
				RPOligomer olig,
				RPBuilderDatabase cdb,
				const string& groupName )
{
    RPCommandOligomer_MutateAllSelectedMonomers	obj;
    obj = RP_Create<O_CommandOligomer_MutateAllSelectedMonomers>(e);
    obj->setup( olig, cdb, groupName );
    return obj;
}








SMART(CommandOligomer_ModifyMonomer);
SMART(CommandOligomer_ModifyMonomer);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_CommandOligomer_ModifyMonomer,CommandOligomer_ModifyMonomer,O_CommandOligomer) // {
public:
	void	archive(RPNode node);
private:
	RPMonomer	_Monomer;
	string		_NewGroupName;
	string		_NewAliases;
	string		_NewComment;
	string		_OldGroupName;
	string		_OldAliases;
	string		_OldComment;
public:
	void setup( RPOligomer olig, RPBuilderDatabase cdb, RPMonomer mon, const string& newGroupName, const string& newAlias, const string& newComment );

	virtual void doIt();
	virtual void undoIt();

	O_CommandOligomer_ModifyMonomer( const O_CommandOligomer_ModifyMonomer& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_CommandOligomer_ModifyMonomer) //}



// Constructor function

inline	RPCommandOligomer_ModifyMonomer create_CommandOligomer_ModifyMonomer(
				RPLisp e,
				RPOligomer olig,
				RPBuilderDatabase cdb,
				RPMonomer mon,
				const string& newGroupName,
				const string& newAlias,
				const string& newComment )
{
    RPCommandOligomer_ModifyMonomer	obj;
    obj = RP_Create<O_CommandOligomer_ModifyMonomer>(e);
    obj->setup( olig, cdb, mon, newGroupName, newAlias, newComment );
    return obj;
}









SMART(CommandOligomer_MutateAllSelectedCouplings);
SMART(CommandOligomer_MutateAllSelectedCouplings);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_CommandOligomer_MutateAllSelectedCouplings,CommandOligomer_MutateAllSelectedCouplings,O_CommandOligomer) // {
public:
	void initialize();
public:
	void	archive(RPNode node);
private:
	string			_NewCouplingName;
	List<O_DirectionalCoupling>	_CouplingsToMutate;
	RPStringList		_OldCouplingNames;
public:
	void setup( RPOligomer olig, RPBuilderDatabase cdb, const string& newCouplingName );

	virtual void doIt();
	virtual void undoIt();

	O_CommandOligomer_MutateAllSelectedCouplings( const O_CommandOligomer_MutateAllSelectedCouplings& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_CommandOligomer_MutateAllSelectedCouplings) //}



// Constructor function

inline	RPCommandOligomer_MutateAllSelectedCouplings create_CommandOligomer_MutateAllSelectedCouplings(
				RPLisp e,
				RPOligomer olig,
				RPBuilderDatabase cdb,
				const string& couplingName )
{
    RPCommandOligomer_MutateAllSelectedCouplings	obj;
    obj = RP_Create<O_CommandOligomer_MutateAllSelectedCouplings>(e);
    obj->setup( olig, cdb, couplingName );
    return obj;
}








SMART(CommandOligomer_ModifyCoupling);
SMART(CommandOligomer_ModifyCoupling);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_CommandOligomer_ModifyCoupling,CommandOligomer_ModifyCoupling,O_CommandOligomer) // {
public:
	void	archive(RPNode node);
private:
	RPDirectionalCoupling	_Coupling;
	string		_NewCouplingName;
	string		_OldCouplingName;
public:
	void setup( RPOligomer olig, RPBuilderDatabase cdb, RPDirectionalCoupling coup, const string& newCouplingName );

	virtual void doIt();
	virtual void undoIt();

	O_CommandOligomer_ModifyCoupling( const O_CommandOligomer_ModifyCoupling& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_CommandOligomer_ModifyCoupling) //}



// Constructor function

inline	RPCommandOligomer_ModifyCoupling create_CommandOligomer_ModifyCoupling(
				RPLisp e,
				RPOligomer olig,
				RPBuilderDatabase cdb,
				RPDirectionalCoupling coup,
				const string& newCouplingName )
{
    RPCommandOligomer_ModifyCoupling	obj;
    obj = RP_Create<O_CommandOligomer_ModifyCoupling>(e);
    obj->setup( olig, cdb, coup, newCouplingName );
    return obj;
}












};
#endif //]
