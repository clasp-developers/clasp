/*
    File: scoreOperations.h
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
#ifndef	ScoreOperations_H //[
#define ScoreOperations_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "matrix.h"
#include "holder.h"

namespace mbb {

SMART(Alias);
SMART(Builder);
SMART(BuilderState);
SMART(ScorerState);
SMART(RenderDisplayList);
SMART(Scorer);
SMART(ScorerBase);
SMART(PointProvider);
SMART(VdwCollisionRejector);




SMART(ScoreOperation);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreOperation,ScoreOperation,O_Object)
private:
	double	_Scale;
public:
	void	initialize();
	void	oldLispInitialize(RPKeyedArguments kargs, RPLisp);
	void	archiveBase(RPNode node);
public:

	/*! If any of the sub-scorers requires all atoms to be built
	 * (ie VdwCollision checker) then return true
	 */
	virtual bool needsAllAtomsBuilt() { return false;};

	void setScale(double s) { this->_Scale = s;};
	double getScale() { return this->_Scale;};


	virtual double evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder) {_OF();SUBCLASS_MUST_IMPLEMENT();};

	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
	virtual void oligomerChanged(RPPointProvider builder ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};

	/*! Called whenever the sequence of the builder changes
	 */
	virtual void sequenceChanged(RPPointProvider builder ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};

	virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder) {_OF();SUBCLASS_MUST_IMPLEMENT();};

		/*! Insert restraints that emulate the score operations
		 * into the restraint list
		 */
	virtual void insertRestraints(RPPointProvider, RPScorerBase, RPCons& restraints) {_OF();SUBCLASS_MUST_IMPLEMENT();};

__END_CLASS_DEFINITION(O_ScoreOperation)




SMART(ScoreList);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreList,ScoreList,O_ScoreOperation)
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp env);
protected:
	List<O_ScoreOperation>	_Operations;
public:
	/*! If any of the sub-scorers requires all atoms then say yes */
    virtual bool needsAllAtomsBuilt();

	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );

    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
public:
	void	addOperation(RPScoreOperation op);

        double evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder) {_OF();SUBCLASS_MUST_IMPLEMENT();};

	virtual void insertRestraints(RPPointProvider, RPScorerBase, RPCons& restraints) {_OF();SUBCLASS_MUST_IMPLEMENT();};

__END_CLASS_DEFINITION(O_ScoreList) //}






SMART(ScoreSum);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreSum,ScoreSum,O_ScoreList)
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
public:
public:
        double evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);

	/*! Insert a restraint for every operation
	 */
    virtual void insertRestraints(RPPointProvider, RPScorerBase, RPCons& restraints);
__END_CLASS_DEFINITION(O_ScoreSum) //}





SMART(ScoreMin);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreMin,ScoreMin,O_ScoreList)
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
public:
        double evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);

	/*! Insert a restraint for the operation that returns the minimum result
	 */
    virtual void insertRestraints(RPPointProvider, RPScorerBase, RPCons& restraints);


__END_CLASS_DEFINITION(O_ScoreMin) //}







SMART(ScoreSuperposition);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreSuperposition,ScoreSuperposition,O_ScoreOperation) // {
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp );
	void	initialize();
private:
		//! Weight for RestraintAnchors when converting to EnergyFunction
	double	_AnchorWeight;
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);

    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );


    virtual void insertRestraints(RPPointProvider, RPScorerBase, RPCons& restraints);

__END_CLASS_DEFINITION(O_ScoreSuperposition) //}



SMART(ScoreDistance);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreDistance,ScoreDistance,O_ScoreOperation) // {
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
private:
	RPAlias _Alias1;
	RPAlias _Alias2;
	double	_Distance;
	double	_Force;
	string	_Comment;
public:
//    static RPObject prim_distance(RPExecutable e, RPCons exp, RPLisp);
public:
	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
	void	setAlias1(RPAlias nm) {this->_Alias1 = nm;};
	RPAlias getAlias1() { return this->_Alias1;};
	void	setAlias2(RPAlias nm) {this->_Alias2 = nm;};
	RPAlias getAlias2() { return this->_Alias2;};
	void	setDistance(double val) { this->_Distance = val; };
	double	getDistance() { return this->_Distance; };
	void	setForce(double val) { this->_Force = val; };
	double	getForce() { return this->_Force; };
	void	setComment(const string& nm) {this->_Comment = nm;};
	string	getComment() { return this->_Comment;};


__END_CLASS_DEFINITION(O_ScoreDistance) //}



SMART(ScoreAngle);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreAngle,ScoreAngle,O_ScoreOperation) // {
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
private:
	RPAlias _Alias1;
	RPAlias _Alias2;
	RPAlias _Alias3;
	double	_Radians;
	double	_Force;
	string	_Comment;
public:
	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
	void	setAlias1(RPAlias nm) {this->_Alias1 = nm;};
	RPAlias getAlias1() { return this->_Alias1;};
	void	setAlias2(RPAlias nm) {this->_Alias2 = nm;};
	RPAlias getAlias2() { return this->_Alias2;};
	void	setAlias3(RPAlias nm) {this->_Alias3 = nm;};
	RPAlias getAlias3() { return this->_Alias3;};
	void	setRadians(double val) { this->_Radians = val; };
	double	getRadians() { return this->_Radians; };
	void	setForce(double val) { this->_Force = val; };
	double	getForce() { return this->_Force; };
	void	setComment(const string& nm) {this->_Comment = nm;};
	string	getComment() { return this->_Comment;};




__END_CLASS_DEFINITION(O_ScoreAngle) //}


SMART(ScoreDihedral);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreDihedral,ScoreDihedral,O_ScoreOperation) // {
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
private:
	RPAlias _Alias1;
	RPAlias _Alias2;
	RPAlias _Alias3;
	RPAlias _Alias4;
	double	_Radians;
	double	_Force;
	string	_Comment;
public:
	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
	void	setAlias1(RPAlias nm) {this->_Alias1 = nm;};
	RPAlias getAlias1() { return this->_Alias1;};
	void	setAlias2(RPAlias nm) {this->_Alias2 = nm;};
	RPAlias getAlias2() { return this->_Alias2;};
	void	setAlias3(RPAlias nm) {this->_Alias3 = nm;};
	RPAlias getAlias3() { return this->_Alias3;};
	void	setAlias4(RPAlias nm) {this->_Alias4 = nm;};
	RPAlias getAlias4() { return this->_Alias4;};
	void	setRadians(double val) { this->_Radians = val; };
	double	getRadians() { return this->_Radians; };
	void	setForce(double val) { this->_Force = val; };
	double	getForce() { return this->_Force; };
	void	setComment(const string& nm) {this->_Comment = nm;};
	string	getComment() { return this->_Comment;};



__END_CLASS_DEFINITION(O_ScoreDihedral) //}





SMART(ScoreNormalizedDotProduct);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreNormalizedDotProduct,ScoreNormalizedDotProduct,O_ScoreOperation) // {
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
private:
	RPAlias _Alias1;
	RPAlias _Alias2;
	RPAlias _Alias3;
	RPAlias _Alias4;
	double	_NormalizedDotProduct;
	double	_Force;
	string	_Comment;
public:
	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
	void	setAlias1(RPAlias nm) {this->_Alias1 = nm;};
	RPAlias getAlias1() { return this->_Alias1;};
	void	setAlias2(RPAlias nm) {this->_Alias2 = nm;};
	RPAlias getAlias2() { return this->_Alias2;};
	void	setAlias3(RPAlias nm) {this->_Alias3 = nm;};
	RPAlias getAlias3() { return this->_Alias3;};
	void	setAlias4(RPAlias nm) {this->_Alias4 = nm;};
	RPAlias getAlias4() { return this->_Alias4;};
	void	setNormalizedDotProduct(double val) { this->_NormalizedDotProduct = val; };
	double	getNormalizedDotProduct() { return this->_NormalizedDotProduct; };
	void	setForce(double val) { this->_Force = val; };
	double	getForce() { return this->_Force; };
	void	setComment(const string& nm) {this->_Comment = nm;};
	string	getComment() { return this->_Comment;};




__END_CLASS_DEFINITION(O_ScoreNormalizedDotProduct) //}









SMART(ScoreDistanceToPoint);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreDistanceToPoint,ScoreDistanceToPoint,O_ScoreOperation) // {
public:
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
private:
	RPAlias _Alias;
	Vector3	_Point;
	double	_Distance;
	double	_Force;
	string	_Comment;
public:
public:
	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
	void	setAlias(RPAlias nm) {this->_Alias = nm;};
	RPAlias getAlias() { return this->_Alias;};
	void	setDistance(double val) { this->_Distance = val; };
	double	getDistance() { return this->_Distance; };
	Vector3 getPoint() { return this->_Point; };
	void setPoint(const Vector3& p) { this->_Point = p; };
	void	setForce(double val) { this->_Force = val; };
	double	getForce() { return this->_Force; };
	void	setComment(const string& nm) {this->_Comment = nm;};
	string	getComment() { return this->_Comment;};


__END_CLASS_DEFINITION(O_ScoreDistanceToPoint) //}



/*
 */
SMART(ScoreBestGeometry);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreBestGeometry,ScoreBestGeometry,O_ScoreOperation) // {
public:
	void	initialize();
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
private:
	List<O_Alias> _TargetAtomAliases;
	double	_Distance;
	RPAlias	_DistanceAlias;
	double	_AngleRadians;
	RPAlias	_AngleAlias;
	double	_DihedralRadians;
	RPAlias	_DihedralAlias;
	double	_DistanceScale;
	double	_AngleScale;
	double	_DihedralScale;
	string	_Comment;
protected:
	void findBest(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder,
					double& bestScore, RPAlias& bestAlias);
public:
public:
	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );

public:

	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
        void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);

	void	setComment(const string& nm) {this->_Comment = nm;};
	string	getComment() { return this->_Comment;};


__END_CLASS_DEFINITION(O_ScoreBestGeometry) //}



SMART(Rejector);
SMART(ScoreRejects);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreRejects,ScoreRejects,O_ScoreOperation) // {
public:
	void	initialize();
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
protected:
	RPRejector	_Rejector;
	double		_Scale;
public:
public:
    bool needsAllAtomsBuilt();

	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);


    	void	setRejector(RPRejector rejector);
	void	setScale(double scale);


__END_CLASS_DEFINITION(O_ScoreRejects) //}





SMART(ScoreIntraMolecularVdwCollisions);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreIntraMolecularVdwCollisions,ScoreIntraMolecularVdwCollisions,O_ScoreRejects) // {
public:
    static void exposeCando(RPLisp e);
    static void exposePython();
public:
	void	initialize();
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
public:
    static RPScoreIntraMolecularVdwCollisions create(RPLisp e,RPVdwCollisionRejector rej);
public:

	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);

    virtual void insertRestraints(RPPointProvider, RPScorerBase, RPCons& restraints);


__END_CLASS_DEFINITION(O_ScoreIntraMolecularVdwCollisions) //}











SMART(ScoreInterMolecularVdwCollisions);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreInterMolecularVdwCollisions,ScoreInterMolecularVdwCollisions,O_ScoreRejects) // {
public:
    static void exposeCando(RPLisp e);
    static void exposePython();
public:
	void	initialize();
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);
public:
    static RPScoreInterMolecularVdwCollisions create(RPLisp e,RPVdwCollisionRejector rej);
public:

	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder );

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder );
public:
	double	evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);
    virtual void renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder);

    virtual void insertRestraints(RPPointProvider, RPScorerBase, RPCons& restraints);


__END_CLASS_DEFINITION(O_ScoreInterMolecularVdwCollisions) //}




};
#endif //]
