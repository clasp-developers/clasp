#ifndef	ScorerStage_H //[
#define ScorerStage_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"


namespace mbb {

SMART(ScorerBase);
SMART(HitList);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScorerStage,ScorerStage,O_Object)
public:
	void	archive(RPNode node);
	void	initialize();
	void	oldLispInitialize(RPKeyedArguments kargs, RPLisp);
private:
	// instance variables
	string		_Name;
	string		_Comment;
	double		_Score;
	RPScorerBase	_Scorer;
	RPHitList	_HitList;
public:
public:

	void setName(const string& nm) { this->_Name = nm; };
	string getName() { return this->_Name;};

	void setComment(const string& nm) { this->_Comment = nm; };
	string getComment() { return this->_Comment; };

	void setScore(double s) { this->_Score = s; };
	double getScore() { return this->_Score; };

	RPHitList getHitList();
	void setHitList(RPHitList hl);

	RPScorerBase getScorer();
	void setScorer(RPScorerBase sc);

	O_ScorerStage( const O_ScorerStage& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_ScorerStage)


};
#endif //]
