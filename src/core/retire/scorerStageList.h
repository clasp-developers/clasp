#ifndef	ScorerStageList_H //[
#define ScorerStageList_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "holder.h"

namespace mbb {


SMART(ScorerStage);


__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScorerStageList,ScorerStageList,O_Object)
public:
	void	archive(RPNode node);
	void	initialize();
	void	oldLispInitialize(RPKeyedArguments kargs, RPLisp);

private:
	// instance variables
	List<O_ScorerStage>	_Stages;
public:
public:

	RPCons entries();

	void addStage(RPScorerStage stage);

	RPScorerStage getScorerStageWithName(const string& name);

	O_ScorerStageList( const O_ScorerStageList& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_ScorerStageList)


};
#endif //]
