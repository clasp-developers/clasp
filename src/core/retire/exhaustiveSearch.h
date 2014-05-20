       
       
#ifndef	ExhaustiveSearch_H //[
#define ExhaustiveSearch_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "search.h"
#include "numerics.h"


namespace mbb {



SMART(ScorerStatistics);
SMART(ExhaustiveSearch);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ExhaustiveSearch,ExhaustiveSearch,O_Search) // {
public:
	void	archive(RPNode node);
	void	initialize();
private:
		//! If we split the search over multiple processes set the num processes here
	int	_NumberOfProcesses;
		//! Set the process number here, 0 is first
	int	_ProcessNumber;
		//! If this is zero then generate ALL conformations
	int	_NumberOfConformationsPerSequence;
		//! If !UseRandomConformations then go through them systematically
	bool	_UseRandomConformations;
public:
    static RPObject prim_exhaustiveSearch(RPExecutable e, RPCons exp, RPEnvironment environ, RPLisp);
    static RPObject prim_exhaustiveSearchOneSequence(RPExecutable e, RPCons exp, RPEnvironment environ, RPLisp);
    static RPObject prim_exhaustiveSearchConformationsOfCurrentSequence(RPExecutable e, RPCons exp, RPEnvironment environ, RPLisp);
public:

	void setDefaultOptions();
	void setKeyedOptions(RPKeyedArguments options);
	void setOptions(RPCons options);

//	virtual void oldrun();
	virtual void run();
	virtual LongLongInt conformationalSearchWithMultipleHits(
				RPScorerStatistics statistics
			);
	virtual LongLongInt searchCurrentSequence(
				RPScorerStatistics statistics
			);

	O_ExhaustiveSearch( const O_ExhaustiveSearch& ss ); //!< Copy constructor


__END_CLASS_DEFINITION(O_ExhaustiveSearch) //}


};
#endif //]
