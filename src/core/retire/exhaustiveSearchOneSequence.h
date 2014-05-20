       
       
#ifndef	ExhaustiveSearchOneSequence_H //[
#define ExhaustiveSearchOneSequence_H



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
SMART(ExhaustiveSearchOneSequence);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ExhaustiveSearchOneSequence,ExhaustiveSearchOneSequence,O_Search) // {
public:
	void	archive(RPNode node);
	void	initialize();
	void	oldLispInitialize(RPKeyedArguments kargs, RPLisp);
private:
		//! If we split the search over multiple processes set the num processes here
	uint	_NumberOfPartitions;
		//! Set the partition number here, 0 is first
	int	_PartitionNumber;
		//! If this is zero then generate ALL conformations
	int	_NumberOfConformationsPerSequence;
		//! If !UseRandomConformations then go through them systematically
	bool	_UseRandomConformations;
	RPScorerStatistics _Statistics;
	LongLongInt	_ConformationsBuilt;
public:
public:
	void setDefaultOptions();
	void setKeyedOptions(RPKeyedArguments options);
	void setOptions(RPCons options);

	virtual void run();

	O_ExhaustiveSearchOneSequence( const O_ExhaustiveSearchOneSequence& ss ); //!< Copy constructor


__END_CLASS_DEFINITION(O_ExhaustiveSearchOneSequence) //}




};
#endif //]
