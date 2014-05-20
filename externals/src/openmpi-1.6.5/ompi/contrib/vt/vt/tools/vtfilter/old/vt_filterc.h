/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_FILTERC_H_
#define _VT_FILTERC_H_

#include <set>
#include <map>
#include <stack>
#include <string>
#include <vector>
#include <algorithm>

#include "vt_inttypes.h"

#ifdef VT_MPI
#	include "vt_defs.h" // to get VT_MPI_INT
#	include "mpi.h"
#endif // VT_MPI

struct Function {

	uint32_t id;
	
	std::string name;
	
	std::set<uint32_t> subFuncs;
	
	uint64_t invocations;
	
	/* maximum depth of functions under itself */
	uint32_t depth;

	/* accumulated duration time in ticks
	divided by the number of invocations it is the average duration */
	int64_t accDurationExcl;
	int64_t accDurationIncl;
	

	Function() :
		id(0), invocations(0), depth(0), accDurationExcl(0), accDurationIncl(0) {}

	Function( uint32_t _id, const std::string& nm ) :
		id(_id), name(nm), invocations(0), depth(0), accDurationExcl(0), accDurationIncl(0) {}

	bool operator<( const Function& func ) const {

		/* order function by depth, subfunction count and invocation count */
		if( depth != func.depth ) return depth < func.depth;
		else if( subFuncs.size() != func.subFuncs.size() ) return subFuncs.size() < func.subFuncs.size();
		else return invocations > func.invocations;
	}

	void operator+=( const Function& func ) {
		invocations += func.invocations;
		if( depth < func.depth ) depth = func.depth;
		accDurationExcl += func.accDurationExcl;
		accDurationIncl += func.accDurationIncl;

		std::set<uint32_t>::const_iterator itsf;

		for( itsf = func.subFuncs.begin(); itsf != func.subFuncs.end(); ++itsf ) {
			if( subFuncs.find( *itsf ) == subFuncs.end() ) subFuncs.insert( *itsf );
		}
	}
	
#ifdef VT_MPI
	VT_MPI_INT getPackSize( void );
	void packBuffer( char*& buffer, const VT_MPI_INT& buffersize, VT_MPI_INT& bufferpos );
	void unpackBuffer( char*& buffer, const VT_MPI_INT& buffersize, VT_MPI_INT& bufferpos );
#endif // VT_MPI
};



struct StackItem {

	StackItem( std::map<uint32_t, Function>::iterator _it ) : it( _it ) {}

	std::map<uint32_t, Function>::iterator it;
};


struct PostStackItem {

	PostStackItem( uint32_t _id, const std::set<uint32_t>& vs ) :
		id( _id ), visited( vs ) { visited.insert( _id ); }

	uint32_t id;//

	std::set<uint32_t> visited;
};


class Filter {

public:

	Filter();

	void setTimerResolution( uint64_t tickspersecond );
	void addFunction( uint32_t func, const std::string& name );
	void addEnter( uint32_t func, uint32_t process, uint64_t time );
	void addLeave( uint32_t process, uint64_t time );

	void incrMessageCount()
		{ ++messageCount; }
	void incrCollectiveCount()
		{ ++collectiveCount; }


	/* calculates 'maxStackDepth', 'totalInvocations', 'maxInvocations'
	 * calculates the 'depth' of every function
	 */
	void postProcessing();



	/* returns a set of functions ordered by their importance for filtering
	 * 1. stackdepth 2. subfunction count 3. invocation count
	 */
	std::vector<Function> getFunctions() const;


	/* Reduces the count of events to 'percent' percent.
	 * It does not filter functions included in 'excludesymbols'.
	 * Returns a set of function tokens, which have been filtered.
	 */
	std::set<uint32_t> reduceTo( float* percent,
	                             const std::set<uint32_t>& excludesymbols,
	                             const std::set<uint32_t>& includesymbols,
	                             bool includechildren,
	                             uint64_t limit );


	uint32_t getMaxStackDepth() const { return maxStackDepth; }
	uint64_t getTotalInvocations() const { return totalInvocations; }
	uint64_t getMaxInvocations() const { return maxInvocations; }
	uint64_t getTimerResolution() const { return timerResolution; }
	uint64_t getMessageCount() const { return messageCount; }
	uint64_t getCollectiveCount() const { return collectiveCount; }

	void operator+=( const Filter& filter );

	const std::map<uint32_t, Function>& getFunctionMap() const { return functions; }

#ifdef VT_MPI
	VT_MPI_INT getPackSize( void );
	void packBuffer( char*& buffer, const VT_MPI_INT& buffersize, VT_MPI_INT& bufferpos );
	void unpackBuffer( char*& buffer, const VT_MPI_INT& buffersize, VT_MPI_INT& bufferpos );
#endif // VT_MPI


protected:

	/* visites a function and its children (recursively) to gather information
	about the stackdepth.
	If killed is not NULL it adds the visited functions to the set and
	counts the killed invocations */
	uint32_t visitFunction( std::stack<PostStackItem>& stck,
		std::set<uint32_t>* killed, uint64_t* killedinvocations,
		const std::set<uint32_t>* nokill );

	/* visites a function and its children (recursively) in order to
	add the parents of nokill-functions to the nokill-set as well */
	void visitFunctionExclude( std::stack<PostStackItem>& stck,
		std::set<uint32_t>& nokill );

protected:
	
	uint32_t maxStackDepth;
	uint64_t totalInvocations;
	uint64_t maxInvocations;
	uint64_t timerResolution;

	uint64_t messageCount;
	uint64_t collectiveCount;

	/* all functions */
	std::map<uint32_t /*token*/, Function> functions;

	std::map<uint32_t /*procid*/, std::stack< StackItem > > callStack;
};


#endif /* _VT_FILTERC_H_ */

