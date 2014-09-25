/*
    File: scorerStage.h
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
