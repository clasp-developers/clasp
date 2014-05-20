#ifndef	_core_Regex_H
#define _core_Regex_H

#include "core/useBoostRegex.h"

#include "core/object.h"
#include "corePackage.fwd.h"


namespace core
{






FORWARD(RegexMatch);
    class RegexMatch_O : public core::T_O
{
    friend class Regex_O;
    LISP_BASE1(core::T_O);
    LISP_CLASS(core,CorePkg,RegexMatch_O,"RegexMatch");
    DEFAULT_CTOR_DTOR(RegexMatch_O);
public:
	void initialize();

private: // instance variables here
    string              _CopyOfTextToMatch; // _Match only has meaning as long as string exists
    boost::cmatch	_Match;

public: // Functions here

	/*! Return the number of matches */
	int size() const;
	/*! Return the captures that matched
	  - index 0 is the entire string and the subsequent
	  indices are the individual captures */
	string part(int idx) const;
	
	/*! Return true if the part was part of the match */
	bool matched(int idx) const;

	/*! Return the prefix of the match */
	string prefix() const { return this->_Match.prefix(); };

	/*! Return the suffix of the match */
	string suffix() const { return this->_Match.suffix(); };
};

FORWARD(Regex);
    class Regex_O : public core::T_O
{
    LISP_BASE1(core::T_O);
    LISP_CLASS(core,CorePkg,Regex_O,"Regex");
//    DECLARE_ARCHIVE();
    DEFAULT_CTOR_DTOR(Regex_O);
public:
	void initialize();

private: // instance variables here
	boost::regex	_Regex;

public:
    static Regex_sp make(const string& str);

public: // Functions here
	bool regexMatches(const string& str) const;

	
	RegexMatch_sp regexMatch(const string& str) const;

	/*! Return a new string that is replaced using "sed"-like syntax */
	string regexSedReplace(const string& str, const string& replace) const;
};




}; /* core */

TRANSLATE(core::Regex_O);
TRANSLATE(core::RegexMatch_O);

#endif /* _core_Regex_H */


