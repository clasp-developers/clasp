#ifndef	PosixTime_H //[
#define PosixTime_H


#include "clasp_gmpxx.h"
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#undef tolower
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>
#include "foundation.h"
#include "numerics.h"
#include "object.h"


namespace core {

SMART(PosixTime);
SMART(PosixTimeDuration);


SMART(PosixTime );
class PosixTime_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,PosixTime_O,"PosixTime");
    DECLARE_INIT();
public: // virtual functions inherited from Object
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
//	string	__repr__() const;

private: // instance variables
	boost::posix_time::ptime	_Time;

public:	// Creation class functions
	static PosixTime_sp createNow();
public:

	PosixTime_sp setToLocalTime();

	PosixTimeDuration_sp sub(PosixTime_sp);

	string toSimpleString();
	string toIsoString();

//	PosixTime_O( const PosixTime_O& ss ); //!< Copy constructor

	DEFAULT_CTOR_DTOR(PosixTime_O);
};


SMART(PosixTimeDuration );
class PosixTimeDuration_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,PosixTimeDuration_O,"PosixTimeDuration");
    DECLARE_INIT();
friend class PosixTime_O;
public: // virtual functions inherited from Object
	void	initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
//	string	__repr__() const;

public:
	static PosixTimeDuration_sp	createDurationSince(PosixTime_sp past);
private: // instance variables
	boost::posix_time::time_duration	_Duration;

public:	// Creation class functions

public:

	PosixTimeDuration_sp sub(PosixTimeDuration_sp);
	mpz_class totalSeconds();
	mpz_class totalMilliseconds();
	mpz_class totalMicroseconds();
	mpz_class fractionalSeconds();

	uint seconds();
	uint minutes();
	uint hours();
	string toSimpleString();
	string toIsoString();

	DEFAULT_CTOR_DTOR(PosixTimeDuration_O);
};


};
TRANSLATE(core::PosixTime_O);
TRANSLATE(core::PosixTimeDuration_O);
#endif //]
