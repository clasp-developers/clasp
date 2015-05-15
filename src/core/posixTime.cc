/*
    File: posixTime.cc
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
#undef USEBOOSTPYTHON
#define	DEBUG_LEVEL_FULL

#ifdef _TARGET_OS_LINUX
# include <sys/time.h>
# include <sys/resource.h>
#endif

#include <clasp/core/common.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lisp.h>
#
// last include is wrappers.h
#include <clasp/core/wrappers.h>


namespace core {


    
    
#define ARGS_cl_getInternalRealTime "()"
#define DECL_cl_getInternalRealTime ""
#define DOCS_cl_getInternalRealTime "getInternalRealTime"
    T_sp cl_getInternalRealTime()
    {_G();
        PosixTime_sp now = PosixTime_O::createNow();
        PosixTimeDuration_sp diff = now->sub(gc::As<PosixTime_sp>(_sym_STARstartRunTimeSTAR->symbolValue()));
        return Integer_O::create(diff->totalMilliseconds());
    };



    
    
#define ARGS_cl_getInternalRunTime "()"
#define DECL_cl_getInternalRunTime ""
#define DOCS_cl_getInternalRunTime "getInternalRunTime"
    T_sp cl_getInternalRunTime()
    {_G();
        struct rusage r;
        getrusage(RUSAGE_SELF,&r);
        size_t usec = r.ru_utime.tv_usec;
        size_t sec = r.ru_utime.tv_sec;
        mpz_class bn(sec);
        bn = bn * BRCL_INTERNAL_TIME_UNITS_PER_SECOND;
        bn = bn + usec/(1000000/BRCL_INTERNAL_TIME_UNITS_PER_SECOND);
	return Integer_O::create(bn);
    };



PosixTime_sp PosixTime_O::createNow()
{_G();
    PosixTime_sp now = PosixTime_O::create();
    now->setToLocalTime();
    return now;
}



void	PosixTime_O::initialize()
{
    this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void	PosixTime_O::archiveBase(ArchiveP node)
{
    this->Base::archiveBase(node);
    if ( node->loading() )
    {
	string iso;
	node->attribute("iso",iso);
	this->_Time = boost::posix_time::from_iso_string(iso);
    } else
    {
	string iso;
	iso = boost::posix_time::to_iso_string(this->_Time);
	node->attribute("iso",iso);
    }
}
#endif // defined(XML_ARCHIVE)


PosixTime_sp PosixTime_O::setToLocalTime()
{_G();
    this->_Time = boost::posix_time::microsec_clock::local_time();
    return this->sharedThis<PosixTime_O>();
}

string	PosixTime_O::toSimpleString()
{_G();
    stringstream ss;
    ss << to_simple_string(this->_Time);
    return ss.str();
}



PosixTimeDuration_sp PosixTime_O::sub(PosixTime_sp t)
{_G();
    PosixTimeDuration_sp result = PosixTimeDuration_O::create();
    result->_Duration = this->_Time - t->_Time;
    return result;
}





#if 0
    PosixTimeDuration_sp PosixTimeDuration_O::make(uint hours, uint minutes, uint seconds, uint milliseconds )
    {_G();
	LongLongInt hours = env->lookup(CorePkg,"hours")->object().as<Rational_O>()->as_int();
    LongLongInt minutes = env->lookup(CorePkg,"minutes")->object().as<Rational_O>()->as_int();
    LongLongInt seconds = env->lookup(CorePkg,"seconds")->object().as<Rational_O>()->as_int();
    LongLongInt milliseconds = env->lookup(CorePkg,"milliseconds")->object().as<Rational_O>()->as_int();
    this->_Duration = boost::posix_time::hours(hours)
			+ boost::posix_time::minutes(minutes)
			+ boost::posix_time::seconds(seconds)
			+ boost::posix_time::milliseconds(milliseconds);
    return _Nil<T_O>();
}
#endif
PosixTimeDuration_sp PosixTimeDuration_O::createDurationSince(PosixTime_sp past)
{
    PosixTime_sp now = PosixTime_O::createNow();
    PosixTimeDuration_sp delta = now->sub(past);
    return delta;
}
void	PosixTimeDuration_O::initialize()
{
    this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void	PosixTimeDuration_O::archiveBase(ArchiveP node)
{
    this->Base::archiveBase(node);
    IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)


PosixTimeDuration_sp PosixTimeDuration_O::sub(PosixTimeDuration_sp t)
{_G();
    PosixTimeDuration_sp result = PosixTimeDuration_O::create();
    result->_Duration = this->_Duration - t->_Duration;
    return result;
}


mpz_class PosixTimeDuration_O::totalSeconds()
{_OF();
    return mpz_class(this->_Duration.total_seconds());
}

mpz_class PosixTimeDuration_O::totalMilliseconds()
{_OF();
    stringstream ss;
    ss << this->_Duration.total_milliseconds();
    return mpz_class(ss.str());
}

mpz_class PosixTimeDuration_O::totalMicroseconds()
{_OF();
    stringstream ss;
    ss << this->_Duration.total_microseconds();
    return mpz_class(ss.str());
}

mpz_class PosixTimeDuration_O::fractionalSeconds()
{_OF();
    stringstream ss;
    ss << this->_Duration.fractional_seconds();
    return mpz_class(ss.str());
}

uint PosixTimeDuration_O::seconds()
{_G();
    return this->_Duration.seconds();
}

uint PosixTimeDuration_O::minutes()
{_G();
    return this->_Duration.minutes();
}

uint PosixTimeDuration_O::hours()
{_G();
    return this->_Duration.hours();
}

string PosixTimeDuration_O::toSimpleString()
{_G();
    return boost::posix_time::to_simple_string(this->_Duration);
}

string PosixTimeDuration_O::toIsoString()
{_G();
    return boost::posix_time::to_iso_string(this->_Duration);
}





    void PosixTime_O::exposeCando(Lisp_sp lisp)
{
	class_<PosixTime_O>()
	.def("setToLocalTime",&PosixTime_O::setToLocalTime)
	.def("now",&PosixTime_O::setToLocalTime)
	.def("sub",&PosixTime_O::sub)
	;
        ClDefun(getInternalRealTime);
        ClDefun(getInternalRunTime);

}

    void PosixTime_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,PosixTime,"","",_lisp)
	.def("setToLocalTime",&PosixTime_O::setToLocalTime)
	.def("sub",&PosixTime_O::sub)
    ;
#endif //]
}







    void PosixTimeDuration_O::exposeCando(Lisp_sp lisp)
{
	class_<PosixTimeDuration_O>()
	.def("sub",&PosixTimeDuration_O::sub)
	.def("totalSeconds",&PosixTimeDuration_O::totalSeconds)
	.def("totalMilliseconds",&PosixTimeDuration_O::totalMilliseconds)
	.def("posix-time-duration-seconds",&PosixTimeDuration_O::seconds)
	.def("minutes",&PosixTimeDuration_O::minutes)
	.def("hours",&PosixTimeDuration_O::hours)
	.def("toSimpleString",&PosixTimeDuration_O::toSimpleString)
	.def("toIsoString",&PosixTimeDuration_O::toIsoString)
	;
}

    void PosixTimeDuration_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,PosixTimeDuration,"","",_lisp)
	.def("sub",&PosixTimeDuration_O::sub)
	.def("totalSeconds",&PosixTimeDuration_O::totalSeconds)
	.def("posix_time_duration_seconds",&PosixTimeDuration_O::seconds)
	.def("minutes",&PosixTimeDuration_O::minutes)
	.def("hours",&PosixTimeDuration_O::hours)
	.def("toSimpleString",&PosixTimeDuration_O::toSimpleString)
	.def("toIsoString",&PosixTimeDuration_O::toIsoString)
    ;
#endif //]
}



    EXPOSE_CLASS(core,PosixTime_O);
    EXPOSE_CLASS(core,PosixTimeDuration_O);
};
