#undef USEBOOSTPYTHON // currently including regex.h runs into problems with boost python

#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "regex.h"
#include "core/multipleValues.h"
#include "core/wrappers.h"
namespace core
{

// ----------------------------------------------------------------------
//

#define ARGS_af_makeRegex "(regex-str)"
#define DECL_af_makeRegex ""
#define DOCS_af_makeRegex "makeRegex"
    Regex_sp af_makeRegex(const string& str)
    {_G();
        Regex_sp regex = Regex_O::make(str);
        return regex;
    };

    EXPOSE_CLASS(core,Regex_O);

    void Regex_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<Regex_O>()
	    .def("regexMatches",&Regex_O::regexMatches)
	    .def("regexMatch",&Regex_O::regexMatch)
//	    .def("regexSedReplace",&Regex_O::regexSedReplace)   // Need to rethink exposing this function so result is returned
	;
        SYMBOL_EXPORT_SC_(CorePkg,makeRegex);
        Defun(makeRegex);
    }

    void Regex_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Regex,"","",_lisp)
	    .def("regexMatches",&Regex_O::regexMatches)
	    .def("regex-matches",&Regex_O::regexMatches)
	    .def("regex-match",&Regex_O::regexMatch)
//	    .def("regex-sed-replace",&Regex_O::regexSedReplace)
	;
#endif
    }


    Regex_sp Regex_O::make(const string& regex)
    {_G();
	GC_ALLOCATE(Regex_O,re);
	re->_Regex = regex;
	return re;
    }



    
    


#ifdef XML_ARCHIVE
    void Regex_O::archiveBase(core::ArchiveP node)
    {
        this->Base::archiveBase(node);
	// Archive other instance variables here
	IMPLEMENT_ME();
    }
#endif


    void Regex_O::initialize()
    {_OF();
        this->Base::initialize();
    }

    bool Regex_O::regexMatches(const string& str) const
    {_OF();
	return boost::regex_match(str,this->_Regex);
    }
    

    RegexMatch_sp Regex_O::regexMatch(const string& str) const
    {_OF();
	GC_ALLOCATE(RegexMatch_O,match);
        match->_CopyOfTextToMatch = str;
	boost::regex_match(match->_CopyOfTextToMatch.data(),match->_Match,this->_Regex);
	return match;
    }

    string Regex_O::regexSedReplace(const string& str, const string& replace) const
    {_OF();
	return boost::regex_replace(str,this->_Regex,replace,
			     boost::match_default | boost::format_sed);
    }


    





    EXPOSE_CLASS(core,RegexMatch_O);

    void RegexMatch_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<RegexMatch_O>()
	    .def("regex-match-length",&RegexMatch_O::size)
	    .def("regex-match-prefix",&RegexMatch_O::prefix)
	    .def("regex-match-suffix",&RegexMatch_O::suffix)
	    .def("regex-match-part",&RegexMatch_O::part)
	    .def("regex-match-matched",&RegexMatch_O::matched)
	;
    }

    void RegexMatch_O::exposePython(core::Lisp_sp lisp)
    {_G();
#if USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,RegexMatch,"","",_lisp)
	    .def("size",&RegexMatch_O::size)
	    .def("prefix",&RegexMatch_O::prefix)
	    .def("suffix",&RegexMatch_O::suffix)
	    .def("part",&RegexMatch_O::part)
	    .def("matched",&RegexMatch_O::matched)
	;
#endif
    }




    void RegexMatch_O::initialize()
    {_OF();
        this->Base::initialize();
    }

    int RegexMatch_O::size() const
    {_OF();
	return this->_Match.size();
    }

    string RegexMatch_O::part(int idx) const
    {_OF();
	ASSERTF(idx<(int)this->_Match.size(),BF("index[%d] exceeded max[%d]") % idx % this->_Match.size());
	string result = "";
	if ( this->_Match[idx].matched )
	{
	    result.assign(this->_Match[idx].first,this->_Match[idx].second);
//            printf("%s:%d - RegexMatch_O::part %d = [%s]\n", __FILE__,__LINE__,idx,result.c_str());
	}
	return result;
    }
    bool RegexMatch_O::matched(int idx) const
    {_OF();
	ASSERTF(idx<(int)this->_Match.size(),BF("index[%d] exceeded max[%d]") % idx % this->_Match.size());
	return this->_Match[idx].matched;
    }

}; /* core */
