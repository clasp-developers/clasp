#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "myReadLine.h"

#ifdef	READLINE
extern "C" char *readline( const char* prompt);
extern "C" void add_history(char* line);
#endif

namespace core
{

  string myReadLine(const string& prompt)
  {_G();
      string res;
#ifdef	READLINE
      char* line_read;
      /* Get a line from the user. */
//      lisp->print(BF("%s")%prompt);
      stringstream ss;
      ss << std::endl << prompt;
      line_read = ::readline(ss.str().c_str()); // prompt.c_str());
      if ( line_read != NULL )
      {
	  if (*line_read) ::add_history(line_read);
	  res = line_read;
	  free(line_read);
      }
#else
      if ( prompt != "" )
      {
	  _lisp->print(BF("%s ") % prompt );
      }
      getline(cin,res);
#endif
      return res;
  }



};
