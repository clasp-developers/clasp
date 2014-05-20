#include <stddef.h>
#include "jam.h"
#include "regexp.h"
#include "hash.h"

#include "newstr.h"
#include "lists.h"
#include "parse.h"
#include "compile.h"
#include "frames.h"

struct regex_entry
{
    const char* pattern;
    regexp* regex;
};
typedef struct regex_entry regex_entry;

static struct hash* regex_hash;

regexp* regex_compile( const char* pattern )
{
    regex_entry entry, *e = &entry;
    entry.pattern = pattern;

    if ( !regex_hash )
        regex_hash = hashinit(sizeof(regex_entry), "regex");

    if ( hashenter( regex_hash, (HASHDATA **)&e ) )
        e->regex = regcomp( (char*)pattern );

    return e->regex;
}

LIST*
builtin_subst(
    PARSE    *parse,
    FRAME      *frame )
{
  LIST* result = L0;
  LIST* arg1 = lol_get( frame->args, 0 );

  if ( arg1 && list_next(arg1) && list_next(list_next(arg1)) )
  {

      const char* source = arg1->string;
      const char* pattern = list_next(arg1)->string;
      regexp* repat = regex_compile( pattern );

      if ( regexec( repat, (char*)source) )
      {
          LIST* subst = list_next(arg1);

          while ((subst = list_next(subst)) != L0)
          {
# define BUFLEN 4096
              char buf[BUFLEN + 1];
              const char* in = subst->string;
              char* out = buf;

              for ( in = subst->string; *in && out < buf + BUFLEN; ++in )
              {
                  if ( *in == '\\' || *in == '$' )
                  {
                      ++in;
                      if ( *in == 0 )
                      {
                          break;
                      }
                      else if ( *in >= '0' && *in <= '9' )
                      {
                          unsigned n = *in - '0';
                          const size_t srclen = repat->endp[n] - repat->startp[n];
                          const size_t remaining = buf + BUFLEN - out;
                          const size_t len = srclen < remaining ? srclen : remaining;
                          memcpy( out, repat->startp[n], len );
                          out += len;
                          continue;
                      }
                      /* fall through and copy the next character */
                  }
                  *out++ = *in;
              }
              *out = 0;

              result = list_new( result, newstr( buf ) );
#undef BUFLEN
          }
      }
  }

  return result;
}

