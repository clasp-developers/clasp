#ifndef LEX_DEBUG_H
#define LEX_DEBUG_H

#if LEXDEBUG == 1
#define	LEXPRINT(l,x) { lispLog(l,BF("Token: %s") % x );}
#define	LEXDPRINT(l,x) { lisp_LOG(l,BF("%s") % (x));}
#else
#define	LEXPRINT(x)
#define	LEXDPRINT(x)
#endif


#endif
