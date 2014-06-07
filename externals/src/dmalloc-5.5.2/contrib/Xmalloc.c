/*
 * Provide malloc library replacements for X Window System heap routines
 * 	XtCalloc
 * 	XtFree
 * 	XtMalloc
 * 	XtRealloc
 * so that we can get accurate caller data.
 *
 * David Hill
 */
#define DMALLOC_DISABLE

#include "conf.h"
#include "dmalloc.h"
#include "dmalloc_loc.h"
#include "dmalloc_lp.h"
#include "return.h"

#define DO_XT_ENTRY_POINTS 1
#if DO_XT_ENTRY_POINTS

static	void	_XtAllocError(const char *name)
{
  (void)write(STDERR, "Xt Error: ", 10);
  (void)write(STDERR, name, strlen(name));
  (void)write(STDERR, "\n", 1);
  exit(1);
}

char	*XtMalloc(unsigned size)
{
  char	*file, *ptr;
  
  GET_RET_ADDR(file);
  
  ptr = _malloc_leap(size > 0 ? size : 1, file, 0);
  if (ptr == NULL) {
    _XtAllocError("malloc");
  }
  
  return ptr;
}

void	XtFree(char *pnt)
{
  char	*file;
  
  GET_RET_ADDR(file);
  
  if (ptr != NULL) {
    _free_leap(pnt, file, 0);
  }
}

char	*XtCalloc(unsigned num_elements, unsigned size)
{
  char	*file;
  
  GET_RET_ADDR(file);
  
  ptr = _calloc_leap(num_elements, size ? size : 1, file, 0);
  if (ptr == NULL) {
    _XtAllocError("calloc");
  }
  
  return ptr;
}

/*
 * resizes OLD_PNT to SIZE bytes and return the new space after either copying
 * all of OLD_PNT to the new area or truncating.  returns 0L on error.
 */
char	*XtRealloc(char *ptr, unsigned size)
{
  char	*file;
  
  GET_RET_ADDR(file);
  
  ptr = _realloc_leap(ptr, size > 0 ? size : 1, file, 0);
  if (ptr == NULL) {
    _XtAllocError("realloc");
  }
  
  return ptr;
}

#endif /* DO_XT_ENTRY_POINTS */
