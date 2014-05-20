/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * headers.h - handle #includes in source files
 */

void headers( TARGET *t );

#ifdef OPT_HEADER_CACHE_EXT
struct regexp;
LIST *headers1( LIST *l, char *file, int rec, struct regexp *re[] );
#endif
