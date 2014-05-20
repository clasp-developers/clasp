/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * search.h - find a target along $(SEARCH) or $(LOCATE)
 */

char *search( char *target, time_t *time, char **another_target, int file );
