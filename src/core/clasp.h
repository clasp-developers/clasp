#ifndef _core_brcl_H
#define _core_brcl_H


#include "foundation.h"
#include "object.h"
#include "cons.h"
#include "lisp.h"



struct brcl_State
{
    int x;
};

#define	BRCL_NOREF 0
#define	brcl_NOREF 0
#define BRCL_REGISTRYINDEX 0

int brclL_ref(brcl_State*, int);
void brcl_rawgeti(brcl_State*, int, int );
void brcl_rawseti(brcl_State*, int, int );
void brclL_unref(brcl_State*, int, int );
void* brcl_touserdata(brcl_State*, int );
void brcl_pushvalue(brcl_State*, int);
void brcl_pushnil(brcl_State*);
bool brcl_isnil(brcl_State*,int);
void brcl_pop(brcl_State*,int);
void brcl_pushliteral(brcl_State*,const char*);
void brcl_rawget(brcl_State*,int);
int brcl_gettop(brcl_State*);
void brcl_pushnumber(brcl_State*,int);
double brcl_tonumber(brcl_State*,int);
bool brcl_isnumber(brcl_State*,int);

#endif
