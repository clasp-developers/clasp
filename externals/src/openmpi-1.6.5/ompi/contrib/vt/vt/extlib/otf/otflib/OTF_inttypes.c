/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include "OTF_Platform.h"
#include "OTF_inttypes.h"


#define OTF_COMPILE_TIME_ASSERT(pred) switch(0){case 0:case pred:;}

/* !!! if your compiler hits an error here, your integer types
have not the correct length, thus are not defined correct !!! */
void otf_compile_time_assert(void);
void otf_compile_time_assert() {
	OTF_COMPILE_TIME_ASSERT(sizeof(uint8_t) == 1);
	OTF_COMPILE_TIME_ASSERT(sizeof(int8_t) == 1);
	OTF_COMPILE_TIME_ASSERT(sizeof(uint16_t) == 2);
	OTF_COMPILE_TIME_ASSERT(sizeof(int16_t) == 2);
	OTF_COMPILE_TIME_ASSERT(sizeof(uint32_t) == 4);
	OTF_COMPILE_TIME_ASSERT(sizeof(int32_t) == 4);
	OTF_COMPILE_TIME_ASSERT(sizeof(uint64_t) == 8);
	OTF_COMPILE_TIME_ASSERT(sizeof(int64_t) == 8);
}


/* this function is for all unsigned integers of, 8, 16, 32 or 64 bit length */
uint64_t OTF_Unsigned2Counter( uint64_t value ) {


	return value;
}


uint64_t OTF_Counter2Unsigned( uint64_t value ) {


	return value;
}


/* this function is for all signed integers of, 8, 16, 32 or 64 bit length */
uint64_t OTF_Signed2Counter( int64_t value ) {


	return *( (uint64_t*) &value);
}


int64_t OTF_Counter2Signed( uint64_t value ) {


	return *( (int64_t*) &value);
}


/* this function is for single precision floating point variables 32 bit length */
uint64_t OTF_Float2Counter( float value ) {

	union types_union {
		float    f;
		uint32_t u;
	} t;

	t.f= value;

	return (uint64_t) t.u;
}


float OTF_Counter2Float( uint64_t value ) {

	uint32_t tmp= (uint32_t) value;

	union types_union {
		float    f;
		uint32_t u;
	} t;

	t.u= tmp;

	return t.f;
}


/* this function is for double precision floating point variables 64 bit length */
uint64_t OTF_Double2Counter( double value ) {

	union types_union {
		double   d;
		uint64_t u;
	} t;

	t.d= value;

	return t.u;
}

double OTF_Counter2Double( uint64_t value ) {

	union types_union {
		double   d;
		uint64_t u;
	} t;

	t.u= value;

	return t.d;
}

