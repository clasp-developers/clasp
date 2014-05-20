/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    char_ctype.d -- Character properties.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef ECL_UNICODE
#include <ctype.h>

bool
ecl_graphic_char_p(ecl_character code)
{
	return code == ' ' || isgraph(code);
}

bool
ecl_alpha_char_p(ecl_character code)
{
	return isalpha(code);
}

bool
ecl_upper_case_p(ecl_character code)
{
	return isupper(code);
}

bool
ecl_lower_case_p(ecl_character code)
{
	return islower(code);
}

bool
ecl_both_case_p(ecl_character code)
{
	return islower(code) || isupper(code);
}

bool
ecl_alphanumericp(ecl_character i)
{
	return isalnum(i);
}

ecl_character
ecl_char_upcase(ecl_character code)
{
	return toupper(code);
}

ecl_character
ecl_char_downcase(ecl_character code)
{
	return tolower(code);
}

#else /* ECL_UNICODE */

extern const unsigned char ecl_ucd_misc_table[];
extern const unsigned char *ecl_ucd_page_table[];
extern const unsigned char ecl_ucd_page_table_1[];

/*
 * 21-bits Unicode (0 to #x110000 char codes)
 */

#if ECL_UNICODE > 16
const unsigned char *
ucd_char_data(ecl_character code)
{
	const unsigned char *page = ecl_ucd_page_table[code >> 8];
        return page + (4 * (code & 0xFF));
}

static cl_index
ucd_value_0(ecl_character code)
{
	return ucd_char_data(code)[0];
}

#define read_case_bytes(c) (c[1] + (c[2] << 8) + (c[3] << 16))
#endif

/*
 * 16-bits Unicode (0 to #x110000 char codes)
 * Each character occupies 3 bytes
 */

#if ECL_UNICODE <= 16
const unsigned char *
ucd_char_data(ecl_character code)
{
	const unsigned char *page = ecl_ucd_page_table[code >> 8];
        return page + (3 * (code & 0xFF));
}

static cl_index
ucd_value_0(ecl_character code)
{
	return ucd_char_data(code)[0];
}

#define read_case_bytes(c) (c[1] + (c[2] << 8))
#endif

static int
ucd_general_category(ecl_character code)
{
	return ecl_ucd_misc_table[8 * ucd_value_0(code)];
}

static int
ucd_decimal_digit(ecl_character code)
{
	return ecl_ucd_misc_table[3 + 8 * ucd_value_0(code)];
}

bool
ecl_graphic_char_p(ecl_character code)
{
	/* compatible to SBCL */
	return code > 159 || ((31 < code) && (code < 127));
}

bool
ecl_alpha_char_p(ecl_character code)
{
	return ucd_general_category(code) < 5;
}

bool
ecl_upper_case_p(ecl_character code)
{
	return ucd_value_0(code) == 0;
}

bool
ecl_lower_case_p(ecl_character code)
{
	return ucd_value_0(code) == 1;
}

bool
ecl_both_case_p(ecl_character code)
{
	return ucd_value_0(code) < 2;
}

bool
ecl_alphanumericp(ecl_character i)
{
	int gc = ucd_general_category(i);
	return (gc < 5) || (gc == 12);
}

ecl_character
ecl_char_upcase(ecl_character code)
{
	const unsigned char *c = ucd_char_data(code);
	if (c[0] == 1) {
		return read_case_bytes(c);
	} else {
		return code;
	}
}

ecl_character
ecl_char_downcase(ecl_character code)
{
	const unsigned char *c = ucd_char_data(code);
	if (c[0] == 0) {
		return read_case_bytes(c);
	} else {
		return code;
	}
}
#endif
