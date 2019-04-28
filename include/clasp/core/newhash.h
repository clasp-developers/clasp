/*
    File: newhash.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
/* -*- mode: c; c-basic-offset: 8 -*- */

#ifndef newhash_H
#define newhash_H
/********************
 * HASHING ROUTINES *
 ********************/

/*
 * SBCL's newest algorithm. Leads to few collisions, and it is faster.
 */

#if INTPTR_BITS == 64
/*
 * 64 bit version
 */
#define GOLDEN_RATIO 0x9e3779b97f4a7c13L
#define hash_mix(a, b, c)   \
  {                    \
    a = a - b;         \
    a = a - c;         \
    a = a ^ (c >> 43); \
    b = b - c;         \
    b = b - a;         \
    b = b ^ (a << 9);  \
    c = c - a;         \
    c = c - b;         \
    c = c ^ (b >> 8);  \
    a = a - b;         \
    a = a - c;         \
    a = a ^ (c >> 38); \
    b = b - c;         \
    b = b - a;         \
    b = b ^ (a << 23); \
    c = c - a;         \
    c = c - b;         \
    c = c ^ (b >> 5);  \
    a = a - b;         \
    a = a - c;         \
    a = a ^ (c >> 35); \
    b = b - c;         \
    b = b - a;         \
    b = b ^ (a << 49); \
    c = c - a;         \
    c = c - b;         \
    c = c ^ (b >> 11); \
    a = a - b;         \
    a = a - c;         \
    a = a ^ (c >> 12); \
    b = b - c;         \
    b = b - a;         \
    b = b ^ (a << 18); \
    c = c - a;         \
    c = c - b;         \
    c = c ^ (b >> 22); \
  }

#define extract_word(k)                                                                      \
  (k[0] + ((uintptr_t)k[1] << 8) + ((uintptr_t)k[2] << 16) + ((uintptr_t)k[3] << 24) + \
   ((uintptr_t)k[4] << 32) + ((uintptr_t)k[5] << 40) + ((uintptr_t)k[6] << 48) +       \
   ((uintptr_t)k[7] << 52))

inline uintptr_t
hash_string(int initval, const unsigned char *k, int length) {
  register uintptr_t a = GOLDEN_RATIO, b = GOLDEN_RATIO, c = initval;
  register int len;
  for (len = length; len >= 24; len -= 24) {
    a += extract_word(k);
    k += 8;
    b += extract_word(k);
    k += 8;
    c += extract_word(k);
    k += 8;
    hash_mix(a, b, c);
  }

  /*------------------------------------- handle the last 11 bytes */
  c += length;
  switch (len) {
  /* all the case statements fall through */
  case 23:
    c += ((uintptr_t)k[22] << 52);
  case 22:
    c += ((uintptr_t)k[21] << 48);
  case 21:
    c += ((uintptr_t)k[20] << 40);
  case 20:
    c += ((uintptr_t)k[19] << 32);
  case 19:
    c += ((uintptr_t)k[18] << 24);
  case 18:
    c += ((uintptr_t)k[17] << 16);
  case 17:
    c += ((uintptr_t)k[16] << 8);
  /* the first byte of c is reserved for the length */
  case 16:
    b += ((uintptr_t)k[15] << 52);
  case 15:
    b += ((uintptr_t)k[14] << 48);
  case 14:
    b += ((uintptr_t)k[13] << 40);
  case 13:
    b += ((uintptr_t)k[12] << 32);
  case 12:
    b += ((uintptr_t)k[11] << 24);
  case 11:
    b += ((uintptr_t)k[10] << 16);
  case 10:
    b += ((uintptr_t)k[9] << 8);
  case 9:
    b += k[8];
  case 8:
    a += ((uintptr_t)k[7] << 52);
  case 7:
    a += ((uintptr_t)k[6] << 48);
  case 6:
    a += ((uintptr_t)k[5] << 40);
  case 5:
    a += ((uintptr_t)k[4] << 32);
  case 4:
    a += ((uintptr_t)k[3] << 24);
  case 3:
    a += ((uintptr_t)k[2] << 16);
  case 2:
    a += ((uintptr_t)k[1] << 8);
  case 1:
    a += k[0];
    /* case 0: nothing left to add */
  }
  hash_mix(a, b, c);
  /*-------------------------------------------- report the result */
  return c;
}

#else
/*
 * 32 bit version
 */

#define GOLDEN_RATIO 0x9e3779b9L
#define hash_mix(a, b, c) \
  {                  \
    a -= b;          \
    a -= c;          \
    a ^= (c >> 13);  \
    b -= c;          \
    b -= a;          \
    b ^= (a << 8);   \
    c -= a;          \
    c -= b;          \
    c ^= (b >> 13);  \
    a -= b;          \
    a -= c;          \
    a ^= (c >> 12);  \
    b -= c;          \
    b -= a;          \
    b ^= (a << 16);  \
    c -= a;          \
    c -= b;          \
    c ^= (b >> 5);   \
    a -= b;          \
    a -= c;          \
    a ^= (c >> 3);   \
    b -= c;          \
    b -= a;          \
    b ^= (a << 10);  \
    c -= a;          \
    c -= b;          \
    c ^= (b >> 15);  \
  }
#define extract_word(k) \
  (k[0] + ((uintptr_t)k[1] << 8) + ((uintptr_t)k[2] << 16) + ((uintptr_t)k[3] << 24))

inline uintptr_t
hash_string(int initval, const unsigned char *k, int length) {
  register uintptr_t a = GOLDEN_RATIO, b = GOLDEN_RATIO, c = initval;
  register int len;
  for (len = length; len >= 12; len -= 12) {
    a += extract_word(k);
    k += 4;
    b += extract_word(k);
    k += 4;
    c += extract_word(k);
    k += 4;
    hash_mix(a, b, c);
  }

  /*------------------------------------- handle the last 11 bytes */
  c += length;
  switch (len) {
  /* all the case statements fall through */
  case 11:
    c += ((uintptr_t)k[10] << 24);
  case 10:
    c += ((uintptr_t)k[9] << 16);
  case 9:
    c += ((uintptr_t)k[8] << 8);
  /* the first byte of c is reserved for the length */
  case 8:
    b += ((uintptr_t)k[7] << 24);
  case 7:
    b += ((uintptr_t)k[6] << 16);
  case 6:
    b += ((uintptr_t)k[5] << 8);
  case 5:
    b += k[4];
  case 4:
    a += ((uintptr_t)k[3] << 24);
  case 3:
    a += ((uintptr_t)k[2] << 16);
  case 2:
    a += ((uintptr_t)k[1] << 8);
  case 1:
    a += k[0];
    /* case 0: nothing left to add */
  }
  hash_mix(a, b, c);
  /*-------------------------------------------- report the result */
  return c;
}
#endif

inline uintptr_t hash_word(uintptr_t c, uintptr_t w) {
  uintptr_t a = w + GOLDEN_RATIO, b = GOLDEN_RATIO;
  hash_mix(a, b, c);
  return c;
}
#if 0
inline uintptr_t hash_base_string(const char *s, int len, uintptr_t h) {
  uintptr_t a = GOLDEN_RATIO, b = GOLDEN_RATIO, i;
  for (i = len; i >= 3; i -= 3) {
    a += *s;
    s++;
    b += *s;
    s++;
    h += *s;
    s++;
    hash_mix(a, b, h);
  }
  switch (i) {
  case 2:
    a += *s;
    s++;
  case 1:
    b += *s;
  default:
    h += len;
  }
  hash_mix(a, b, h);
  return h;
}

#ifdef CLASP_UNICODE
static uintptr_t hash_full_string(const claspCharacter *s, int len, int h) {
  uintptr_t a = GOLDEN_RATIO, b = GOLDEN_RATIO, i;
  for (i = len; i >= 3; i -= 3) {
    a += (*s);
    s++;
    b += (*s);
    s++;
    h += (*s);
    s++;
    mix(a, b, h);
  }
  switch (i) {
  case 2:
    a += (*s);
    s++;
  case 1:
    b += (*s);
  default:
    h += len;
  }
  mix(a, b, h);
  return h;
}
#endif
#endif // #if 0
#endif
