
#ifndef _NB_FATFS_DEFS
#define _NB_FATFS_DEFS

#include "nb_ff.h"
#include "nb_diskio.h" /* Declarations of device I/O functions */

namespace FatFS_NB
{

    /* DBCS code ranges and SBCS upper conversion tables */

#if _NB_CODE_PAGE == 932 /* Japanese Shift-JIS */
#define _DF1S 0x81       /* DBC 1st byte range 1 start */
#define _DF1E 0x9F       /* DBC 1st byte range 1 end */
#define _DF2S 0xE0       /* DBC 1st byte range 2 start */
#define _DF2E 0xFC       /* DBC 1st byte range 2 end */
#define _DS1S 0x40       /* DBC 2nd byte range 1 start */
#define _DS1E 0x7E       /* DBC 2nd byte range 1 end */
#define _DS2S 0x80       /* DBC 2nd byte range 2 start */
#define _DS2E 0xFC       /* DBC 2nd byte range 2 end */

#elif _NB_CODE_PAGE == 936 /* Simplified Chinese GBK */
#define _DF1S 0x81
#define _DF1E 0xFE
#define _DS1S 0x40
#define _DS1E 0x7E
#define _DS2S 0x80
#define _DS2E 0xFE

#elif _NB_CODE_PAGE == 949 /* Korean */
#define _DF1S 0x81
#define _DF1E 0xFE
#define _DS1S 0x41
#define _DS1E 0x5A
#define _DS2S 0x61
#define _DS2E 0x7A
#define _DS3S 0x81
#define _DS3E 0xFE

#elif _NB_CODE_PAGE == 950 /* Traditional Chinese Big5 */
#define _DF1S 0x81
#define _DF1E 0xFE
#define _DS1S 0x40
#define _DS1E 0x7E
#define _DS2S 0xA1
#define _DS2E 0xFE

#elif _NB_CODE_PAGE == 437 /* U.S. */
#define _DF1S 0
#define _EXCVT {0x80, 0x9A, 0x45, 0x41, 0x8E, 0x41, 0x8F, 0x80, 0x45, 0x45, 0x45, 0x49, 0x49, 0x49, 0x8E, 0x8F, \
                0x90, 0x92, 0x92, 0x4F, 0x99, 0x4F, 0x55, 0x55, 0x59, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0x41, 0x49, 0x4F, 0x55, 0xA5, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 720 /* Arabic */
#define _DF1S 0
#define _EXCVT {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, \
                0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 737 /* Greek */
#define _DF1S 0
#define _EXCVT {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, \
                0x90, 0x92, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, \
                0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, 0x90, 0x91, 0xAA, 0x92, 0x93, 0x94, 0x95, 0x96, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0x97, 0xEA, 0xEB, 0xEC, 0xE4, 0xED, 0xEE, 0xEF, 0xF5, 0xF0, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 771 /* KBL */
#define _DF1S 0
#define _EXCVT {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, \
                0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDC, 0xDE, 0xDE, \
                0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0xF0, 0xF0, 0xF2, 0xF2, 0xF4, 0xF4, 0xF6, 0xF6, 0xF8, 0xF8, 0xFA, 0xFA, 0xFC, 0xFC, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 775 /* Baltic */
#define _DF1S 0
#define _EXCVT {0x80, 0x9A, 0x91, 0xA0, 0x8E, 0x95, 0x8F, 0x80, 0xAD, 0xED, 0x8A, 0x8A, 0xA1, 0x8D, 0x8E, 0x8F, \
                0x90, 0x92, 0x92, 0xE2, 0x99, 0x95, 0x96, 0x97, 0x97, 0x99, 0x9A, 0x9D, 0x9C, 0x9D, 0x9E, 0x9F, \
                0xA0, 0xA1, 0xE0, 0xA3, 0xA3, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xB5, 0xB6, 0xB7, 0xB8, 0xBD, 0xBE, 0xC6, 0xC7, 0xA5, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE5, 0xE5, 0xE6, 0xE3, 0xE8, 0xE8, 0xEA, 0xEA, 0xEE, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 850 /* Latin 1 */
#define _DF1S 0
#define _EXCVT {0x43, 0x55, 0x45, 0x41, 0x41, 0x41, 0x41, 0x43, 0x45, 0x45, 0x45, 0x49, 0x49, 0x49, 0x41, 0x41, \
                0x45, 0x92, 0x92, 0x4F, 0x4F, 0x4F, 0x55, 0x55, 0x59, 0x4F, 0x55, 0x4F, 0x9C, 0x4F, 0x9E, 0x9F, \
                0x41, 0x49, 0x4F, 0x55, 0xA5, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0x41, 0x41, 0x41, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0x41, 0x41, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD1, 0xD1, 0x45, 0x45, 0x45, 0x49, 0x49, 0x49, 0x49, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0x49, 0xDF, \
                0x4F, 0xE1, 0x4F, 0x4F, 0x4F, 0x4F, 0xE6, 0xE8, 0xE8, 0x55, 0x55, 0x55, 0x59, 0x59, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 852 /* Latin 2 */
#define _DF1S 0
#define _EXCVT {0x80, 0x9A, 0x90, 0xB6, 0x8E, 0xDE, 0x8F, 0x80, 0x9D, 0xD3, 0x8A, 0x8A, 0xD7, 0x8D, 0x8E, 0x8F, \
                0x90, 0x91, 0x91, 0xE2, 0x99, 0x95, 0x95, 0x97, 0x97, 0x99, 0x9A, 0x9B, 0x9B, 0x9D, 0x9E, 0xAC, \
                0xB5, 0xD6, 0xE0, 0xE9, 0xA4, 0xA4, 0xA6, 0xA6, 0xA8, 0xA8, 0xAA, 0x8D, 0xAC, 0xB8, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBD, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC6, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD1, 0xD1, 0xD2, 0xD3, 0xD2, 0xD5, 0xD6, 0xD7, 0xB7, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE3, 0xD5, 0xE6, 0xE6, 0xE8, 0xE9, 0xE8, 0xEB, 0xED, 0xED, 0xDD, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xEB, 0xFC, 0xFC, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 855 /* Cyrillic */
#define _DF1S 0
#define _EXCVT {0x81, 0x81, 0x83, 0x83, 0x85, 0x85, 0x87, 0x87, 0x89, 0x89, 0x8B, 0x8B, 0x8D, 0x8D, 0x8F, 0x8F, \
                0x91, 0x91, 0x93, 0x93, 0x95, 0x95, 0x97, 0x97, 0x99, 0x99, 0x9B, 0x9B, 0x9D, 0x9D, 0x9F, 0x9F, \
                0xA1, 0xA1, 0xA3, 0xA3, 0xA5, 0xA5, 0xA7, 0xA7, 0xA9, 0xA9, 0xAB, 0xAB, 0xAD, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB6, 0xB6, 0xB8, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBE, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC7, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD1, 0xD1, 0xD3, 0xD3, 0xD5, 0xD5, 0xD7, 0xD7, 0xDD, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xE0, 0xDF, \
                0xE0, 0xE2, 0xE2, 0xE4, 0xE4, 0xE6, 0xE6, 0xE8, 0xE8, 0xEA, 0xEA, 0xEC, 0xEC, 0xEE, 0xEE, 0xEF, \
                0xF0, 0xF2, 0xF2, 0xF4, 0xF4, 0xF6, 0xF6, 0xF8, 0xF8, 0xFA, 0xFA, 0xFC, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 857 /* Turkish */
#define _DF1S 0
#define _EXCVT {0x80, 0x9A, 0x90, 0xB6, 0x8E, 0xB7, 0x8F, 0x80, 0xD2, 0xD3, 0xD4, 0xD8, 0xD7, 0x49, 0x8E, 0x8F, \
                0x90, 0x92, 0x92, 0xE2, 0x99, 0xE3, 0xEA, 0xEB, 0x98, 0x99, 0x9A, 0x9D, 0x9C, 0x9D, 0x9E, 0x9E, \
                0xB5, 0xD6, 0xE0, 0xE9, 0xA5, 0xA5, 0xA6, 0xA6, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC7, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0x49, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE5, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xDE, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 860 /* Portuguese */
#define _DF1S 0
#define _EXCVT {0x80, 0x9A, 0x90, 0x8F, 0x8E, 0x91, 0x86, 0x80, 0x89, 0x89, 0x92, 0x8B, 0x8C, 0x98, 0x8E, 0x8F, \
                0x90, 0x91, 0x92, 0x8C, 0x99, 0xA9, 0x96, 0x9D, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0x86, 0x8B, 0x9F, 0x96, 0xA5, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 861 /* Icelandic */
#define _DF1S 0
#define _EXCVT {0x80, 0x9A, 0x90, 0x41, 0x8E, 0x41, 0x8F, 0x80, 0x45, 0x45, 0x45, 0x8B, 0x8B, 0x8D, 0x8E, 0x8F, \
                0x90, 0x92, 0x92, 0x4F, 0x99, 0x8D, 0x55, 0x97, 0x97, 0x99, 0x9A, 0x9D, 0x9C, 0x9D, 0x9E, 0x9F, \
                0xA4, 0xA5, 0xA6, 0xA7, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 862 /* Hebrew */
#define _DF1S 0
#define _EXCVT {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, \
                0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0x41, 0x49, 0x4F, 0x55, 0xA5, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 863 /* Canadian-French */
#define _DF1S 0
#define _EXCVT {0x43, 0x55, 0x45, 0x41, 0x41, 0x41, 0x86, 0x43, 0x45, 0x45, 0x45, 0x49, 0x49, 0x8D, 0x41, 0x8F, \
                0x45, 0x45, 0x45, 0x4F, 0x45, 0x49, 0x55, 0x55, 0x98, 0x4F, 0x55, 0x9B, 0x9C, 0x55, 0x55, 0x9F, \
                0xA0, 0xA1, 0x4F, 0x55, 0xA4, 0xA5, 0xA6, 0xA7, 0x49, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 864 /* Arabic */
#define _DF1S 0
#define _EXCVT {0x80, 0x9A, 0x45, 0x41, 0x8E, 0x41, 0x8F, 0x80, 0x45, 0x45, 0x45, 0x49, 0x49, 0x49, 0x8E, 0x8F, \
                0x90, 0x92, 0x92, 0x4F, 0x99, 0x4F, 0x55, 0x55, 0x59, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0x41, 0x49, 0x4F, 0x55, 0xA5, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 865 /* Nordic */
#define _DF1S 0
#define _EXCVT {0x80, 0x9A, 0x90, 0x41, 0x8E, 0x41, 0x8F, 0x80, 0x45, 0x45, 0x45, 0x49, 0x49, 0x49, 0x8E, 0x8F, \
                0x90, 0x92, 0x92, 0x4F, 0x99, 0x4F, 0x55, 0x55, 0x59, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0x41, 0x49, 0x4F, 0x55, 0xA5, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, \
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 866 /* Russian */
#define _DF1S 0
#define _EXCVT {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, \
                0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, \
                0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, \
                0xF0, 0xF0, 0xF2, 0xF2, 0xF4, 0xF4, 0xF6, 0xF6, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 869 /* Greek 2 */
#define _DF1S 0
#define _EXCVT {0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, \
                0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x86, 0x9C, 0x8D, 0x8F, 0x90, \
                0x91, 0x90, 0x92, 0x95, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, \
                0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, \
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, \
                0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xA4, 0xA5, 0xA6, 0xD9, 0xDA, 0xDB, 0xDC, 0xA7, 0xA8, 0xDF, \
                0xA9, 0xAA, 0xAC, 0xAD, 0xB5, 0xB6, 0xB7, 0xB8, 0xBD, 0xBE, 0xC6, 0xC7, 0xCF, 0xCF, 0xD0, 0xEF, \
                0xF0, 0xF1, 0xD1, 0xD2, 0xD3, 0xF5, 0xD4, 0xF7, 0xF8, 0xF9, 0xD5, 0x96, 0x95, 0x98, 0xFE, 0xFF}

#elif _NB_CODE_PAGE == 1 /* ASCII (for only non-LFN cfg) */
#if _NB_USE_LFN != 0
#error Cannot enable LFN without valid code page.
#endif
#define _DF1S 0

#else
#error Unknown code page

#endif

/* Character code support macros */
#define IsUpper(c) (((c) >= 'A') && ((c) <= 'Z'))
#define IsLower(c) (((c) >= 'a') && ((c) <= 'z'))
#define IsDigit(c) (((c) >= '0') && ((c) <= '9'))

#if _DF1S != 0 /* Code page is DBCS */

#ifdef _DF2S /* Two 1st byte areas */
#define IsDBCS1(c) (((BYTE)(c) >= _DF1S && (BYTE)(c) <= _DF1E) || ((BYTE)(c) >= _DF2S && (BYTE)(c) <= _DF2E))
#else /* One 1st byte area */
#define IsDBCS1(c) ((BYTE)(c) >= _DF1S && (BYTE)(c) <= _DF1E)
#endif

#ifdef _DS3S /* Three 2nd byte areas */
#define IsDBCS2(c) (((BYTE)(c) >= _DS1S && (BYTE)(c) <= _DS1E) || ((BYTE)(c) >= _DS2S && (BYTE)(c) <= _DS2E) || ((BYTE)(c) >= _DS3S && (BYTE)(c) <= _DS3E))
#else /* Two 2nd byte areas */
#define IsDBCS2(c) (((BYTE)(c) >= _DS1S && (BYTE)(c) <= _DS1E) || ((BYTE)(c) >= _DS2S && (BYTE)(c) <= _DS2E))
#endif

#else /* Code page is SBCS */

#define IsDBCS1(c) 0
#define IsDBCS2(c) 0

#endif /* _DF1S */

/* Additional file attribute bits for internal use */
#define AM_VOL 0x08  /* Volume label */
#define AM_LFN 0x0F  /* LFN entry */
#define AM_MASK 0x3F /* Mask of defined bits */

/* Additional file access control and file status flags for internal use */
#define FA_SEEKEND 0x20  /* Seek to end of the file on file open */
#define FA_MODIFIED 0x40 /* File has been modified */
#define FA_DIRTY 0x80    /* FIL.buf[] needs to be written-back */

/* Name status flags in fn[] */
#define NSFLAG 11      /* Index of the name status byte */
#define NS_LOSS 0x01   /* Out of 8.3 format */
#define NS_LFN 0x02    /* Force to create LFN entry */
#define NS_LAST 0x04   /* Last segment */
#define NS_BODY 0x08   /* Lower case flag (body) */
#define NS_EXT 0x10    /* Lower case flag (ext) */
#define NS_DOT 0x20    /* Dot entry */
#define NS_NOLFN 0x40  /* Do not find LFN */
#define NS_NONAME 0x80 /* Not followed */

/* Limits and boundaries */
#define MAX_DIR 0x200000      /* Max size of FAT directory */
#define MAX_DIR_EX 0x10000000 /* Max size of exFAT directory */
#define MAX_FAT12 0xFF5       /* Max FAT12 clusters (differs from specs, but correct for real DOS/Windows behavior) */
#define MAX_FAT16 0xFFF5      /* Max FAT16 clusters (differs from specs, but correct for real DOS/Windows behavior) */
#define MAX_FAT32 0x0FFFFFF5  /* Max FAT32 clusters (not specified, practical limit) */
#define MAX_EXFAT 0x7FFFFFFD  /* Max exFAT clusters (differs from specs, implementation limit) */

    /* FatFs refers the FAT structure as simple byte array instead of structure member
    / because the C structure is not binary compatible between different platforms */

#define BS_JmpBoot 0      /* x86 jump instruction (3-byte) */
#define BS_OEMName 3      /* OEM name (8-byte) */
#define BPB_BytsPerSec 11 /* Sector size [byte] (WORD) */
#define BPB_SecPerClus 13 /* Cluster size [sector] (BYTE) */
#define BPB_RsvdSecCnt 14 /* Size of reserved area [sector] (WORD) */
#define BPB_NumFATs 16    /* Number of FATs (BYTE) */
#define BPB_RootEntCnt 17 /* Size of root directory area for FAT12/16 [entry] (WORD) */
#define BPB_TotSec16 19   /* Volume size (16-bit) [sector] (WORD) */
#define BPB_Media 21      /* Media descriptor byte (BYTE) */
#define BPB_FATSz16 22    /* FAT size (16-bit) [sector] (WORD) */
#define BPB_SecPerTrk 24  /* Track size for int13h [sector] (WORD) */
#define BPB_NumHeads 26   /* Number of heads for int13h (WORD) */
#define BPB_HiddSec 28    /* Volume offset from top of the drive (DWORD) */
#define BPB_TotSec32 32   /* Volume size (32-bit) [sector] (DWORD) */
#define BS_DrvNum 36      /* Physical drive number for int13h (BYTE) */
#define BS_NTres 37       /* Error flag (BYTE) */
#define BS_BootSig 38     /* Extended boot signature (BYTE) */
#define BS_VolID 39       /* Volume serial number (DWORD) */
#define BS_VolLab 43      /* Volume label string (8-byte) */
#define BS_FilSysType 54  /* File system type string (8-byte) */
#define BS_BootCode 62    /* Boot code (448-byte) */
#define BS_55AA 510       /* Signature word (WORD) */

#define BPB_FATSz32 36     /* FAT32: FAT size [sector] (DWORD) */
#define BPB_ExtFlags32 40  /* FAT32: Extended flags (WORD) */
#define BPB_FSVer32 42     /* FAT32: File system version (WORD) */
#define BPB_RootClus32 44  /* FAT32: Root directory cluster (DWORD) */
#define BPB_FSInfo32 48    /* FAT32: Offset of FSINFO sector (WORD) */
#define BPB_BkBootSec32 50 /* FAT32: Offset of backup boot sector (WORD) */
#define BS_DrvNum32 64     /* FAT32: Physical drive number for int13h (BYTE) */
#define BS_NTres32 65      /* FAT32: Error flag (BYTE) */
#define BS_BootSig32 66    /* FAT32: Extended boot signature (BYTE) */
#define BS_VolID32 67      /* FAT32: Volume serial number (DWORD) */
#define BS_VolLab32 71     /* FAT32: Volume label string (8-byte) */
#define BS_FilSysType32 82 /* FAT32: File system type string (8-byte) */
#define BS_BootCode32 90   /* FAT32: Boot code (420-byte) */

#define BPB_ZeroedEx 11      /* exFAT: MBZ field (53-byte) */
#define BPB_VolOfsEx 64      /* exFAT: Volume offset from top of the drive [sector] (QWORD) */
#define BPB_TotSecEx 72      /* exFAT: Volume size [sector] (QWORD) */
#define BPB_FatOfsEx 80      /* exFAT: FAT offset from top of the volume [sector] (DWORD) */
#define BPB_FatSzEx 84       /* exFAT: FAT size [sector] (DWORD) */
#define BPB_DataOfsEx 88     /* exFAT: Data offset from top of the volume [sector] (DWORD) */
#define BPB_NumClusEx 92     /* exFAT: Number of clusters (DWORD) */
#define BPB_RootClusEx 96    /* exFAT: Root directory start cluster (DWORD) */
#define BPB_VolIDEx 100      /* exFAT: Volume serial number (DWORD) */
#define BPB_FSVerEx 104      /* exFAT: File system version (WORD) */
#define BPB_VolFlagEx 106    /* exFAT: Volume flags (BYTE) */
#define BPB_ActFatEx 107     /* exFAT: Active FAT flags (BYTE) */
#define BPB_BytsPerSecEx 108 /* exFAT: Log2 of sector size in unit of byte (BYTE) */
#define BPB_SecPerClusEx 109 /* exFAT: Log2 of cluster size in unit of sector (BYTE) */
#define BPB_NumFATsEx 110    /* exFAT: Number of FATs (BYTE) */
#define BPB_DrvNumEx 111     /* exFAT: Physical drive number for int13h (BYTE) */
#define BPB_PercInUseEx 112  /* exFAT: Percent in use (BYTE) */
#define BPB_RsvdEx 113       /* exFAT: Reserved (7-byte) */
#define BS_BootCodeEx 120    /* exFAT: Boot code (390-byte) */

#define DIR_Name 0            /* Short file name (11-byte) */
#define DIR_Attr 11           /* Attribute (BYTE) */
#define DIR_NTres 12          /* Lower case flag (BYTE) */
#define DIR_CrtTime10 13      /* Created time sub-second (BYTE) */
#define DIR_CrtTime 14        /* Created time (DWORD) */
#define DIR_LstAccDate 18     /* Last accessed date (WORD) */
#define DIR_FstClusHI 20      /* Higher 16-bit of first cluster (WORD) */
#define DIR_ModTime 22        /* Modified time (DWORD) */
#define DIR_FstClusLO 26      /* Lower 16-bit of first cluster (WORD) */
#define DIR_FileSize 28       /* File size (DWORD) */
#define LDIR_Ord 0            /* LFN: LFN order and LLE flag (BYTE) */
#define LDIR_Attr 11          /* LFN: LFN attribute (BYTE) */
#define LDIR_Type 12          /* LFN: Entry type (BYTE) */
#define LDIR_Chksum 13        /* LFN: Checksum of the SFN (BYTE) */
#define LDIR_FstClusLO 26     /* LFN: MBZ field (WORD) */
#define XDIR_Type 0           /* exFAT: Type of exFAT directory entry (BYTE) */
#define XDIR_NumLabel 1       /* exFAT: Number of volume label characters (BYTE) */
#define XDIR_Label 2          /* exFAT: Volume label (11-WORD) */
#define XDIR_CaseSum 4        /* exFAT: Sum of case conversion table (DWORD) */
#define XDIR_NumSec 1         /* exFAT: Number of secondary entries (BYTE) */
#define XDIR_SetSum 2         /* exFAT: Sum of the set of directory entries (WORD) */
#define XDIR_Attr 4           /* exFAT: File attribute (WORD) */
#define XDIR_CrtTime 8        /* exFAT: Created time (DWORD) */
#define XDIR_ModTime 12       /* exFAT: Modified time (DWORD) */
#define XDIR_AccTime 16       /* exFAT: Last accessed time (DWORD) */
#define XDIR_CrtTime10 20     /* exFAT: Created time subsecond (BYTE) */
#define XDIR_ModTime10 21     /* exFAT: Modified time subsecond (BYTE) */
#define XDIR_CrtTZ 22         /* exFAT: Created timezone (BYTE) */
#define XDIR_ModTZ 23         /* exFAT: Modified timezone (BYTE) */
#define XDIR_AccTZ 24         /* exFAT: Last accessed timezone (BYTE) */
#define XDIR_GenFlags 33      /* exFAT: General secondary flags (WORD) */
#define XDIR_NumName 35       /* exFAT: Number of file name characters (BYTE) */
#define XDIR_NameHash 36      /* exFAT: Hash of file name (WORD) */
#define XDIR_ValidFileSize 40 /* exFAT: Valid file size (QWORD) */
#define XDIR_FstClus 52       /* exFAT: First cluster of the file data (DWORD) */
#define XDIR_FileSize 56      /* exFAT: File/Directory size (QWORD) */

#define SZDIRE 32  /* Size of a directory entry */
#define DDEM 0xE5  /* Deleted directory entry mark set to DIR_Name[0] */
#define RDDEM 0x05 /* Replacement of the character collides with DDEM */
#define LLEF 0x40  /* Last long entry flag in LDIR_Ord */

#define FSI_LeadSig 0      /* FAT32 FSI: Leading signature (DWORD) */
#define FSI_StrucSig 484   /* FAT32 FSI: Structure signature (DWORD) */
#define FSI_Free_Count 488 /* FAT32 FSI: Number of free clusters (DWORD) */
#define FSI_Nxt_Free 492   /* FAT32 FSI: Last allocated cluster (DWORD) */

#define MBR_Table 446 /* MBR: Offset of partition table in the MBR */
#define SZ_PTE 16     /* MBR: Size of a partition table entry */
#define PTE_Boot 0    /* MBR PTE: Boot indicator */
#define PTE_StHead 1  /* MBR PTE: Start head */
#define PTE_StSec 2   /* MBR PTE: Start sector */
#define PTE_StCyl 3   /* MBR PTE: Start cylinder */
#define PTE_System 4  /* MBR PTE: System ID */
#define PTE_EdHead 5  /* MBR PTE: End head */
#define PTE_EdSec 6   /* MBR PTE: End sector */
#define PTE_EdCyl 7   /* MBR PTE: End cylinder */
#define PTE_StLba 8   /* MBR PTE: Start in LBA */
#define PTE_SizLba 12 /* MBR PTE: Size in LBA */

/* Post process after fatal error on file operation */
#define ABORT(fs, res)         \
    {                          \
        fp->err = (BYTE)(res); \
        LEAVE_FF(fs, res);     \
    }

/* Reentrancy related */
#if _NB_FS_REENTRANT
#if _NB_USE_LFN == 1
#error Static LFN work area cannot be used at thread-safe configuration
#endif
#define ENTER_FF(fs)           \
    {                          \
        if (!lock_fs(fs))      \
            return FR_TIMEOUT; \
    }
#define LEAVE_FF(fs, res)   \
    {                       \
        unlock_fs(fs, res); \
        return res;         \
    }
#else
#define ENTER_FF(fs)
#define LEAVE_FF(fs, res) return res
#endif

/* Definitions of volume - partition conversion */
#if _NB_MULTI_PARTITION
#define LD2PD(vol) VolToPart[vol].pd /* Get physical drive number */
#define LD2PT(vol) VolToPart[vol].pt /* Get partition index */
#else
#define LD2PD(vol) (BYTE)(vol) /* Each logical drive is bound to the same physical drive number */
#define LD2PT(vol) 0           /* Find first valid partition or in SFD */
#endif

/* Definitions of sector size */
#if (_NB_MAX_SS < _NB_MIN_SS) || (_NB_MAX_SS != 512 && _NB_MAX_SS != 1024 && _NB_MAX_SS != 2048 && _NB_MAX_SS != 4096) || (_NB_MIN_SS != 512 && _NB_MIN_SS != 1024 && _NB_MIN_SS != 2048 && _NB_MIN_SS != 4096)
#error Wrong sector size configuration
#endif
#if _NB_MAX_SS == _NB_MIN_SS
#define SS(fs) ((UINT)_NB_MAX_SS) /* Fixed sector size */
#else
#define SS(fs) ((fs)->sectorSize) /* Variable sector size */
#endif

/* Timestamp */
#if _NB_FS_NORTC == 1
#if _NB_NORTC_YEAR < 1980 || _NB_NORTC_YEAR > 2107 || _NB_NORTC_MON < 1 || _NB_NORTC_MON > 12 || _NB_NORTC_MDAY < 1 || _NB_NORTC_MDAY > 31
#error Invalid _FS_NORTC settings
#endif
#define GET_FATTIME() ((DWORD)(_NB_NORTC_YEAR - 1980) << 25 | (DWORD)_NB_NORTC_MON << 21 | (DWORD)_NB_NORTC_MDAY << 16)
#else
#define GET_FATTIME() get_fattime()
#endif

/* File lock controls */
#if _NB_FS_LOCK != 0
#if _NB_FS_READONLY
#error _FS_LOCK must be 0 at read-only configuration
#endif
    typedef struct
    {
        FATFS *fs; /* Object ID 1, volume (NULL:blank entry) */
        DWORD clu; /* Object ID 2, containing directory (0:root) */
        DWORD ofs; /* Object ID 3, offset in the directory */
        WORD ctr;  /* Object open counter, 0:none, 0x01..0xFF:read mode open count, 0x100:write mode */
    } FILESEM;
#endif

}

#endif