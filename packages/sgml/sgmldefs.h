/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef SGMLDEFS_H_INCLUDED
#define SGMLDEFS_H_INCLUDED

#ifdef HAVE_CONFIG_H
#include <config.h>
#else
#ifdef WIN32
#define HAVE_MALLOC_H 1
#define HAVE_IO_H 1
#endif
#endif

#ifdef HAVE_DMALLOC_H
#include <dmalloc.h>
#endif

#if 0
#define UTF8 1				/* Include UTF-8 decoding */
#endif

#define XMLNS 1				/* support XML namespaces */

		 /*******************************
		 *    INPUT/OUTPUT CHARACTERS	*
		 *******************************/

typedef unsigned char ochar;		/* output character */
typedef unsigned char ichar;		/* input character */

#define USE_STRING_FUNCTIONS 1		/* use built-in str* functions */


		 /*******************************
		 *	       LIMITS		*
		 *******************************/

#define INPUT_CHARSET_SIZE	(1<<(sizeof(ichar)*8))
#define OUTPUT_CHARSET_SIZE	(1<<(sizeof(ochar)*8))
#define SYMBOLHASHSIZE		256
#define MAXSTRINGLEN		1024
#define MAXNMLEN		256
#define MAXDECL			10240
#define MAXATTELEM		256	/* #elements in one ATTLIST */
#define MAXNAMEGROUP		256	/* #names in a (group) */
#define MAXATTRIBUTES		256	/* attributes per element */
#define MAXMAPLEN		 32	/* max sequence length for SHORTREF */


		 /*******************************
		 *	    CONSTANTS		*
		 *******************************/

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#endif /*SGMLDEFS_H_INCLUDED*/
