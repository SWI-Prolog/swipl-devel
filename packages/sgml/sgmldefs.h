/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

#define UTF8 1				/* Include UTF-8 decoding */
#define XMLNS 1				/* support XML namespaces */

		 /*******************************
		 *    INPUT/OUTPUT CHARACTERS	*
		 *******************************/

typedef unsigned char ochar;		/* output character */
typedef unsigned char ichar;		/* input character */

#define ICHARSET_SIZE (1<<(8*sizeof(ichar)))

#define USE_STRING_FUNCTIONS 1		/* use built-in str* functions */


		 /*******************************
		 *	       LIMITS		*
		 *******************************/

#define INPUT_CHARSET_SIZE	(1<<(sizeof(ichar)*8))
#define OUTPUT_CHARSET_SIZE	(1<<(sizeof(ochar)*8))
#define SYMBOLHASHSIZE		256
#define MAXSTRINGLEN	       2048
#define MAXNMLEN		256
#define MAXDECL		      10240
#define MAXATTELEM		256	/* #elements in one ATTLIST */
#define MAXNAMEGROUP		256	/* #names in a (group) */
#define MAXATTRIBUTES		256	/* attributes per element */
#define MAXMAPLEN		 32	/* max sequence length for SHORTREF */
#define SHORTENTITYFILE		100	/* short external entities in mem */


		 /*******************************
		 *	    CONSTANTS		*
		 *******************************/

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define LF 10
#define CR 13

#endif /*SGMLDEFS_H_INCLUDED*/
