/*
 * Copyright (c) 1998, 1999 Henry Spencer.  All rights reserved.
 * 
 * Development of this software was funded, in part, by Cray Research Inc.,
 * UUNET Communications Services Inc., Sun Microsystems Inc., and Scriptics
 * Corporation, none of whom are responsible for the results.  The author
 * thanks all of them. 
 * 
 * Redistribution and use in source and binary forms -- with or without
 * modification -- are permitted for any purpose, provided that
 * redistributions in source form retain this entire copyright notice and
 * indicate the origin and nature of any modifications.
 * 
 * I'd appreciate being given credit for this package in the documentation
 * of software which uses it, but that is not a requirement.
 * 
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 * HENRY SPENCER BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* headers if any */
#define tree pceTree
#define string pceString
#include <h/kernel.h>
#undef tree
#undef string

#undef NOTREACHED
#undef EOS
#undef OFF

#define _ANSI_ARGS_(args) args
#define UCHAR(c) ((unsigned char)(c))

/* overrides for regguts.h definitions, if any */
#define	FUNCPTR(name, args)	(*name) _ANSI_ARGS_(args)
#ifndef NOPCE
#define	MALLOC(n)		pceMalloc(n)
#define	FREE(p)			pceFree(VS(p))
#define	REALLOC(p,n)		pceRealloc(VS(p),n)
#else
#undef assert
#endif

#include "regtypes.h"

/* internal character type and related */
#ifdef REG_WIDE
typedef charW chr;		/* the type itself */
typedef int pchr;		/* what it promotes to */
typedef unsigned uchr;		/* unsigned type that will hold a chr */
typedef int celt;		/* type to hold chr, MCCE number, or NOCELT */
#define	NOCELT	(-1)		/* celt value which is not valid chr or MCCE */
#define	CHR(c)	(UCHAR(c))	/* turn char literal into chr literal */
#define	DIGITVAL(c) ((c)-'0')	/* turn chr digit into its value */
#ifndef WIN32			/* TBD, but why not sizeof!? */
#define	CHRBITS	32		/* bits in a chr; must not use sizeof */
#define	CHR_MIN	0x00000000	/* smallest and largest chr; the value */
#define	CHR_MAX	0x3fffffff	/* CHR_MAX-CHR_MIN+1 should fit in uchr */
#else
#define	CHRBITS	16		/* bits in a chr; must not use sizeof */
#define	CHR_MIN	0x0000		/* smallest and largest chr; the value */
#define	CHR_MAX	0xffff		/*  CHR_MAX-CHR_MIN+1 should fit in uchr */
#endif
#else /*REG_WIDE*/
typedef charA chr;		/* the type itself */
typedef int pchr;		/* what it promotes to */
typedef unsigned uchr;		/* unsigned type that will hold a chr */
typedef int celt;		/* type to hold chr, MCCE number, or NOCELT */
#define	NOCELT	(-1)		/* celt value which is not valid chr or MCCE */
#define	CHR(c)	(UCHAR(c))	/* turn char literal into chr literal */
#define	DIGITVAL(c) ((c)-'0')	/* turn chr digit into its value */
#define	CHRBITS	8		/* bits in a chr; must not use sizeof */
#define	CHR_MIN	0x00		/* smallest and largest chr; the value */
#define	CHR_MAX	0xff		/* CHR_MAX-CHR_MIN+1 should fit in uchr */
#endif /*REG_WIDE*/

/* functions operating on chr */
#define	iscalnum(x)	iswalnum(x)
#define	iscalpha(x)	iswalpha(x)
#define	iscdigit(x)	iswdigit(x)
#define	iscspace(x)	iswspace(x)

/* name the external functions */
#ifdef REG_WIDE
#define	compile		__REG_WIDE_COMPILE
#define	exec		__REG_WIDE_EXEC
#else
#define	compile		re_compileA
#define	exec		re_execA
#endif

/* enable/disable debugging code (by whether REG_DEBUG is defined or not) */
#if 0		/* no debug unless requested by makefile */
#define	REG_DEBUG	/* */
#endif

/* and pick up the standard header */
#include "regex.h"
