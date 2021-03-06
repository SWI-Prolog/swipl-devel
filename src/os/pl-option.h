/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2013, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef OPTION_H_INCLUDED
#define OPTION_H_INCLUDED

#define OPT_BOOL	(0)		/* types */
#define OPT_INT		(1)
#define OPT_STRING	(2)
#define OPT_ATOM	(3)
#define OPT_TERM	(4)		/* arbitrary term */
#define OPT_LONG	(5)
#define OPT_NATLONG	(6)		/* > 0 */
#define OPT_SIZE	(7)		/* size_t */
#define OPT_DOUBLE	(8)
#define OPT_LOCALE	(9)
#define OPT_TYPE_MASK	0xff
#define OPT_INF		0x100		/* allow 'inf' */

#define OPT_ALL		0x1		/* flags */

typedef struct
{ atom_t	name;			/* Name of option */
  int		type;			/* Type of option */
} opt_spec, *OptSpec;

#if USE_LD_MACROS
#define scan_options(list, flags, name, specs, ...) LDFUNC(scan_options, list, flags, name, specs, __VA_ARGS__)
#endif

#define LDFUNC_DECLARATIONS

COMMON(int)		scan_options(term_t list, int flags, atom_t name,
				     const opt_spec *specs, ...);

#undef LDFUNC_DECLARATIONS

#endif /*OPTION_H_INCLUDED*/
