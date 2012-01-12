/*  Part of SWI-Prolog

    Author:        Keri Harris
    E-mail:        keri.harris@securitease.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PL_DEBUG_INCLUDED
#define PL_DEBUG_INCLUDED 1

#define MAX_TOPIC_LEN 32

#define DBG_LEVEL0  0
#define DBG_LEVEL1  1
#define DBG_LEVEL2  2
#define DBG_LEVEL3  3
#define DBG_LEVEL4  4
#define DBG_LEVEL5  5
#define DBG_LEVEL6  6
#define DBG_LEVEL7  7
#define DBG_LEVEL8  8
#define DBG_LEVEL9  9

#define	MSG_VMI			 10
#define	MSG_CLEANUP		 11
#define	MSG_PROLOG_FLAG		 12
#define	MSG_HASH_STAT		 13
#define	MSG_SPARE_STACK		 14
#define	MSG_THREAD		 15
#define	MSG_AGC			 16
#define	MSG_CLAUSE_GC		 17
#define	MSG_GC_STATS		 18
#define	MSG_GC_SCHEDULE		 19
#define	MSG_GC_PROGRESS		 20
#define	MSG_GC_MARK_VAR		 21
#define	MSG_GC_MARK_GVAR	 22
#define	MSG_GC_MARK_ATTVAR	 23
#define	MSG_GC_MARK_TERMREF	 24
#define	MSG_GC_MARK_FOREIGN	 25
#define	MSG_GC_MARK_ARGS	 26
#define	MSG_GC_MARK_QUERY	 27
#define	MSG_GC_MARK_VAR_WALK	 28
#define	MSG_GC_CLEAR		 29
#define	MSG_GC_ASSIGNMENTS	 30
#define	MSG_GC_ASSIGNMENTS_MERGE 31
#define	MSG_GC_ASSIGNMENTS_MARK	 32
#define	MSG_GC_RESET		 33
#define	MSG_GC_WALK		 34
#define	MSG_GC_RELOC		 35
#define	MSG_GC_HOLE		 36
#define	MSG_GC_SWEEP		 37
#define	MSG_GC_CHECK		 38
#define	MSG_SHIFT_PROGRESS	 39
#define	MSG_SHIFT_POINTER	 40
#define	MSG_SHIFT_FRAME		 41
#define	MSG_STACK_OVERFLOW	 42

#define CHK_SECURE    1000

typedef struct debug_topic
{ unsigned	code;
  const char   *name;
} debug_topic;

COMMON(void)	cleanupDebug(void);
COMMON(int)	prolog_debug_from_string(const char *spec, int flag);

#endif /*PL_DEBUG_INCLUDED*/
