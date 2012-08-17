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
#define MSG_THROW		 16
#define MSG_SRCLOC		 17
#define MSG_PROC		 18

#define MSG_COMP_ARGVAR		 19
#define MSG_UNLOAD		 20
#define MSG_INDEX_FIND		 21
#define MSG_INDEX_UPDATE	 22
#define MSG_JIT			 23
#define MSG_TRACE		 24

						/* GC messages */
#define	MSG_AGC			 100
#define	MSG_CLAUSE_GC		 101
#define	MSG_GC_STATS		 102
#define	MSG_GC_SCHEDULE		 103
#define	MSG_GC_PROGRESS		 104
#define	MSG_GC_MARK_VAR		 105
#define	MSG_GC_MARK_GVAR	 106
#define	MSG_GC_MARK_ATTVAR	 107
#define	MSG_GC_MARK_TERMREF	 108
#define	MSG_GC_MARK_FOREIGN	 109
#define	MSG_GC_MARK_ARGS	 110
#define	MSG_GC_MARK_QUERY	 111
#define	MSG_GC_MARK_VAR_WALK	 112
#define	MSG_GC_CLEAR		 113
#define	MSG_GC_ASSIGNMENTS	 114
#define	MSG_GC_ASSIGNMENTS_MERGE 115
#define	MSG_GC_ASSIGNMENTS_MARK	 116
#define	MSG_GC_RESET		 117
#define	MSG_GC_WALK		 118
#define	MSG_GC_RELOC		 119
#define	MSG_GC_HOLE		 120
#define	MSG_GC_SWEEP		 121
#define	MSG_GC_CHECK		 122
#define	MSG_SHIFT_PROGRESS	 123
#define	MSG_SHIFT_POINTER	 124
#define	MSG_SHIFT_FRAME		 125
#define	MSG_STACK_OVERFLOW	 126

#define CHK_SECURE    1000

typedef struct debug_topic
{ unsigned	code;
  const char   *name;
} debug_topic;

COMMON(void)	cleanupDebug(void);
COMMON(int)	prolog_debug_from_string(const char *spec, int flag);

#endif /*PL_DEBUG_INCLUDED*/
