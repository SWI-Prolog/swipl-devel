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
#define MSG_CALL		 17
#define MSG_SRCLOC		 18
#define MSG_PROC		 19

#define MSG_COMP_ARGVAR		 20
#define MSG_UNLOAD		 21
#define MSG_INDEX_FIND		 22
#define MSG_INDEX_UPDATE	 23
#define MSG_TRACE		 25

#define MSG_QLF_INTEGER		 26
#define MSG_QLF_FLOAT		 27
#define MSG_QLF_XR		 28
#define MSG_QLF_TERM		 29
#define MSG_QLF_DIRECTIVE	 30
#define MSG_QLF_PREDICATE	 31
#define MSG_QLF_EXPORT		 32
#define MSG_QLF_VMI		 33
#define MSG_QLF_PATH		 34
#define MSG_QLF_SECTION		 35
#define MSG_QLF_BOOT		 36
#define MSG_QLF_BOOT_READ	 37
#define MSG_PROC_COUNT		 38

#define MSG_QUEUE		 40
#define MSG_QUEUE_WAIT		 41
#define MSG_SIGNAL		 42
#define MSG_COMP_VARS		 43
#define MSG_DICT		 44
#define MSG_PROF_CALLTREE	 45
#define MSG_PROF_TICKS		 46
#define MSG_INFERENCE_LIMIT	 47
#define MSG_NSOLS		 48
#define MSG_SRCFILE		 49
#define MSG_DESTROY_MODULE	 50
#define MSG_QUEUE_GC		 51
#define MSG_ACYCLIC		 52
#define MSG_OPERATOR		 53
#define MSG_MUTEX_GC		 54
#define MSG_REC_ATTVAR		 55

#define MSG_READ_TOKEN		 60
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

#define MSG_ATTVAR_LINK		 150
#define MSG_CALL_RESIDUE_VARS	 151

#define MSG_HASH_TABLE_API	 160
#define MSG_HASH_TABLE_KVS	 161
#define MSG_HASH_TABLE_ENUM	 162

#define MSG_CGC			 170
#define MSG_CGC_CREF		 171
#define MSG_CGC_CREF_PL		 172
#define MSG_CGC_CREF_TRACK	 173
#define MSG_CGC_PRED		 174
#define MSG_CGC_CONSIDER	 175
#define MSG_CGC_STACK		 176

#define MSG_JIT			 180
#define MSG_JIT_DELINDEX	 181

#define MSG_RECONSULT		 190
#define MSG_RECONSULT_PRED	 191
#define MSG_RECONSULT_CLAUSE	 192
#define MSG_RECONSULT_MODULE	 193

#define CHK_SECURE              1000
#define CHK_HIGH_ARITY          1001
#define CHK_HIGHER_ADDRESS      1002

typedef struct debug_topic
{ unsigned	code;
  const char   *name;
} debug_topic;

COMMON(void)	cleanupDebug(void);
COMMON(int)	prolog_debug_from_string(const char *spec, int flag);

#endif /*PL_DEBUG_INCLUDED*/
