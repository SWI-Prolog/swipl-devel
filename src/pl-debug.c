/*  Part of SWI-Prolog

    Author:        Keri Harris
    E-mail:        keri.harris@securitease.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2025, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include "pl-incl.h"

#define DEBUG_TOPIC(Name) { Name, #Name }

const debug_topic debug_topics[] =
{ DEBUG_TOPIC(DBG_LEVEL0),
  DEBUG_TOPIC(DBG_LEVEL1),
  DEBUG_TOPIC(DBG_LEVEL2),
  DEBUG_TOPIC(DBG_LEVEL3),
  DEBUG_TOPIC(DBG_LEVEL4),
  DEBUG_TOPIC(DBG_LEVEL5),
  DEBUG_TOPIC(DBG_LEVEL6),
  DEBUG_TOPIC(DBG_LEVEL7),
  DEBUG_TOPIC(DBG_LEVEL8),
  DEBUG_TOPIC(DBG_LEVEL9),

  DEBUG_TOPIC(MSG_VMI),
  DEBUG_TOPIC(MSG_CLEANUP),
  DEBUG_TOPIC(MSG_PROLOG_FLAG),
  DEBUG_TOPIC(MSG_HASH_STAT),
  DEBUG_TOPIC(MSG_SPARE_STACK),
  DEBUG_TOPIC(MSG_THREAD),
  DEBUG_TOPIC(MSG_THROW),
  DEBUG_TOPIC(MSG_CALL),
  DEBUG_TOPIC(MSG_SRCLOC),
  DEBUG_TOPIC(MSG_PROC),
  DEBUG_TOPIC(MSG_COMP_ARGVAR),
  DEBUG_TOPIC(MSG_UNLOAD),
  DEBUG_TOPIC(MSG_INDEX_FIND),
  DEBUG_TOPIC(MSG_INDEX_UPDATE),
  DEBUG_TOPIC(MSG_INDEX_DEEP),

  DEBUG_TOPIC(MSG_QLF_INTEGER),
  DEBUG_TOPIC(MSG_QLF_FLOAT),
  DEBUG_TOPIC(MSG_QLF_XR),
  DEBUG_TOPIC(MSG_QLF_TERM),
  DEBUG_TOPIC(MSG_QLF_DIRECTIVE),
  DEBUG_TOPIC(MSG_QLF_PREDICATE),
  DEBUG_TOPIC(MSG_QLF_EXPORT),
  DEBUG_TOPIC(MSG_QLF_VMI),
  DEBUG_TOPIC(MSG_QLF_PATH),
  DEBUG_TOPIC(MSG_QLF_SECTION),
  DEBUG_TOPIC(MSG_QLF_BOOT),
  DEBUG_TOPIC(MSG_QLF_BOOT_READ),
  DEBUG_TOPIC(MSG_QLF_LABEL),
  DEBUG_TOPIC(MSG_QLF_IMPORT),
  DEBUG_TOPIC(MSG_QLF_INCLUDE),
  DEBUG_TOPIC(MSG_PROC_COUNT),
  DEBUG_TOPIC(MSG_CUT),

  DEBUG_TOPIC(MSG_QUEUE),
  DEBUG_TOPIC(MSG_QUEUE_WAIT),
  DEBUG_TOPIC(MSG_SIGNAL),
  DEBUG_TOPIC(MSG_COMP_VARS),
  DEBUG_TOPIC(MSG_DICT),
  DEBUG_TOPIC(MSG_PROF_CALLTREE),
  DEBUG_TOPIC(MSG_PROF_TICKS),
  DEBUG_TOPIC(MSG_INFERENCE_LIMIT),
  DEBUG_TOPIC(MSG_NSOLS),
  DEBUG_TOPIC(MSG_SRCFILE),
  DEBUG_TOPIC(MSG_SRCFILE_REF),
  DEBUG_TOPIC(MSG_DESTROY_MODULE),
  DEBUG_TOPIC(MSG_CREATE_MODULE),
  DEBUG_TOPIC(MSG_QUEUE_GC),
  DEBUG_TOPIC(MSG_ACYCLIC),
  DEBUG_TOPIC(MSG_OPERATOR),
  DEBUG_TOPIC(MSG_MUTEX_GC),
  DEBUG_TOPIC(MSG_REC_ATTVAR),
  DEBUG_TOPIC(MSG_TTY),
  DEBUG_TOPIC(MSG_OS_DIR),
  DEBUG_TOPIC(MSG_COMP_ARG_UNIFY),
  DEBUG_TOPIC(MSG_DETERMINISM),
						/* Parser */
  DEBUG_TOPIC(MSG_READ_TOKEN),
  DEBUG_TOPIC(MSG_LOCALE),

  DEBUG_TOPIC(MSG_CONTINUE),

  DEBUG_TOPIC(MSG_CLEANUP_THREAD),
  DEBUG_TOPIC(MSG_INITIALISE),
  DEBUG_TOPIC(MSG_COPYTERM),
  DEBUG_TOPIC(MSG_PRED_COUNT),

  DEBUG_TOPIC(MSG_BACKTRACK),
  DEBUG_TOPIC(MSG_CSTACK),
  DEBUG_TOPIC(MSG_GMP_ALLOC),
  DEBUG_TOPIC(MSG_GMP_OVERFLOW),

						/* GC messages */
  DEBUG_TOPIC(MSG_AGC),
  DEBUG_TOPIC(MSG_CLAUSE_GC),
  DEBUG_TOPIC(MSG_GC_STATS),
  DEBUG_TOPIC(MSG_GC_SCHEDULE),
  DEBUG_TOPIC(MSG_GC_PROGRESS),
  DEBUG_TOPIC(MSG_GC_MARK_VAR),
  DEBUG_TOPIC(MSG_GC_MARK_GVAR),
  DEBUG_TOPIC(MSG_GC_MARK_ATTVAR),
  DEBUG_TOPIC(MSG_GC_MARK_TERMREF),
  DEBUG_TOPIC(MSG_GC_MARK_FOREIGN),
  DEBUG_TOPIC(MSG_GC_MARK_ARGS),
  DEBUG_TOPIC(MSG_GC_MARK_QUERY),
  DEBUG_TOPIC(MSG_GC_MARK_VAR_WALK),
  DEBUG_TOPIC(MSG_GC_CLEAR),
  DEBUG_TOPIC(MSG_GC_ASSIGNMENTS),
  DEBUG_TOPIC(MSG_GC_ASSIGNMENTS_MERGE),
  DEBUG_TOPIC(MSG_GC_ASSIGNMENTS_MARK),
  DEBUG_TOPIC(MSG_GC_RESET),
  DEBUG_TOPIC(MSG_GC_WALK),
  DEBUG_TOPIC(MSG_GC_RELOC),
  DEBUG_TOPIC(MSG_GC_HOLE),
  DEBUG_TOPIC(MSG_GC_SWEEP),
  DEBUG_TOPIC(MSG_GC_CHECK),
  DEBUG_TOPIC(MSG_SHIFT),
  DEBUG_TOPIC(MSG_SHIFT_PROGRESS),
  DEBUG_TOPIC(MSG_SHIFT_POINTER),
  DEBUG_TOPIC(MSG_SHIFT_FRAME),
  DEBUG_TOPIC(MSG_STACK_OVERFLOW),
  DEBUG_TOPIC(MSG_UNWIND_EXCEPTION),
  DEBUG_TOPIC(MSG_AGC_CONSIDER),

  DEBUG_TOPIC(MSG_TRACE),
  DEBUG_TOPIC(MSG_TRACE_REDO),
  DEBUG_TOPIC(MSG_TRACE_RETRY),
  DEBUG_TOPIC(MSG_TRACE_PORT),
  DEBUG_TOPIC(MSG_TRACE_FRAME),

  DEBUG_TOPIC(MSG_ATTVAR_LINK),
  DEBUG_TOPIC(MSG_CALL_RESIDUE_VARS),
  DEBUG_TOPIC(MSG_SOFTCUT),
  DEBUG_TOPIC(MSG_WAKEUP),
  DEBUG_TOPIC(MSG_UNDO),

  DEBUG_TOPIC(MSG_HASH_TABLE_API),
  DEBUG_TOPIC(MSG_HASH_TABLE_KVS),
  DEBUG_TOPIC(MSG_HASH_TABLE_ENUM),

  DEBUG_TOPIC(MSG_CGC),
  DEBUG_TOPIC(MSG_CGC_CREF),
  DEBUG_TOPIC(MSG_CGC_CREF_PL),
  DEBUG_TOPIC(MSG_CGC_CREF_TRACK),
  DEBUG_TOPIC(MSG_CGC_PRED),
  DEBUG_TOPIC(MSG_CGC_CONSIDER),
  DEBUG_TOPIC(MSG_CGC_STACK),
  DEBUG_TOPIC(MSG_CGC_PRED_REF),
  DEBUG_TOPIC(MSG_CGC_RETRACT),
  DEBUG_TOPIC(MSG_CGC_GENERATION),

  DEBUG_TOPIC(MSG_JIT),
  DEBUG_TOPIC(MSG_JIT_DELINDEX),
  DEBUG_TOPIC(MSG_JIT_POOR),
  DEBUG_TOPIC(MSG_JIT_PRIMARY),

  DEBUG_TOPIC(MSG_RECONSULT),
  DEBUG_TOPIC(MSG_RECONSULT_PRED),
  DEBUG_TOPIC(MSG_RECONSULT_CLAUSE),
  DEBUG_TOPIC(MSG_RECONSULT_MODULE),
  DEBUG_TOPIC(MSG_AUTOLOAD),

  DEBUG_TOPIC(MSG_TRIE_PUT_TERM),
  DEBUG_TOPIC(MSG_TRIE_GC),
  DEBUG_TOPIC(MSG_TRIE_GEN),
  DEBUG_TOPIC(MSG_TRIE_VM),

  DEBUG_TOPIC(MSG_THREAD_LOCAL),

  DEBUG_TOPIC(MSG_ZIP),
  DEBUG_TOPIC(MSG_ZIP_STATE),
  DEBUG_TOPIC(MSG_WIN_API),
  DEBUG_TOPIC(MSG_WIN_DDE),

  DEBUG_TOPIC(MSG_MODULE_REF),

  DEBUG_TOPIC(MSG_STRING_BUFFER),

  DEBUG_TOPIC(MSG_TABLING_WORK),
  DEBUG_TOPIC(MSG_TABLING_MODED),
  DEBUG_TOPIC(MSG_TABLING_NEG),
  DEBUG_TOPIC(MSG_TABLING_SIMPLIFY),
  DEBUG_TOPIC(MSG_TABLING_DELAY),
  DEBUG_TOPIC(MSG_TABLING_DELAY_VAR),
  DEBUG_TOPIC(MSG_TABLING_AC),
  DEBUG_TOPIC(MSG_TABLING_MERGE),
  DEBUG_TOPIC(MSG_TABLING_VTRIE_DEPENDENCIES),
  DEBUG_TOPIC(MSG_TABLING_IDG),
  DEBUG_TOPIC(MSG_TABLING_IDG_CHANGED),
  DEBUG_TOPIC(MSG_TABLING_IDG_REEVAL),
  DEBUG_TOPIC(MSG_TABLING_EXCEPTION),
  DEBUG_TOPIC(MSG_TABLING_SHARED),
  DEBUG_TOPIC(MSG_TABLING_ABOLISH),
  DEBUG_TOPIC(MSG_TABLING_CALL_SUBSUMPTION),
  DEBUG_TOPIC(MSG_TABLING_RESTRAINT),
  DEBUG_TOPIC(MSG_TABLING_MONOTONIC),
  DEBUG_TOPIC(MSG_TABLING_TRANSACTION),
  DEBUG_TOPIC(MSG_TABLING_IDG_REEVAL_NODE),

  DEBUG_TOPIC(TABLING_NO_EARLY_COMPLETION),
  DEBUG_TOPIC(TABLING_NO_SIMPLIFY),
  DEBUG_TOPIC(TABLING_NO_AC),

  DEBUG_TOPIC(MSG_GVAR_LAZY),

  DEBUG_TOPIC(MSG_THREAD_WAIT),
  DEBUG_TOPIC(MSG_THREAD_SIGNAL),

  DEBUG_TOPIC(MSG_TRANSACTION),
  DEBUG_TOPIC(MSG_COMMIT),

  DEBUG_TOPIC(MSG_READ_OP),

  DEBUG_TOPIC(MSG_WRITE_FLOAT),

  DEBUG_TOPIC(MSG_YIELD),
  DEBUG_TOPIC(MSG_WASM_ASYNC),

  DEBUG_TOPIC(CHK_SECURE),
  DEBUG_TOPIC(CHK_HIGH_ARITY),
  DEBUG_TOPIC(CHK_HIGHER_ADDRESS),
  DEBUG_TOPIC(CHK_ATOM_GARBAGE_COLLECTED),
						/* end-of-list */
  { 0, NULL }
};


#if defined(__WINDOWS__) && !defined(strncasecmp)
#define strncasecmp(s1, s2, n) _strnicmp(s1, s2, n)
#endif

static const debug_topic *
get_next_debug_topic(const char *topic, const debug_topic *dt)
{ int cmplen = strlen(topic) + 1;
  if (cmplen > 1 && topic[cmplen-2] == '*')
    cmplen -= 2;
  for (dt = dt ? dt + 1 : debug_topics; dt->name; dt++)
  { if ( strncasecmp(topic, dt->name, cmplen) == 0 )
    { return dt;
    }
  }

  return NULL;
}


static unsigned
debug_high_code(void)
{ unsigned high = 0;
  const debug_topic *dt;

  for (dt=debug_topics; dt->name; dt++)
  { if ( dt->code > high )
      high = dt->code;
  }

  return high;
}


static int
prolog_debug_topic(const char *topic, int flag)
{ long level;
  char *end;
  int success = false;

  level = strtol(topic, &end, 10);
  if ( end > topic && *end == EOS )
  { GD->debug_level = level;
    success = true;
  } else
  { const debug_topic *dt = NULL;

    if ( !GD->debug_topics )
      GD->debug_topics = new_bitvector(debug_high_code()+1);

    if (topic[0] == '^')
    { topic++;
      flag = !flag;
    }

    while ( (dt = get_next_debug_topic(topic, dt)) )
    { int code = dt->code;
      success = true;

      if ( code <= DBG_LEVEL9 )
	GD->debug_level = code;
      else if (flag)
	set_bit(GD->debug_topics, code);
      else
	clear_bit(GD->debug_topics, code);
    }
  }

  return success;
}

const char *
prolog_debug_topic_name(unsigned code)
{ static unsigned last_code = -1;
  static const char *last_name = NULL;
  const debug_topic *dt;

  if (code == last_code)
    return last_name;

  last_code = code;

  for (dt=debug_topics; dt->name; dt++)
  { if ( dt->code == code )
      return last_name = dt->name;
  }

  Sdprintf("ERROR: Unknown debug code: %d\n", code);
  return last_name = NULL;
}


int
prolog_debug_from_string(const char *spec, int flag)
{ const char *end;
  bool quiet = (flag < 0);
  if (quiet)
    flag = ~flag;

  while((end=strchr(spec, ',')))
  { if ( end-spec < MAX_TOPIC_LEN )
    { char buf[MAX_TOPIC_LEN];

      strncpy(buf, spec, end-spec);
      buf[end-spec] = EOS;
      if ( !prolog_debug_topic(buf, flag) )
      { if (quiet)
	  return false;
	Sdprintf("ERROR: Unknown debug topic: %s\n", buf);
	PL_halt(1);
      }

      spec = end+1;
    } else if (!quiet)
    { Sdprintf("ERROR: Invalid debug topic: %s\n", spec);
    }
  }

  if ( !prolog_debug_topic(spec, flag) )
  { if (quiet)
      return false;
    Sdprintf("ERROR: Unknown debug topic: %s\n", spec);
    PL_halt(1);
  }

  return true;
}


int
PL_prolog_debug(const char *topic)
{ return prolog_debug_topic(topic, true);
}


int
PL_prolog_nodebug(const char *topic)
{ return prolog_debug_topic(topic, false);
}


static int
prolog_debug(term_t t, int flag)
{ char *topic;

  /* FIXME: handle lists */
  if ( !PL_get_chars(t, &topic, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
    fail;

  if ( prolog_debug_topic(topic, flag) )
    return true;

  return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_debug_topic, t);
}


static
PRED_IMPL("prolog_debug", 1, pl_prolog_debug, 0)
{ return prolog_debug(A1, true);
}


static
PRED_IMPL("prolog_nodebug", 1, pl_prolog_nodebug, 0)
{ return prolog_debug(A1, false);
}


void
cleanupDebug(void)
{ if ( GD->debug_topics )
  { free_bitvector(GD->debug_topics);
    GD->debug_topics = NULL;
  }
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(debug)
  PRED_DEF("prolog_debug", 1, pl_prolog_debug, 0)
  PRED_DEF("prolog_nodebug", 1, pl_prolog_nodebug, 0)
EndPredDefs
