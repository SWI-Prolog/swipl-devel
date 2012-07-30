/*  $Id$

    Part of SWI-Prolog

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
  DEBUG_TOPIC(MSG_SRCLOC),
  DEBUG_TOPIC(MSG_PROC),
  DEBUG_TOPIC(MSG_COMP_ARGVAR),
  DEBUG_TOPIC(MSG_UNLOAD),
  DEBUG_TOPIC(MSG_INDEX_FIND),
  DEBUG_TOPIC(MSG_INDEX_UPDATE),
  DEBUG_TOPIC(MSG_JIT),
  DEBUG_TOPIC(MSG_TRACE),
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
  DEBUG_TOPIC(MSG_SHIFT_PROGRESS),
  DEBUG_TOPIC(MSG_SHIFT_POINTER),
  DEBUG_TOPIC(MSG_SHIFT_FRAME),
  DEBUG_TOPIC(MSG_STACK_OVERFLOW),

  DEBUG_TOPIC(CHK_SECURE),
						/* end-of-list */
  { 0, NULL }
};


static int
get_debug_code(const char *topic)
{ const debug_topic *dt;

  for (dt=debug_topics; dt->name; dt++)
  { if ( strcasecmp(topic, dt->name) == 0 )
    { return dt->code;
    }
  }

  return -1;
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

  level = strtol(topic, &end, 10);
  if ( end > topic && *end == EOS )
  { GD->debug_level = level;
  } else
  { int code;

    if ( !GD->debug_topics )
      GD->debug_topics = new_bitvector(debug_high_code()+1);

    if( (code = get_debug_code(topic)) < 0 )
      return FALSE;

    if ( code <= DBG_LEVEL9 )
      GD->debug_level = code;
    else if (flag)
      set_bit(GD->debug_topics, code);
    else
      clear_bit(GD->debug_topics, code);
  }

  return TRUE;
}


int
prolog_debug_from_string(const char *spec, int flag)
{ const char *end;

  while((end=strchr(spec, ',')))
  { if ( end-spec < MAX_TOPIC_LEN )
    { char buf[MAX_TOPIC_LEN];

      strncpy(buf, spec, end-spec);
      buf[end-spec] = EOS;
      if ( !prolog_debug_topic(buf, flag) )
      { Sdprintf("ERROR: Unknown debug topic: %s\n", buf);
	PL_halt(1);
      }

      spec = end+1;
    } else
    { Sdprintf("ERROR: Invalid debug topic: %s\n", spec);
    }
  }

  if ( !prolog_debug_topic(spec, flag) )
  { Sdprintf("ERROR: Unknown debug topic: %s\n", spec);
    PL_halt(1);
  }

  return TRUE;
}


static int
prolog_debug(term_t t, int flag)
{ char *topic;

  /* FIXME: handle lists */
  if ( !PL_get_chars(t, &topic, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
    fail;

  if ( prolog_debug_topic(topic, flag) )
    return TRUE;

  return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_debug_topic, t);
}


static
PRED_IMPL("prolog_debug", 1, pl_prolog_debug, 0)
{ return prolog_debug(A1, TRUE);
}


static
PRED_IMPL("prolog_nodebug", 1, pl_prolog_nodebug, 0)
{ return prolog_debug(A1, FALSE);
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

