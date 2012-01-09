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

  DEBUG_TOPIC(CHK_SECURE),
						/* end-of-list */
  { 0, NULL }
};


static int
get_debug_code(char *topic)
{ const debug_topic *dt;

  for (dt=debug_topics; dt->name; dt++)
  { if ( strcasecmp(topic, dt->name) == 0 )
    { return dt->code;
    }
  }

  return -1;
}


static int
prolog_debug(term_t t, int flag)
{ GET_LD
  char *topic;
  int code;

  /* FIXME: handle lists */
  if( !PL_get_chars(t, &topic, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
    fail;

  if ( !GD->debug_topics )
    GD->debug_topics = new_bitvector(sizeof(debug_topics)/sizeof(*debug_topics));

  if( (code = get_debug_code(topic)) < 0 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_debug_topic, t);

  if ( code <= DBG_LEVEL9 )
    GD->debug_level = code;
  else if (flag)
    set_bit(GD->debug_topics, code);
  else
    clear_bit(GD->debug_topics, code);

  return TRUE;
}


static
PRED_IMPL("prolog_debug", 1, pl_prolog_debug, 0)
{ return prolog_debug(A1, 1);
}


static
PRED_IMPL("prolog_nodebug", 1, pl_prolog_nodebug, 0)
{ return prolog_debug(A1, 0);
}


void
cleanupDebug(void)
{ if ( GD->debug_topics )
  { GET_LD

    free_bitvector(GD->debug_topics);
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

