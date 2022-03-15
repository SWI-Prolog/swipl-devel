/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2020, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
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

#ifndef _PL_EVENT_H
#define _PL_EVENT_H

typedef enum pl_event_type
{ PLEV_ABORT = 0,			/* Execution aborted */
  PLEV_ERASED_CLAUSE,			/* clause was erased */
  PLEV_ERASED_RECORD,			/* record was erased */
  PLEV_BREAK,				/* a break-point was set */
  PLEV_BREAK_EXISTS,			/* existing breakpoint */
  PLEV_NOBREAK,				/* a break-point was cleared */
  PLEV_GCNOBREAK,			/* cleared due to clause GC */
  PLEV_FRAMEFINISHED,			/* A watched frame was discarded */
  PLEV_UNTABLE,				/* Stop tabling some predicate */
					/* Keep these two at the end */
  PLEV_THREAD_START,			/* A thread started */
  PLEV_THREAD_EXIT,			/* A thread has finished */
  PLEV_THIS_THREAD_EXIT			/* This thread has finished */
} pl_event_type;

typedef struct event_callback
{ atom_t		name;		/* Name of the callback */
  Module		 module;	/* context module */
  Procedure		 procedure;	/* procedure to use */
  int		       (*function)();	/* C-function */
  union
  { struct fastheap_term *term;		/* closure */
    void		 *pointer;	/* for C functions */
  } closure;
  int			 argc;		/* #context args */
  struct event_callback *next;		/* next in chain */
} event_callback;

typedef struct event_list
{ event_callback *head;			/* First event handler */
  event_callback *tail;			/* Last event handler */
#ifdef O_PLMT
  recursiveMutex  lock;			/* Access lock */
#endif
} event_list;

typedef struct event_type
{ pl_event_type id;
  atom_t	name;
  int		argc;
  unsigned      local : 1;
  event_list  **location;
} event_type;

#define P_EVENT_ROLLBACK	0x0001

#if USE_LD_MACROS
#define	predicate_update_event(def, action, cl, flags)	LDFUNC(predicate_update_event, def, action, cl, flags)
#define	table_answer_event(def, action, answer)		LDFUNC(table_answer_event, def, action, answer)
#define	retractall_event(def, head, start)		LDFUNC(retractall_event, def, head, start)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void	cleanupEvents(void);
int	delayEvents(void);
int	sendDelayedEvents(int noerror);
int	PL_call_event_hook(pl_event_type ev, ...);
int	PL_call_event_hook_va(pl_event_type ev, va_list args);
int	register_event_hook(event_list **list, atom_t name, int last,
			    term_t closure, int argc);
int	register_event_function(event_list **list, atom_t name, int last,
				int (*func)(), void *closure, int argc);
void	destroy_event_list(event_list **listp);
int	predicate_update_event(Definition def, atom_t action, Clause cl,
			       unsigned flags);
int	table_answer_event(Definition def, atom_t action,
			   term_t answer);
int	retractall_event(Definition def, term_t head, atom_t start);

#undef LDFUNC_DECLARATIONS

extern const event_type PL_events[PLEV_THIS_THREAD_EXIT+2];

static inline event_list**
event_list_location(pl_event_type ev)
{ if ( likely(!PL_events[ev].local) )
  { return PL_events[ev].location;
  } else
  { GET_LD
    return (event_list**)(((char*)LD) + (size_t)PL_events[ev].location);
  }
}


static inline int WUNUSED
callEventHook(pl_event_type ev, ...)
{ event_list **listp = event_list_location(ev);

  if ( *listp )
  { va_list args;
    int rc;

    va_start(args, ev);
    rc = PL_call_event_hook_va(ev, args);
    va_end(args);

    return rc;
  }

  return TRUE;
}

#endif /*_PL_EVENT_H*/
