/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
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
{ PLEV_ABORT,				/* Execution aborted */
  PLEV_ERASED_CLAUSE,			/* clause was erased */
  PLEV_ERASED_RECORD,			/* record was erased */
  PLEV_DEBUGGING,			/* changed debugging mode */
  PLEV_TRACING,				/* changed tracing mode */
  PLEV_SPY,				/* changed spypoint */
  PLEV_BREAK,				/* a break-point was set */
  PLEV_BREAK_EXISTS,			/* existing breakpoint */
  PLEV_NOBREAK,				/* a break-point was cleared */
  PLEV_GCNOBREAK,			/* cleared due to clause GC */
  PLEV_FRAMEFINISHED,			/* A watched frame was discarded */
  PL_EV_THREADFINISHED			/* A thread has finished */
} pl_event_type;


COMMON(int)	delayEvents(void);
COMMON(int)	sendDelayedEvents(int noerror);
COMMON(int)	PL_call_event_hook(pl_event_type ev, ...);
COMMON(int)	PL_call_event_hook_va(pl_event_type ev, va_list args);

static inline int WUNUSED
callEventHook(pl_event_type ev, ...)
{ if ( PROCEDURE_event_hook1->definition->impl.any.defined )
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
