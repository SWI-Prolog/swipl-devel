/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


#include <h/kernel.h>

#undef DEBUG				/* only if needed on this module */
#define DEBUG(t, g)

static struct to_cell AnswerStackBaseCell;

void
pushAnswerObject(Any obj)
{ if ( isVirginObj(obj) )
  { ToCell c = alloc(sizeof(struct to_cell));

    DEBUG(NAME_gc, Cprintf("pushAnswerObject(%s)\n", pp(obj)));
    setAnswerObj(obj);
    c->value = obj;
    c->index = AnswerStack->index + 1;
    c->next  = AnswerStack;
    AnswerStack = c;
  }
}


void
deleteAnswerObject(Any obj)
{ if ( isAnswerObj(obj) )
  { ToCell c;

    DEBUG(NAME_gc, Cprintf("deleteAnswerObject(%s)\n", pp(obj)));

    c = AnswerStack;

    if ( c->value == obj )
    { AnswerStack = c->next;
      unalloc(sizeof(struct to_cell), c);
    } else
    { ToCell p = c;

      for( c = c->next; c; p = c, c = c->next)
      { if ( c->value == obj )
	{ p->next = c->next;
	  unalloc(sizeof(struct to_cell), c);
	  break;				/* can only be there once! */
	}
      }
    }

    clearAnswerObj(obj);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: deletion of the  head  is  avoided   to  ensure  this  routine  is
reentrant. This may be  necessary  if   unlinking  an  object causes new
mark/rewind actions. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

export void
_rewindAnswerStack(AnswerMark *mark, Any obj)
{ ToCell c = AnswerStack;
  long index = *mark;

  if ( c->index > index )
  { ToCell n, preserve = NULL;
    int freehead = FALSE;
    
    for( ; c->index > index; c = n )
    { n = c->next;
      DEBUG(NAME_gc, Cprintf("Cell at 0x%lx\n", (unsigned long)c));
      if ( c->value )
      { if ( c->value != obj )
	{ Any o = c->value;
  
	  if ( noRefsObj(o) && !onFlag(o, F_LOCKED|F_PROTECTED) )
	  { DEBUG(NAME_gc, 
		  Cprintf("Removing %s from AnswerStack\n", pp(o)));
	    clearAnswerObj(c->value);
	    freeObject(o);
	  }
	  if ( c != AnswerStack )
	    unalloc(sizeof(struct to_cell), c);
	  else
	    freehead = TRUE;
	} else
	  preserve = c;
      } else
      { if ( c != AnswerStack )
	  unalloc(sizeof(struct to_cell), c);
	else
	  freehead = TRUE;
      }
    }
    
    if ( freehead )
      unalloc(sizeof(struct to_cell), AnswerStack);
  
    AnswerStack = c;
  
    if ( preserve )
    { preserve->next  = AnswerStack;
      preserve->index = AnswerStack->index + 1;
      AnswerStack     = preserve;
    }
  }
}


void
initAnswerStack(void)
{ AnswerStack = &AnswerStackBaseCell;
  AnswerStack->index = 1;
  AnswerStack->value = 0;
  AnswerStack->next  = NULL;
}


void
resetAnswerStack(void)
{ ToCell c, n;

  for( c = AnswerStack; c != &AnswerStackBaseCell; c = n )
  { if ( c->value )
    { clearAnswerObj(c->value);
      /*freeableObj(c->value);		May be handy to leave the
					object for debugging */
    }
    n = c->next;
    unalloc(sizeof(struct to_cell), c);
  }

  initAnswerStack();
}


Int
countAnswerStack()
{ ToCell c;
  int n = 0;

  for( c = AnswerStack; c != &AnswerStackBaseCell; c = c->next )
    n++;

  answer(toInt(n));
}
