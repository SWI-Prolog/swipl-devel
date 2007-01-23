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
#include <h/text.h>

static void	resetUndoBuffer(UndoBuffer ub);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  TEXTBUFFER UNDO

This module implements multi-action undo  for textbuffers.  Only three
basic operations are  defined on textbuffers: deleting, inserting  and
change.  For any of these, a record is made that describes the change.
For   insertions,  this just  defines the  place   and the size.   For
deletions  and changes the  old text  is stored  as well.  Undo simply
implies to walk back this change and perform the inverse operations.

The records are stored in an `undo buffer'.  This buffer operates as a
cycle.  New records  are appended to the end.   If the buffer is full,
old records are destroyed at  the start,  just until enough  space  is
available to  store  the new record.   The  elements are linked  in  a
double  linked chain.   The  backward links are used  to   perform the
incremental  undo's, the forward  links are used  to  destroy old undo
records.  The head and tail members point to the  last inserted, resp.
oldest cell  in the chain.  The free  member points ahead  of the head
and describes where to allocate  the next cell.  The current describes
the  cell to be undone on  the next ->undo  message.  While performing
undos this pointer goes backwards in the list.  On any other operation
it is placed at the head of the list.

Finally, the checkpoint member describes  the head cell  at the moment
->modified @off was send to the textbuffer  the last time.  If undoing
hits this cell it will again mark the buffer as not modified.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct undo_cell	* UndoCell;
typedef struct undo_delete	* UndoDelete;
typedef struct undo_insert	* UndoInsert;
typedef struct undo_change	* UndoChange;

#define UNDO_DELETE 0		/* action types */
#define UNDO_INSERT 1
#define UNDO_CHANGE 2

#define NOCHECKPOINT		((UndoCell) -1)

#define COMMON_CELL \
  UndoCell	previous;	/* previous cell */ \
  UndoCell	next;		/* next in chain */ \
  unsigned int	size;		/* size in chars */ \
  char		marked;		/* marked as interactive cell */ \
  char		type;		/* type of action */

struct undo_cell
{ COMMON_CELL
};

struct undo_delete
{ COMMON_CELL
  int		iswide;
  long		where;		/* start address of delete */
  long		len;		/* number of characters deleted there */
  union
  { charA	A[1];		/* ISO Latin-1 text */
    charW	W[1];		/* Wide character text */
  } text;
};
  
struct undo_insert
{ COMMON_CELL
  long		where;		/* start address of insert */
  long		len;		/* number of characters inserted there */
};
  
struct undo_change
{ COMMON_CELL
  int		iswide;
  long		where;		/* start of change */
  long		len;		/* length of change */
  union
  { charA	A[1];		/* ISO Latin-1 text */
    charW	W[1];		/* Wide character text */
  } text;
};

struct undo_buffer
{ TextBuffer	client;		/* so we know whom to talk to */
  unsigned	size;		/* size of buffer in chars */
  int		undone;		/* last action was an undo */
  int		aborted;	/* sequence was too big, aborted */
  UndoCell	current;	/* current undo cell for undos */
  UndoCell	checkpoint;	/* non-modified checkpoint */
  UndoCell	lastmark;	/* last marked cell */
  UndoCell	head;		/* first cell */
  UndoCell	tail;		/* last cell */
  UndoCell	free;		/* allocate next one here */
  UndoCell	buffer;		/* buffer storage */
};

#define istbA(tb)		((tb)->buffer.iswide == 0)
#define Round(n, r)		(((n)+(r)-1) & (~((r)-1)))
#define AllocRound(s)		Round(s, sizeof(UndoCell))
#define Distance(p1, p2)	((char *)(p1) - (char *)(p2))
#define SizeAfter(ub, size)	((size) <= ub->size - \
				           Distance(ub->free, ub->buffer))
#define UndoDeleteSize(len) ((unsigned)(long) &((UndoDelete)NULL)->text.A[len])
#define UndoChangeSize(len) ((unsigned)(long) &((UndoChange)NULL)->text.A[len])


static UndoBuffer
createUndoBuffer(long size)
{ UndoBuffer ub = alloc(sizeof(struct undo_buffer));

  ub->size    = AllocRound(size);
  ub->buffer  = alloc(ub->size);
  ub->aborted = FALSE;
  ub->client  = NIL;
  resetUndoBuffer(ub);

  return ub;
}


static void
resetUndoBuffer(UndoBuffer ub)
{ ub->current = ub->lastmark = ub->head = ub->tail = NULL;
  ub->checkpoint = NOCHECKPOINT;
  ub->free = ub->buffer;
}


void
destroyUndoBuffer(UndoBuffer ub)
{ if ( ub->buffer != NULL )
  { unalloc(ub->size, ub->buffer);
    ub->buffer = NULL;
  }

  unalloc(sizeof(struct undo_buffer), ub);
}



Int
getUndoTextBuffer(TextBuffer tb)
{ long caret = -1;

  if ( tb->undo_buffer != NULL )
  { UndoBuffer ub = tb->undo_buffer;
    UndoCell cell;

    if ( (cell = ub->current) == NULL )	/* No further undo's */
      fail;
      
    while(cell != NULL)
    { DEBUG(NAME_undo, Cprintf("Undo using cell %d: ",
			       Distance(cell, ub->buffer)));
      switch( cell->type )
      { case UNDO_DELETE:
	{ UndoDelete d = (UndoDelete) cell;
	  string s;
	  
	  s.size = d->len;
	  s.iswide = d->iswide;
	  if ( d->iswide )
	    s.s_textA = d->text.A;
	  else
	    s.s_textW = d->text.W;

	  DEBUG(NAME_undo, Cprintf("Undo delete at %ld, len=%ld\n",
				   d->where, d->len));
	  insert_textbuffer(tb, d->where, 1, &s);
	  caret = max(caret, d->where + d->len);
	  break;
	}
	case UNDO_INSERT:
	{ UndoInsert i = (UndoInsert) cell;
	  DEBUG(NAME_undo, Cprintf("Undo insert at %ld, len=%ld\n",
				   i->where, i->len));
	  delete_textbuffer(tb, i->where, i->len);
	  caret = max(caret, i->where);
	  break;
	}
	case UNDO_CHANGE:
	{ UndoChange c = (UndoChange) cell;
	  string s;

	  s.size = c->len;
	  s.iswide = c->iswide;
	  if ( c->iswide )
	    s.s_textA = c->text.A;
	  else
	    s.s_textW = c->text.W;

	  DEBUG(NAME_undo, Cprintf("Undo change at %ld, len=%ld\n",
				   c->where, c->len));

	  change_textbuffer(tb, c->where, &s);
	  caret = max(caret, c->where + c->len);
	  break;
 	}
      }

      cell = cell->previous;
      if ( cell == NULL || cell->marked == TRUE )
      {	ub->current = cell;

	if ( cell == ub->checkpoint )	/* reached non-modified checkpoint */
	{ DEBUG(NAME_undo, Cprintf("Reset modified to @off\n"));
	  CmodifiedTextBuffer(tb, OFF);
	}

        changedTextBuffer(tb);
	ub->undone = TRUE;

	answer(toInt(caret));
      }
    }
  }

  fail;
}


status
undoTextBuffer(TextBuffer tb)
{ return getUndoTextBuffer(tb) ? SUCCEED : FAIL;
}


static UndoBuffer
getUndoBufferTextBuffer(TextBuffer tb)
{ if ( tb->undo_buffer != NULL )
    return tb->undo_buffer;

  if ( isDefault(tb->undo_buffer_size) )
    assign(tb, undo_buffer_size,
	   getClassVariableValueObject(tb, NAME_undoBufferSize));

  if ( tb->undo_buffer_size != ZERO )
  { tb->undo_buffer = createUndoBuffer(valInt(tb->undo_buffer_size));
    tb->undo_buffer->client = tb;
  }
    
  return tb->undo_buffer;
}


status
undoBufferSizeTextBuffer(TextBuffer tb, Int size)
{ if ( tb->undo_buffer_size != size )
  { if ( tb->undo_buffer != NULL )
    { destroyUndoBuffer(tb->undo_buffer);
      tb->undo_buffer = NULL;
    }
    
    assign(tb, undo_buffer_size, size);
  }

  succeed;
}


status
markUndoTextBuffer(TextBuffer tb)
{ UndoBuffer ub;

  if ( (ub = getUndoBufferTextBuffer(tb)) )
  { DEBUG(NAME_undo, Cprintf("markUndoTextBuffer(%s)\n", pp(tb)));

    if ( ub->head )
    { ub->head->marked = TRUE;
      ub->lastmark = ub->head;
    }

    if ( ub->undone == FALSE )
      ub->current = ub->head;

    ub->undone = FALSE;
    ub->aborted = FALSE;
  }
    
  succeed;
}


status
resetUndoTextBuffer(TextBuffer tb)
{ if ( tb->undo_buffer != NULL )
  { destroyUndoBuffer(tb->undo_buffer);
    tb->undo_buffer = NULL;
  }

  succeed;
}


status
checkpointUndoTextBuffer(TextBuffer tb)
{ UndoBuffer ub;

  if ( (ub = getUndoBufferTextBuffer(tb)) )
    ub->checkpoint = ub->head;
  
  succeed;
}


static void
destroy_oldest_undo(UndoBuffer ub)
{ if ( ub->tail != NULL )
    ub->tail->marked = FALSE;

  while( ub->tail != NULL && ub->tail->marked == FALSE )
  { if ( ub->tail == ub->current )
      ub->current = NULL;
    if ( ub->tail == ub->checkpoint )
      ub->checkpoint = NOCHECKPOINT;
    if ( ub->tail == ub->head )
    { resetUndoBuffer(ub);
      return;
    }
    if ( ub->tail->next )
      ub->tail->next->previous = NULL;
    ub->tail = ub->tail->next;
  }
  
  if ( ub->tail == NULL )
    resetUndoBuffer(ub);
}
    

		/********************************
		*           ALLOCATION          *
		*********************************/

static int
Between(UndoBuffer ub, UndoCell new, UndoCell old)
{ if ( new > old )
    return Distance(new, old);

  return ub->size - Distance(old, new);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate a new undo size with the requested size in bytes
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void *
new_undo_cell(UndoBuffer ub, unsigned int size)
{ UndoCell new;

  if ( ub->aborted )
    return NULL;

  size = AllocRound(size);
  
  if ( size > ub->size/2 )		/* Too big: destroy all */
  { errorPce(ub->client, NAME_undoOverflow);

    ub->aborted = TRUE;
    resetUndoBuffer(ub);
    return NULL;
  }

  for(;;)
  { if ( ub->head == NULL )		/* Empty: new cell is head & tail */
      break;

    if ( ub->tail < ub->free )
    { if ( SizeAfter(ub, size) )
	break;
      ub->free = ub->buffer;
    } else if ( (int)size <= Distance(ub->tail, ub->free) )
      break;

    destroy_oldest_undo(ub);
  }

  if ( ub->lastmark && Between(ub, ub->free, ub->lastmark) >= (int)ub->size/2 )
  { errorPce(ub->client, NAME_undoOverflow);

    ub->aborted = TRUE;
    resetUndoBuffer(ub);
    return NULL;
  }

  new = ub->free;
  new->size = size;
  new->marked = FALSE;
  new->next = NULL;
  new->previous = ub->head;
  if ( ub->head == NULL )		/* empty */
  { ub->tail = new;
    ub->lastmark = new;
  } else
    ub->head->next = new;
  ub->head = new;
  ub->free = (UndoCell) ((char *)new + size);

  DEBUG(NAME_undo, Cprintf("Cell at %d size=%d: ",
			   Distance(new, ub->buffer), new->size));

  return new;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resize the current undo-cell to be at least <size> characters.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
resize_undo_cell(UndoBuffer ub, UndoCell cell, unsigned int size)
{ size = AllocRound(size);
  assert(cell == ub->head);

  if ( cell->size == size )
    return TRUE;

  while( ub->tail > cell && (int)size > Distance(ub->tail, cell) && ub->head )
    destroy_oldest_undo(ub);

  if ( ub->head &&
       ((ub->tail > cell && (int)size < Distance(ub->tail, cell)) ||
	(ub->tail < cell && SizeAfter(ub, size))) )
  { cell->size = size;
    ub->free = (UndoCell) ((char *) cell + size);

    DEBUG(NAME_undo, Cprintf("Resized cell at %d size=%d\n",
			     Distance(cell, ub->buffer), cell->size));
    return TRUE;
  }
  
  DEBUG(NAME_undo,
	if ( !ub->head )
	  Cprintf("**** UNDO buffer overflow ****\n");
	else
	  Cprintf("**** UNDO buffer circle ****\n"));

  return FALSE;
}


void
register_insert_textbuffer(TextBuffer tb, long int where, long int len)
{ UndoBuffer ub;

  if ( len > 0 && (ub = getUndoBufferTextBuffer(tb)) != NULL )
  { UndoInsert i = (UndoInsert) ub->head;

    if ( i != NULL && i->type == UNDO_INSERT && i->marked == FALSE )
    { if ( i->where + i->len == where || where + len == i->where )
      { i->len += len;
	DEBUG(NAME_undo, Cprintf("Insert at %ld grown %ld bytes\n",
				 i->where, i->len));
        return;
      }
    }

    if ( (i = new_undo_cell(ub, sizeof(struct undo_insert))) == NULL )
      return;

    i->type =  UNDO_INSERT;
    i->where = where;
    i->len  = len;
    DEBUG(NAME_undo, Cprintf("New Insert at %ld, %ld bytes\n",
			     i->where, i->len));
  }
}


static void
copy_undo_del(TextBuffer tb, long from, long len, UndoDelete udc, long offset)
{ if ( udc->iswide )
  { charW *to = &udc->text.W[offset];

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  } else
  { charA *to = &udc->text.A[offset];

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  }
}


void
register_delete_textbuffer(TextBuffer tb, long where, long len)
{ UndoBuffer ub;
  long i;
  int need_wide = FALSE;
  int cell_size;

  for(i=where; i<where+len; i++)
  { wint_t c = fetch_textbuffer(tb, i);

    if ( tisendsline(tb->syntax, c) )
      tb->lines--;
    
    if ( c > 0xff )
      need_wide = TRUE;
  }

  if ( len > 0 && (ub = getUndoBufferTextBuffer(tb)) != NULL )
  { UndoDelete udc = (UndoDelete) ub->head;

    if ( udc != NULL && udc->type == UNDO_DELETE &&
	 udc->marked == FALSE &&
	 tb->buffer.iswide == udc->iswide )
    { if ( where == udc->where )	/* forward delete */
      { cell_size = len+udc->len;
	if ( udc->iswide )
	  cell_size *= sizeof(charW);

	if ( resize_undo_cell(ub, (UndoCell)udc, UndoDeleteSize(cell_size)) )
	{ copy_undo_del(tb, where, len, udc, udc->len);
	  udc->len += len;
	  DEBUG(NAME_undo, Cprintf("Delete at %ld grown forward %ld bytes\n",
				   udc->where, udc->len));
	}

        return;
      }

      if ( where + len == udc->where )	/* backward delete */
      { cell_size = len+udc->len;
	if ( udc->iswide )
	  cell_size *= sizeof(charW);

	if ( resize_undo_cell(ub, (UndoCell) udc, UndoDeleteSize(cell_size)) )
	{ if ( udc->iswide )
	  { memmove(&udc->text.W[len], &udc->text.W, udc->len*sizeof(charW));
	  } else
	  { memmove(&udc->text.A[len], &udc->text.A, udc->len);
	  }

	  copy_undo_del(tb, where, len, udc, 0);
	  udc->len += len;
	  udc->where -= len;
	  DEBUG(NAME_undo, Cprintf("Delete at %ld grown backward %ld bytes\n",
				   udc->where, udc->len));
	}
	return;
      }
    }

    cell_size = need_wide ? len*(int)sizeof(charW) : len;
    if ( (udc = new_undo_cell(ub, UndoDeleteSize(cell_size))) == NULL )
      return;
    udc->type   = UNDO_DELETE;
    udc->where  = where;
    udc->len    = len;
    udc->iswide = need_wide;
    copy_undo_del(tb, where, len, udc, 0);
    DEBUG(NAME_undo, Cprintf("New delete at %ld, %ld bytes\n",
			     udc->where, udc->len));
  }
}


static void
copy_undo_chg(TextBuffer tb, long from, long len, UndoChange uc, long offset)
{ if ( uc->iswide )
  { charW *to = &uc->text.W[offset];

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  } else
  { charA *to = &uc->text.A[offset];

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  }
}


void
register_change_textbuffer(TextBuffer tb, long int where, long int len)
{ UndoBuffer ub;
  int need_wide = FALSE;
  int cell_size;
  long i;

  for(i=where; i<where+len; i++)
  { wint_t c = fetch_textbuffer(tb, i);

    if ( c > 0xff )
      need_wide = TRUE;
  }

  if ( len > 0 && (ub = getUndoBufferTextBuffer(tb)) != NULL )
  { UndoChange uc = (UndoChange) ub->head;

    if ( uc != NULL && uc->type == UNDO_CHANGE &&
	 uc->marked == FALSE &&
	 tb->buffer.iswide == uc->iswide )
    { if ( where == uc->where + uc->len )	/* forward change */
      {	cell_size = len+uc->len;
	if ( uc->iswide )
	  cell_size *= sizeof(charW);

	if ( resize_undo_cell(ub, (UndoCell)uc,
			      UndoChangeSize(cell_size)) )
	{ copy_undo_chg(tb, where, len, uc, uc->len);
      
	  uc->len += len;
	  DEBUG(NAME_undo,
		Cprintf("Change at %ld grown forward to %ld bytes\n",
			uc->where, uc->len));
	}
	return;
      }

      if ( where + len == uc->where )		/* backward change */
      { cell_size = len+uc->len;
	if ( uc->iswide )
	  cell_size *= sizeof(charW);

	if ( resize_undo_cell(ub, (UndoCell)uc,
			      UndoChangeSize(cell_size)) )
	{ if ( uc->iswide )
	  { memmove(&uc->text.W[len], &uc->text.W, uc->len*sizeof(charW));
	  } else
	  { memmove(&uc->text.A[len], &uc->text.A, uc->len);
	  }

	  copy_undo_chg(tb, where, len, uc, 0);
	  uc->len += len;
	  uc->where -= len;
	  DEBUG(NAME_undo,
		Cprintf("Change at %ld grown backward to %ld bytes\n",
			uc->where, uc->len));
	}
	return;
      }
    }

    cell_size = need_wide ? len*(int)sizeof(charW) : len;
    if ( (uc = new_undo_cell(ub, UndoChangeSize(cell_size))) == NULL )
      return;
    uc->type   = UNDO_CHANGE;
    uc->where  = where;
    uc->len    = len;
    uc->iswide = need_wide;
    copy_undo_chg(tb, where, len, uc, 0);
    DEBUG(NAME_undo, Cprintf("New change at %ld, %ld bytes\n",
			     uc->where, uc->len));
  }
}
