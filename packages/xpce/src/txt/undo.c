/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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
  int		size;		/* size in words */ \
  char		marked;		/* marked as interactive cell */ \
  char		type;		/* type of action */

struct undo_cell
{ COMMON_CELL
};

struct undo_delete
{ COMMON_CELL
  long		where;		/* start address of delete */
  long		len;		/* number of characters deleted there */
  char		chars[1];	/* deleted characters */
};
  
struct undo_insert
{ COMMON_CELL
  long		where;		/* start address of insert */
  long		len;		/* number of characters inserted there */
};
  
struct undo_change
{ COMMON_CELL
  long		where;		/* start of change */
  long		len;		/* length of change */
  char		chars[1];	/* changed characters */
};

struct undo_buffer
{ unsigned	size;		/* size of buffer in chars */
  int		b16;		/* 16- rather than 8-bit characters */
  int		undone;		/* last action was an undo */
  UndoCell	current;	/* current undo cell for undos */
  UndoCell	checkpoint;	/* non-modified checkpoint */
  UndoCell	head;		/* first cell */
  UndoCell	tail;		/* last cell */
  UndoCell	free;		/* allocate next one here */
  UndoCell	buffer;		/* buffer storage */
};

#define istb8(tb)		((tb)->buffer.b16 == 0)
#define Round(n, r)		((n)%(r) == 0 ? (n) : ((n)|(r-1)) + 1)
#define AllocRound(s)		Round(s, sizeof(UndoCell))
#define Distance(p1, p2)	((char *)(p1) - (char *)(p2))
#define SizeAfter(ub, size)	((size) <= ub->size - \
				           Distance(ub->free, ub->buffer))
#define Address(b, c, i)	((b)->b16 ? &(c)->chars[(i)*2] \
					  : &(c)->chars[(i)])
#define Chars(b, l)		((b)->b16 ? (l) * 2 : (l))
#define _UndoDeleteSize(len)    ((unsigned) &((UndoDelete)NULL)->chars[len])
#define _UndoChangeSize(len)    ((unsigned) &((UndoChange)NULL)->chars[len])
#define UndoDeleteSize(b, l)	_UndoDeleteSize((b)->b16 ? (l)*2 : (l))
#define UndoChangeSize(b, l)	_UndoChangeSize((b)->b16 ? (l)*2 : (l))


static UndoBuffer
createUndoBuffer(long int size, int b16)
{ UndoBuffer ub = alloc(sizeof(struct undo_buffer));

  ub->b16    = b16;
  ub->size   = AllocRound(b16 ? size*2 : size);
  ub->buffer = alloc(ub->size);
  resetUndoBuffer(ub);

  return ub;
}


static void
resetUndoBuffer(UndoBuffer ub)
{ ub->current = ub->head = ub->tail = NULL;
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
	  s.s_text = d->chars;
	  s.b16 = ub->b16;
	  s.encoding = ENC_ASCII;
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
	  DEBUG(NAME_undo, Cprintf("Undo change at %ld, len=%ld\n",
				   c->where, c->len));
	  change_textbuffer(tb, c->where, c->chars, c->len);
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
	   getResourceValueObject(tb, NAME_undoBufferSize));

  if ( tb->undo_buffer_size != ZERO )
    tb->undo_buffer = createUndoBuffer(valInt(tb->undo_buffer_size),
				       tb->buffer.b16);
    
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

  if ( (ub = getUndoBufferTextBuffer(tb)) != FAIL )
  { if ( ub->head )
      ub->head->marked = TRUE;

    if ( ub->undone == FALSE )
      ub->current = ub->head;

    ub->undone = FALSE;
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

static void *
new_undo_cell(UndoBuffer ub, unsigned int size)
{ UndoCell new;

  size = AllocRound(size);
  
  if ( size > ub->size )		/* Too big: destroy all */
  { resetUndoBuffer(ub);
    return NULL;
  }

  for(;;)
  { if ( ub->head == NULL )		/* Empty: new cell is head & tail */
      break;

    if ( ub->tail < ub->free )
    { if ( SizeAfter(ub, size) )
	break;
      ub->free = ub->buffer;
    } else if ( size <= Distance(ub->tail, ub->free) )
      break;

    destroy_oldest_undo(ub);
  }

  new = ub->free;
  new->size = size;
  new->next = NULL;
  new->previous = ub->head;
  if ( ub->head == NULL )
    ub->tail = new;
  else
    ub->head->next = new;
  ub->head = new;
  ub->free = (UndoCell) ((char *)new + size);

  DEBUG(NAME_undo, Cprintf("Cell at %d size=%d: ",
			   Distance(new, ub->buffer), new->size));

  return new;
}


static int
resize_undo_cell(UndoBuffer ub, UndoCell cell, unsigned int size)
{ size = AllocRound(size);

  if ( cell->size == size )
    return TRUE;

  while( ub->tail > cell && size > Distance(ub->tail, cell) )
    destroy_oldest_undo(ub);

  if ( size >= Distance(ub->tail, cell) || SizeAfter(ub, size) )
  { cell->size = size;
    ub->free = (UndoCell) ((char *) cell + size);

    DEBUG(NAME_undo, Cprintf("Resized cell at %d size=%d: ",
			     cell - ub->buffer, cell->size));
    return TRUE;
  }
  
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
copy_undo(TextBuffer tb, long int from, long int len, void *buf)
{ if ( istb8(tb) )
  { char8 *to = buf;

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  } else
  { char16 *to = buf;

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  }
}


void
register_delete_textbuffer(TextBuffer tb, long int where, long int len)
{ UndoBuffer ub;

  if ( len > 0 && (ub = getUndoBufferTextBuffer(tb)) != NULL )
  { UndoDelete i = (UndoDelete) ub->head;

    if ( i != NULL && i->type == UNDO_DELETE && i->marked == FALSE )
    { if ( where == i->where &&		/* forward delete */
	   resize_undo_cell(ub, (UndoCell)i, UndoDeleteSize(ub, len+i->len)) )
      { copy_undo(tb, where, len, Address(ub, i, len));
	i->len += len;
	DEBUG(NAME_undo, Cprintf("Delete at %ld grown forward %ld bytes\n",
				 i->where, i->len));
        return;
      }

      if ( where + len == i->where &&	/* backward delete */
           resize_undo_cell(ub, (UndoCell) i, UndoDeleteSize(ub, len+i->len)) )
      { memcpy(Address(ub, i, len), Address(ub, i, 0), Chars(ub, len));
        copy_undo(tb, where, len, Address(ub, i, 0));
	i->len += len;
	i->where -= len;
	DEBUG(NAME_undo, Cprintf("Delete at %ld grown backward %ld bytes\n",
				 i->where, i->len));
	return;
      }
    }

    if ( (i = new_undo_cell(ub, UndoDeleteSize(ub, len))) == NULL )
      return;
    i->type =  UNDO_DELETE;
    i->where = where;
    i->len  = len;
    copy_undo(tb, where, len, Address(ub, i, 0));
    DEBUG(NAME_undo, Cprintf("New delete at %ld, %ld bytes\n",
			     i->where, i->len));
  }
}


void
register_change_textbuffer(TextBuffer tb, long int where, long int len)
{ UndoBuffer ub;

  if ( len > 0 && (ub = getUndoBufferTextBuffer(tb)) != NULL )
  { UndoChange i = (UndoChange) ub->head;

    if ( i != NULL && i->type == UNDO_CHANGE && i->marked == FALSE )
    { if ( where == i->where + i->len &&	/* forward change */
	   resize_undo_cell(ub, (UndoCell)i, UndoChangeSize(ub, len+i->len)) )
      { copy_undo(tb, where, len, Address(ub, i, len));
	i->len += len;
	DEBUG(NAME_undo, Cprintf("Change at %ld grown forward to %ld bytes\n",
				 i->where, i->len));
        return;
      }

      if ( where + len == i->where &&		/* backward change */
           resize_undo_cell(ub, (UndoCell)i, UndoChangeSize(ub, len+i->len)) )
      { memcpy(Address(ub, i, len), Address(ub, i, 0), Chars(ub, len));
        copy_undo(tb, where, len, Address(ub, i, 0));
	i->len += len;
	i->where -= len;
	DEBUG(NAME_undo, Cprintf("Change at %ld grown backward to %ld bytes\n",
				 i->where, i->len));
	return;
      }
    }

    if ( (i = new_undo_cell(ub, UndoChangeSize(ub, len))) == NULL )
      return;
    i->type  = UNDO_CHANGE;
    i->where = where;
    i->len   = len;
    copy_undo(tb, where, len, Address(ub, i, 0));
    DEBUG(NAME_undo, Cprintf("New change at %ld, %ld bytes\n",
			     i->where, i->len));
  }
}
