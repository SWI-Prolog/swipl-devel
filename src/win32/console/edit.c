/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#define _MAKE_DLL 1
#undef _export
#include "console.h"
#include "history.h"
#include "common.h"
#include <memory.h>
#include <string.h>
#include <ctype.h>

typedef void (*function)(Line ln, int chr);	/* edit-function */

static function dispatch_table[256];	/* general dispatch-table */
static RlcCompleteFunc _rlc_complete_function = rlc_complete_file_function;

static void	init_line_package(void);
static void	bind_actions(void);

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef EOS
#define EOS 0
#endif

#ifndef ESC
#define ESC 27
#endif

#define COMPLETE_NEWLINE 1
#define COMPLETE_EOF	 2

#define ctrl(c)	((c) - '@')
#define meta(c) ((c) + 128)

		 /*******************************
		 *	       BUFFER		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
make_room(Line, int room)
	Make n-characters space after the point.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
make_room(Line ln, int room)
{ while ( ln->size + room + 1 > ln->allocated )
  { if ( !ln->data )
    { ln->data = rlc_malloc(256);
      ln->allocated = 256;
    } else
    { ln->allocated *= 2;
      ln->data = rlc_realloc(ln->data, ln->allocated);
    }
  }

  memmove(&ln->data[ln->point + room], &ln->data[ln->point],
	  ln->size - ln->point);
  ln->size += room;
  if ( room > 0 )
    ln->change_start = min(ln->change_start, ln->point);
}


static void
set_line(Line ln, const char *s)
{ int len = strlen(s);

  ln->size = ln->point = 0;
  make_room(ln, len);
  memcpy(ln->data, s, len);
}


static void
terminate(Line ln)
{ if ( !ln->data )
  { ln->data = rlc_malloc(1);
    ln->allocated = 1;
  }
  ln->data[ln->size] = EOS;
}


static void
delete(Line ln, int from, int len)
{ if ( from < 0 || from > ln->size || len < 0 || from + len > ln->size )
    return;

  memcpy(&ln->data[from], &ln->data[from+len], ln->size - (from+len));
  ln->size -= len;
} 


		 /*******************************
		 *	     POSITIONING	*
		 *******************************/

static int
back_word(Line ln, int from)
{ from = min(from, ln->size);
  from = max(0, from);

  if ( ln->data )
  { while(!rlc_is_word_char(ln->data[from-1]) && from > 0 )
      from--;
    while(rlc_is_word_char(ln->data[from-1]) && from > 0 )
      from--;
  }

  return from;
}

static int
forw_word(Line ln, int from)
{ from = min(from, ln->size);
  from = max(0, from);

  if ( ln->data )
  { while(!rlc_is_word_char(ln->data[from]) && from < ln->size )
      from++;
    while(rlc_is_word_char(ln->data[from]) && from < ln->size )
      from++;
  }

  return from;
}

		 /*******************************
		 *       EDITING FUNCTIONS	*
		 *******************************/

static __inline void
changed(Line ln, int from)
{ ln->change_start = min(ln->change_start, from);
}


static void
insert_self(Line ln, int chr)
{ make_room(ln, 1);
  ln->data[ln->point++] = chr;
}


static void
backward_delete_character(Line ln, int chr)
{ if ( ln->point > 0 )
  { memmove(&ln->data[ln->point-1], &ln->data[ln->point],
	    ln->size - ln->point);
    ln->size--;
    ln->point--;
  }

  changed(ln, ln->point);
}


static void
delete_character(Line ln, int chr)
{ if ( ln->point < ln->size )
  { ln->point++;
    backward_delete_character(ln, chr);
  }
}


static void
backward_character(Line ln, int chr)
{ if ( ln->point > 0 )
    ln->point--;
}


static void
forward_character(Line ln, int chr)
{ if ( ln->point < ln->size )
    ln->point++;
}


static void
backward_word(Line ln, int chr)
{ ln->point = back_word(ln, ln->point);
}


static void
forward_word(Line ln, int chr)
{ ln->point = forw_word(ln, ln->point);
}


static void
backward_delete_word(Line ln, int chr)
{ int from = back_word(ln, ln->point);
  
  memmove(&ln->data[from], &ln->data[ln->point], ln->size - ln->point);
  ln->size -= ln->point - from;
  ln->point = from;
  changed(ln, from);
}


static void
forward_delete_word(Line ln, int chr)
{ int to = forw_word(ln, ln->point);
  
  memmove(&ln->data[ln->point], &ln->data[to], ln->size - to);
  ln->size -= to - ln->point;
  changed(ln, ln->point);
}


static void
transpose_chars(Line ln, int chr)
{ if ( ln->point > 0 && ln->point < ln->size )
  { int c0 = ln->data[ln->point-1];
    ln->data[ln->point-1] = ln->data[ln->point];
    ln->data[ln->point] = c0;
    changed(ln, ln->point-1);
  } 
}


static void
start_of_line(Line ln, int chr)
{ ln->point = 0;
}


static void
end_of_line(Line ln, int chr)
{ ln->point = ln->size;
}


static void
kill_line(Line ln, int chr)
{ ln->size = ln->point;
  changed(ln, ln->size);
}


static void
empty_line(Line ln, int chr)
{ ln->size = ln->point = 0;
  changed(ln, 0);
}


static void
enter(Line ln, int chr)
{ ln->point = ln->size;
#ifdef DOS_CRNL  
  make_room(ln, 2);
  ln->data[ln->point++] = '\r';
  ln->data[ln->point++] = '\n';
#else
  make_room(ln, 1);
  ln->data[ln->point++] = '\n';
#endif
  terminate(ln);
  ln->complete = COMPLETE_NEWLINE;
}


static void
eof(Line ln, int chr)
{ ln->point = ln->size;
  terminate(ln);
  ln->complete = COMPLETE_EOF;
}


static void
delete_character_or_eof(Line ln, int chr)
{ if ( ln->size == 0 )
  { ln->point = ln->size;
    terminate(ln);
    ln->complete = COMPLETE_EOF;
  } else
    delete_character(ln, chr);
}


static void
undefined(Line ln, int chr)
{ 
}


static void
interrupt(Line ln, int chr)
{ raise(SIGINT);
}

		 /*******************************
		 *		HISTORY		*
		 *******************************/

static void
add_history(const char *data)
{ const char *s = data;

  while(*s && *s <= ' ')
    s++;

  if ( *s )
    rlc_add_history(s);
}


static void
backward_history(Line ln, int chr)
{ const char *h;

  if ( rlc_at_head_history() && ln->size > 0 )
  { terminate(ln);
    add_history(ln->data);
  }

  if ( (h = rlc_bwd_history()) )
  { set_line(ln, h);
    ln->point = ln->size;
  }
}


static void
forward_history(Line ln, int chr)
{ if ( !rlc_at_head_history() )
  { const char *h = rlc_fwd_history();

    if ( h )
    { set_line(ln, h);
      ln->point = ln->size;
    }
  } else
    empty_line(ln, chr);
}

		 /*******************************
		 *	      COMPLETE		*
		 *******************************/

RlcCompleteFunc
rlc_complete_hook(RlcCompleteFunc new)
{ RlcCompleteFunc old = _rlc_complete_function;

  _rlc_complete_function = new;

  return old;
}


static int
common(const char *s1, const char *s2, int insensitive)
{ int n = 0;

  if ( !insensitive )
  { while(*s1 && *s1 == *s2)
    { s1++, s2++;
      n++;
    }
    return n;
  } else
  { while(*s1)
    { if ( tolower(*s1) == tolower(*s2) )
      { s1++, s2++;
	n++;
      } else
	break;
    }
    return n;
  }
}


static void
complete(Line ln, int chr)
{ if ( _rlc_complete_function )
  { rlc_complete_data dbuf;
    RlcCompleteData data = &dbuf;

    memset(data, 0, sizeof(dbuf));
    data->line      = ln;
    data->call_type = COMPLETE_INIT;

    if ( (*_rlc_complete_function)(data) )
    { char match[COMPLETE_MAX_WORD_LEN];
      int nmatches = 1;
      int ncommon = strlen(data->candidate);
      int patlen = ln->point - data->replace_from;

      strcpy(match, data->candidate);

      data->call_type = COMPLETE_ENUMERATE;
      while( (*data->function)(data) )
      { ncommon = common(match, data->candidate, data->case_insensitive);
	match[ncommon] = EOS;
	nmatches++;
      }
      data->call_type = COMPLETE_CLOSE;
      (*data->function)(data);
       
      delete(ln, data->replace_from, patlen);
      ln->point = data->replace_from;
      make_room(ln, ncommon);
      memcpy(&ln->data[data->replace_from], match, ncommon);
      ln->point += ncommon;
      if ( nmatches == 1 && data->quote )
	insert_self(ln, data->quote);
    }
  }
}

#define MAX_LIST_COMPLETIONS 256

static void
list_completions(Line ln, int chr)
{ if ( _rlc_complete_function )
  { rlc_complete_data dbuf;
    RlcCompleteData data = &dbuf;

    memset(data, 0, sizeof(dbuf));
    data->line      = ln;
    data->call_type = COMPLETE_INIT;

    if ( (*_rlc_complete_function)(data) )
    { char *buf[COMPLETE_MAX_MATCHES];
      int nmatches = 0;
      int len = strlen(data->candidate) + 1;
      int longest = len;
      int n, cols;

      buf[nmatches] = rlc_malloc(len);
      memcpy(buf[nmatches], data->candidate, len);
      nmatches++;

      data->call_type = COMPLETE_ENUMERATE;
      while( (*data->function)(data) )
      { len = strlen(data->candidate) + 1;
	buf[nmatches] = rlc_malloc(len);
	memcpy(buf[nmatches], data->candidate, len);
	nmatches++;
	longest = max(longest, len);

	if ( nmatches > COMPLETE_MAX_MATCHES )
	{ char *msg = "\r\n! Too many matches\r\n";
	  
	  while(*msg)
	    rlc_putchar(*msg++);
	  ln->reprompt = TRUE;
	  data->call_type = COMPLETE_CLOSE;
	  (*data->function)(data);
	  return;
	}
      }
      data->call_type = COMPLETE_CLOSE;
      (*data->function)(data);

      cols = ScreenCols() / longest;
      rlc_putchar('\r');
      rlc_putchar('\n');

      for(n=0; n<nmatches; )
      { char *s = buf[n];
	len = 0;

	while(*s)
	{ len++;
	  rlc_putchar(*s++);
	}
	
	rlc_free(buf[n++]);

	if ( n % cols == 0 )
	{ rlc_putchar('\r');
	  rlc_putchar('\n');
	} else
	{ while( len++ < longest )
	  rlc_putchar(' ');
	}
      }
      if ( nmatches % cols != 0 )
      { rlc_putchar('\r');
	rlc_putchar('\n');
      }

      ln->reprompt = TRUE;
    }
  }
}


		 /*******************************
		 *	      REPAINT		*
		 *******************************/

static void
output(char *s, int len)
{ while(len-- > 0)
  { if ( *s == '\n' )
      rlc_putchar('\r');
    rlc_putchar(*s++);
  }
}


static void
update_display(Line ln)
{ if ( ln->reprompt )
  { const char *prompt = rlc_prompt(NULL);
    const char *s = prompt;
      
    rlc_putchar('\r');
    while(*s)
      rlc_putchar(*s++);

    rlc_get_mark(&ln->origin);

    ln->change_start = 0;
    ln->reprompt = FALSE;
  }

  rlc_goto_mark(&ln->origin, ln->data, ln->change_start);
  output(&ln->data[ln->change_start], ln->size - ln->change_start);
  rlc_erase_from_caret();
  rlc_goto_mark(&ln->origin, ln->data, ln->point);
  rlc_update();

  ln->change_start = ln->size;
}

		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

char *
read_line()
{ line ln;

  init_line_package();

  memset(&ln, 0, sizeof(line));
  rlc_get_mark(&ln.origin);

  while(!ln.complete)
  { int c;
    rlc_mark m0, m1;

    rlc_get_mark(&m0);
    if ( (c = getch()) == IMODE_SWITCH_CHAR )
      return RL_CANCELED_CHARP;

    if ( c == ESC )
    { if ( (c = getch()) == IMODE_SWITCH_CHAR )
	return RL_CANCELED_CHARP;
      if ( c >= 0 && c < 128 )
	c += 128;
    }
    rlc_get_mark(&m1);

    (*dispatch_table[c & 0xff])(&ln, c);
    if ( m0.mark_x != m1.mark_x || m0.mark_y != m1.mark_y )
      ln.reprompt = TRUE;
    update_display(&ln);
  }
  rlc_clearprompt();

  add_history(ln.data);

  return ln.data;
}


		 /*******************************
		 *	     DISPATCH		*
		 *******************************/

static void
init_dispatch_table()
{ int n;

  for(n=0; n<32; n++)
    dispatch_table[n] = undefined;
  for(n=32; n<256; n++)
    dispatch_table[n] = insert_self;
      
  bind_actions();
}


static void
init_line_package()
{ static int done;

  if ( !done )
  { init_dispatch_table();
    rlc_init_history(TRUE, 50);
    done = TRUE;
  }
}

		 /*******************************
		 *	       BIND		*
		 *******************************/

typedef struct _action
{ char		*name;
  function	 function;
  unsigned char  keys[4];
} action, *Action;

#define ACTION(n, f, k) { n, f, k }

static action actions[] = {
  ACTION("insert_self",		      insert_self,		 ""),
  ACTION("backward_delete_character", backward_delete_character, "\b"),
  ACTION("complete",		      complete,			 "\t"),
  ACTION("enter",		      enter, 			 "\r\n"),
  ACTION("start_of_line",	      start_of_line,		 {ctrl('A')}),
  ACTION("backward_character",	      backward_character,        {ctrl('B')}),
  ACTION("interrupt",		      interrupt,		 {ctrl('C')}),
  ACTION("end_of_line",		      end_of_line,		 {ctrl('E')}),
  ACTION("forward_character",	      forward_character,	 {ctrl('F')}),
  ACTION("transpose_chars",	      transpose_chars,		 {ctrl('T')}),
  ACTION("kill_line",		      kill_line,		 {ctrl('K')}),
  ACTION("backward_history",	      backward_history,		 {ctrl('P')}),
  ACTION("forward_history",	      forward_history,		 {ctrl('N')}),
  ACTION("empty_line",		      empty_line,		 {ctrl('U')}),
  ACTION("eof",			      eof,			 {ctrl('Z')}),

  ACTION("delete_character_or_eof",   delete_character_or_eof,	 {ctrl('D')}),
  ACTION("delete_character",	      delete_character,		 {127}),
  { "forward_word",		forward_word,  {meta(ctrl('F')), meta('f')}},
  { "backward_word",		backward_word, {meta(ctrl('B')), meta('b')}},
  { "forward_delete_word",	forward_delete_word, {meta(127), meta('d')}},
  ACTION("list_completions",	      list_completions,     {meta('?')}),
  ACTION("backward_delete_word",      backward_delete_word, {meta('\b')}),

  ACTION(NULL,			      NULL,			 "")
};

static void
bind_actions()
{ Action a = actions;

  for( ; a->name; a++ )
  { unsigned char *k = a->keys;

    for( ; *k; k++ )
      dispatch_table[*k] = a->function;
  }
}


int
rlc_bind(int chr, const char *fname)
{ if ( chr >= 0 && chr <= 256 )
  { Action a = actions;
    
    for( ; a->name; a++ )
    { if ( strcmp(a->name, fname) == 0 )
      { dispatch_table[chr] = a->function;
	return TRUE;
      }
    }
  }

  return FALSE;
}

