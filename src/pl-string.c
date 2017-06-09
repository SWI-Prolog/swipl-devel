/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2015, University of Amsterdam
                              VU University Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "os/pl-ctype.h"
#include "os/pl-utf8.h"
#include "pl-inline.h"

#undef LD
#define LD LOCAL_LD

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
String manipulation predicates. Note that   many other string predicates
are in other files because they are   written as generalisations of code
already in those files.

Eventually, it might be better to concentrate string operations and atom
operations that handle atoms as strings in this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	       UTIL		*
		 *******************************/

static void
addUTF8Buffer(Buffer b, int c)
{ if ( c >= 0x80 )
  { char buf[6];
    char *p, *end;

    end = utf8_put_char(buf, c);
    for(p=buf; p<end; p++)
    { addBuffer(b, *p&0xff, char);
    }
  } else
  { addBuffer(b, c, char);
  }
}


static const char *
backSkipUTF8(const char *start, const char *s, int *chr)
{ s = utf8_backskip_char(start, s);
  utf8_get_char(s, chr);

  return s;
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

static
PRED_IMPL("atom_string", 2, atom_string, 0)
{ PRED_LD
  term_t a = A1;
  term_t str = A2;
  PL_chars_t t;
  int rc;

  if ( PL_get_text(str, &t, CVT_ALL) )
    rc = PL_unify_text(a, 0, &t, PL_ATOM);
  else if ( PL_get_text(a, &t, CVT_ALL) )
    rc = PL_unify_text(str, 0, &t, PL_STRING);
  else if ( !PL_is_variable(str) )
    return PL_type_error("string", str);
  else if ( !PL_is_variable(a) )
    return PL_type_error("atom", a);
  else
    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);

  PL_free_text(&t);

  return rc;
}


static int
string_x(term_t str, term_t list, int out_type ARG_LD)
{ PL_chars_t t;
  int rc;

  switch ( PL_get_text(str, &t, CVT_ALL|CVT_EXCEPTION|CVT_VARNOFAIL) )
  { case TRUE:
      rc = PL_unify_text(list, 0, &t, out_type);
      PL_free_text(&t);
      return rc;
    case FALSE:
      return FALSE;
    /*case 2: str can bind */
  }

  if ( PL_get_text(list, &t, CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { rc = PL_unify_text(str, 0, &t, PL_STRING);
    PL_free_text(&t);
    return rc;
  }

  return FALSE;
}

static
PRED_IMPL("string_codes", 2, string_codes, 0)
{ PRED_LD

  return string_x(A1, A2, PL_CODE_LIST PASS_LD);
}

static
PRED_IMPL("string_chars", 2, string_chars, 0)
{ PRED_LD

  return string_x(A1, A2, PL_CHAR_LIST PASS_LD);
}


static
PRED_IMPL("text_to_string", 2, text_to_string, 0)
{ PRED_LD
  PL_chars_t t;

  if ( PL_is_string(A1) )
    return PL_unify(A1, A2);

  if ( PL_get_text(A1, &t, CVT_ATOM|CVT_LIST|CVT_EXCEPTION) )
  { int rc = PL_unify_text(A2, 0, &t, PL_STRING);
    PL_free_text(&t);
    return rc;
  }

  return FALSE;
}


/** string_code(?Index, +String, ?Code)

True when the Index'ed character of String has code Code.
*/

static
PRED_IMPL("string_code", 3, string_code, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  PL_chars_t t;
  size_t idx;
  int tchar;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { size_t i;

      if ( !PL_get_text(A2, &t, CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
	return FALSE;
      if ( !PL_is_variable(A1) )
      { if ( !PL_get_size_ex(A1, &i) )
	  return FALSE;
	if ( i == 0 || i > t.length )
	  return FALSE;
	return PL_unify_integer(A3, text_get_char(&t, i-1));
      } else
      { if ( !PL_is_variable(A3) )
	{ if ( !PL_get_char_ex(A3, &tchar, FALSE) )
	    return FALSE;
	} else if ( t.length > 0 )
	{ tchar = -1;
	} else
	  return FALSE;

	idx = 0;
	goto gen;
      }
    }
    case FRG_REDO:
    { idx = (size_t)CTX_INT;

      PL_get_text(A2, &t, CVT_ALL);
      if ( PL_is_variable(A3) )
	tchar = -1;
      else
	PL_get_char(A3, &tchar, FALSE);

    gen:
      if ( tchar == -1 )
      { if ( PL_unify_integer(A1, idx) &&
	     PL_unify_integer(A3, text_get_char(&t, idx)) )
	{ if ( idx+1 < t.length )
	    ForeignRedoInt(idx+1);
	  else
	    return TRUE;
	}
	return FALSE;
      }

      for(; idx < t.length; idx++)
      { if ( text_get_char(&t, idx) == tchar )
	{ if ( PL_unify_integer(A1, idx+1) )
	  { for(idx++; idx < t.length; idx++)
	    { if ( text_get_char(&t, idx) == tchar )
		ForeignRedoInt(idx);
	    }
	    return TRUE;
	  }
	  return FALSE;
	}
      }

      return FALSE;
    }
    default:
      return TRUE;
  }
}


/** get_string_code(+Index, +String, -Code)

True when the Index'ed character of String has code Code.
*/

static
PRED_IMPL("get_string_code", 3, get_string_code, 0)
{ PRED_LD
  PL_chars_t t;
  int64_t i;

  if ( !PL_get_text(A2, &t, CVT_ALL|CVT_EXCEPTION) ||
       !PL_get_int64_ex(A1, &i) )
    return FALSE;

  if ( i < 1 || i > (int64_t) t.length )
  { term_t av;

    return ( (av = PL_new_term_refs(2)) &&
	     PL_put_int64(av+0, 1) &&
	     PL_put_int64(av+1, t.length) &&
	     PL_error(NULL, 0, NULL, ERR_RANGE, av+0, av+1, A1)
	   );
  }

  return PL_unify_integer(A3, text_get_char(&t, i-1));
}


/** split_string(+String, +SepChars, +PadChars, -SubStrings) is det.
*/

static
PRED_IMPL("split_string", 4, split_string, 0)
{ PRED_LD
  PL_chars_t input, sep, pad;
  int rc = FALSE;
  int flags = CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION;

  input.storage = PL_CHARS_VIRGIN;
    sep.storage = PL_CHARS_VIRGIN;
    pad.storage = PL_CHARS_VIRGIN;

  if ( PL_get_text(A1, &input, flags) &&
       PL_get_text(A2, &sep,   flags) &&
       PL_get_text(A3, &pad,   flags) )
  { size_t i, last;
    term_t tail = PL_copy_term_ref(A4);
    term_t head = PL_new_term_ref();
    size_t sep_at = (size_t)-1;
    size_t end;

						/* back skip padding at end */
    for(end=input.length;
	end > 0 &&
	text_chr(&pad, text_get_char(&input, end-1)) != (size_t)-1;
	end--)
      ;

    for(i=0;;)
    {					/* skip padding */
      while( i<end &&
	     text_chr(&pad, text_get_char(&input, i)) != (size_t)-1 )
	i++;

      if ( i == end )
      { if ( !PL_unify_list_ex(tail, head, tail) ||
	     !PL_unify_chars(head, PL_STRING, 0, "") )
	  goto error;
	break;
      }

    no_skip_padding:
      last = i;				/* find sep */
      while( i<end &&
	     text_chr(&sep, text_get_char(&input, i)) == (size_t)-1 )
	i++;
      sep_at = i;			/* back skip padding */
      while( i>last &&
	     text_chr(&pad, text_get_char(&input, i-1)) != (size_t)-1 )
	i--;

      if ( !PL_unify_list_ex(tail, head, tail) ||
	   !PL_unify_text_range(head, &input, last, i-last, PL_STRING) )
	goto error;

      if ( sep_at == end )
	break;					/* no separator found */

      i = sep_at+1;

      if ( text_chr(&pad, text_get_char(&input, sep_at)) == (size_t)-1 &&
	   text_chr(&sep, text_get_char(&input, sep_at+1)) != (size_t)-1 )
	goto no_skip_padding;
    }

    rc = PL_unify_nil(tail);
  }

error:
  PL_free_text(&input);
  PL_free_text(&sep);
  PL_free_text(&pad);

  return rc;
}


static char *
backSkipPadding(const char *in, size_t len, PL_chars_t *pad)
{ const char *end = &in[len];

  while(end > in)
  { int chr;
    const char *e = backSkipUTF8(in, end, &chr);

    if ( text_chr(pad, chr) == (size_t)-1 )
      break;
    end = e;
  }

  return (char *)end;
}


/** read_string(+Stream, +Delimiters, +Padding, -Delimiter, -String)
*/

static
PRED_IMPL("read_string", 5, read_string, 0)
{ PRED_LD
  IOSTREAM *s = NULL;
  PL_chars_t sep, pad;
  int flags = CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION;
  int rc = FALSE;
  tmp_buffer tmpbuf;

  sep.storage = PL_CHARS_VIRGIN;
  pad.storage = PL_CHARS_VIRGIN;
  initBuffer(&tmpbuf);

  if ( getTextInputStream(A1, &s) &&
       PL_get_text(A2, &sep, flags) &&
       PL_get_text(A3, &pad, flags) )
  { int chr;

    do
    { chr = Sgetcode(s);
    } while(chr != EOF && text_chr(&pad, chr) != (size_t)-1);

    for(;;)
    { if ( chr == EOF && Sferror(s) )
	goto out;
      if ( chr == EOF || text_chr(&sep, chr) != (size_t)-1 )
	break;
      addUTF8Buffer((Buffer)&tmpbuf, chr);
      chr = Sgetcode(s);
    }

    tmpbuf.top = backSkipPadding(baseBuffer(&tmpbuf,char),
				 entriesBuffer(&tmpbuf, char),
				 &pad);
    rc = ( PL_unify_chars(A5, PL_STRING|REP_UTF8,
			  entriesBuffer(&tmpbuf, char),
			  baseBuffer(&tmpbuf, char)) &&
	   PL_unify_integer(A4, chr)
	 );
  }

out:
  discardBuffer(&tmpbuf);
  if ( s )
  { if ( rc )
      rc = PL_release_stream(s);
    else
      PL_release_stream(s);
  }
  PL_free_text(&sep);
  PL_free_text(&pad);

  return rc;
}


/** read_string(+Stream, ?Length, -String)
*/

static
PRED_IMPL("read_string", 3, read_string, 0)
{ PRED_LD
  IOSTREAM *s = NULL;
  tmp_buffer tmpbuf;
  int vlen;
  size_t len = (size_t)-1;
  int rc = FALSE;

  initBuffer(&tmpbuf);
  if ( getTextInputStream(A1, &s) &&
       ( (vlen=PL_is_variable(A2)) ||
	 PL_get_size_ex(A2, &len)
       ) )
  { size_t count;

    for(count=0; count < len; count++)
    { int chr = Sgetcode(s);
      if ( chr == EOF )
      { if ( Sferror(s) )
	  goto out;
	break;
      }
      addUTF8Buffer((Buffer)&tmpbuf, chr);
    }

    rc = ( PL_unify_chars(A3, PL_STRING|REP_UTF8,
			  entriesBuffer(&tmpbuf, char),
			  baseBuffer(&tmpbuf, char)) &&
	   (!vlen || PL_unify_int64(A2, count))
	 );
  }

out:
  discardBuffer(&tmpbuf);
  if ( s )
  { if ( rc )
      rc = PL_release_stream(s);
    else
      PL_release_stream(s);
  }

  return rc;
}

/** open_string(+String, -Stream)
 *
 * Open a string as a stream.
*/

static
PRED_IMPL("open_string", 2, open_string, 0)
{ PRED_LD
  PL_chars_t text;
  int flags = CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION;

  if ( PL_get_text(A1, &text, flags) )
  { IOSTREAM *s;

    if ( text.encoding != ENC_ISO_LATIN_1 )
      PL_mb_text(&text, REP_UTF8);

    PL_save_text(&text, BUF_MALLOC);
    s = Sopenmem(&text.text.t, &text.length, "rF");

    if ( s )
    { s->encoding = text.encoding;

      if ( PL_unify_stream(A2, s) )
	return TRUE;
      Sclose(s);
    }

    PL_free_text(&text);
  }

  return FALSE;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(strings)
  PRED_DEF("atom_string",     2, atom_string,	  0)
  PRED_DEF("split_string",    4, split_string,	  0)
  PRED_DEF("string_codes",    2, string_codes,	  0)
  PRED_DEF("string_chars",    2, string_chars,	  0)
  PRED_DEF("text_to_string",  2, text_to_string,  0)
  PRED_DEF("string_code",     3, string_code,	  PL_FA_NONDETERMINISTIC)
  PRED_DEF("get_string_code", 3, get_string_code, 0)
  PRED_DEF("read_string",     5, read_string,     0)
  PRED_DEF("read_string",     3, read_string,     0)
  PRED_DEF("open_string",     2, open_string,     0)
EndPredDefs
