/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

#include "pl-incl.h"
#include "pl-ctype.h"

#undef LD
#define LD LOCAL_LD

static inline word
valHandle__LD(term_t r ARG_LD)
{ Word p = valTermRef(r);

  deRef(p);
  return *p;
}

#define valHandle(r) valHandle__LD(r PASS_LD)
#define setHandle(h, w)		(*valTermRef(h) = (w))


		 /*******************************
		 *	UNIFIED TEXT STUFF	*
		 *******************************/

static inline int
bufsize_text(PL_chars_t *text)
{ int unit;

  if ( true(text, PL_CHARS_LATIN) )
    unit = sizeof(char);
  else if ( true(text, PL_CHARS_UCS) )
    unit = sizeof(pl_wchar_t);
  else
    assert(0);

  return (text->length+1)*unit;
}


void
PL_save_text(PL_chars_t *text, int flags)
{ if ( false(text, BUF_MALLOC) && false(text, PL_CHARS_MALLOC) )
  { int bl = bufsize_text(text);
    void *new = PL_malloc(bl);

    memcpy(new, text->text.t, bl);
    text->text.w = new;
    text->flags &= ~PL_CHARS_ALLOC_MASK;
    text->flags |= PL_CHARS_MALLOC;
  }  
}


int
PL_get_text(term_t l, PL_chars_t *text, int flags)
{ GET_LD
  word w = valHandle(l);

  if ( (flags & CVT_ATOM) && isAtom(w) )
  { Atom a = atomValue(w);
    if ( false(a->type, PL_BLOB_TEXT) )
      fail;				/* non-textual atom */
    if ( isUCSAtom(a) )
    { text->text.w = (pl_wchar_t *) a->name;
      text->length = a->length / sizeof(pl_wchar_t);
      text->flags = PL_CHARS_UCS|PL_CHARS_HEAP;
    } else
    { text->text.t = a->name;
      text->length = a->length;
      text->flags = PL_CHARS_LATIN|PL_CHARS_HEAP;
    }
  } else if ( (flags & CVT_INTEGER) && isInteger(w) )
  { Ssprintf(text->buf, "%ld", valInteger(w) );
    text->text.t = text->buf;
    text->length = strlen(text->text.t);
    text->flags = PL_CHARS_LATIN|PL_CHARS_LOCAL;
  } else if ( (flags & CVT_FLOAT) && isReal(w) )
  { char *q;

    Ssprintf(text->buf, LD->float_format, valReal(w) );
    text->text.t = text->buf;

    q = text->buf;			/* See also writePrimitive() */
    if ( *q == L'-' )
      q++;
    for(; *q; q++)
    { if ( !isDigit(*q) )
	break;
    }
    if ( !*q )
    { *q++ = '.';
      *q++ = '0';
      *q = EOS;
    }
    text->length = strlen(text->text.t);
    text->flags = PL_CHARS_LATIN|PL_CHARS_LOCAL;
  } else if ( (flags & CVT_STRING) && isString(w) )
  { if ( isBString(w) )
    { text->text.t = getCharsString(w, &text->length);
      text->flags = PL_CHARS_LATIN|PL_CHARS_STACK;
    } else
    { text->text.w = getCharsWString(w, &text->length);
      text->flags = PL_CHARS_UCS|PL_CHARS_STACK;
    }
  } else if ( (flags & CVT_LIST) &&
	      (isList(w) || isNil(w)) )
  { Buffer b;

    if ( (b = codes_or_chars_to_buffer(l, BUF_RING, FALSE)) )
    { text->length = entriesBuffer(b, char);
      addBuffer(b, EOS, char);
      text->text.t = baseBuffer(b, char);
      text->flags = PL_CHARS_LATIN|PL_CHARS_RING;
    } else if ( (b = codes_or_chars_to_buffer(l, BUF_RING, TRUE)) )
    { text->length = entriesBuffer(b, pl_wchar_t);
      addBuffer(b, EOS, pl_wchar_t);
      text->text.w = baseBuffer(b, pl_wchar_t);
      text->flags = PL_CHARS_UCS|PL_CHARS_RING;
    } else
      fail;
  } else if ( (flags & CVT_VARIABLE) && isVar(w) )
  { text->text.t = varName(l, text->buf);
    text->length = strlen(text->text.t);
    text->flags = PL_CHARS_LATIN|PL_CHARS_LOCAL;
  } else if ( (flags & CVT_WRITE) )
  { IOENC encodings[] = { ENC_ISO_LATIN_1, ENC_WCHAR, ENC_NONE };
    IOENC *enc;
    char *r;

    for(enc = encodings; *enc != ENC_NONE; enc++)
    { int size;
      IOSTREAM *fd;
    
      r = text->buf;
      size = sizeof(text->buf);
      fd = Sopenmem(&r, &size, "w");
      fd->encoding = *enc;
      if ( PL_write_term(fd, l, 1200, 0) &&
	   Sputcode(EOS, fd) >= 0 &&
	   Sflush(fd) >= 0 )
      { text->length = (size/sizeof(pl_wchar_t))-1;
	text->text.w = (pl_wchar_t *)r;
	Sclose(fd);
	break;
      } else
      { Sclose(fd);
	if ( r != text->buf )
	  Sfree(r);
      }
    }

    if ( *enc != ENC_NONE )
    { if ( r != text->buf )
	text->flags = *enc|PL_CHARS_MALLOC;
      else
	text->flags = *enc|PL_CHARS_LOCAL;

      text->text.t = r;
    } else
    { return FALSE;
    }
  } else
  { fail;
  }

  succeed;
}


int
PL_unify_text(term_t term, PL_chars_t *text, int type)
{ switch(type)
  { case PL_ATOM:
    case PL_STRING:
      PL_canonise_text(text);
      if ( true(text, PL_CHARS_LATIN) )
      { if ( type == PL_ATOM )
	  return PL_unify_atom_nchars(term, text->length, text->text.t);
	else
	  return PL_unify_string_nchars(term, text->length, text->text.t);
      }
    case PL_CODE_LIST:
    case PL_CHAR_LIST:
    { if ( text->length == 0 )
      { return PL_unify_nil(term);
      } else
      { GET_LD
	term_t l = PL_new_term_ref();
	Word p = allocGlobal(text->length*3);
      
	setHandle(l, consPtr(p, TAG_COMPOUND|STG_GLOBAL));
	if ( true(text, PL_CHARS_LATIN) )
	{ const unsigned char *s = (const unsigned char *)text->text.t;
	  const unsigned char *e = &s[text->length];

	  for( ; s < e; p++)
	  { *p++ = FUNCTOR_dot2;
	    if ( type == PL_CODE_LIST )
	      *p++ = consInt(*s);
	    else
	      *p++ = codeToAtom(*s);
	    *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);
	    p++;
	  }
	} else
	{ const pl_wchar_t *s = (const pl_wchar_t *)text->text.t;
	  const pl_wchar_t *e = &p[text->length];

	  for( ; s < e; p++)
	  { *p++ = FUNCTOR_dot2;
	    if ( type == PL_CODE_LIST )
	      *p++ = consInt(*s);
	    else
	      *p++ = codeToAtom(*s);
	    *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);
	    p++;
	  }
	}
	p[-1] = ATOM_nil;

	return PL_unify(l, term);
      }
    }
    default:
      assert(0);
  }
}


int
PL_unify_text_range(term_t term, PL_chars_t *text,
		    unsigned offset, unsigned len, int type)
{ if ( offset == 0 && len == text->length )
  { return PL_unify_text(term, text, type);
  } else
  { PL_chars_t sub;
    int rc;

    if ( offset > text->length || offset + len > text->length )
      return FALSE;

    sub.length = len;
    if ( true(text, PL_CHARS_LATIN) )
    { sub.text.t = text->text.t+offset;
      sub.flags  = (PL_CHARS_LATIN|PL_CHARS_HEAP);
    } else
    { sub.text.w = text->text.w+offset;
      sub.flags  = (PL_CHARS_UCS|PL_CHARS_HEAP);
    }

    rc = PL_unify_text(term, &sub, type);

    PL_free_text(&sub);
    
    return rc;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int PL_promote_text(PL_chars_t *text)

Promote a text to USC if it is currently 8-bit text.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_promote_text(PL_chars_t *text)
{ if ( false(text, PL_CHARS_UCS) )
  { if ( true(text, PL_CHARS_MALLOC) )
    { pl_wchar_t *new = PL_malloc(sizeof(pl_wchar_t)*(text->length+1));
      pl_wchar_t *t = new;
      const unsigned char *s = (const unsigned char *)text->text.t;
      const unsigned char *e = &s[text->length];

      while(s<e)
      { *t++ = *s++;
      }
      *t = EOS;

      PL_free(text->text.t);
      text->text.w = new;
      
      text->flags = (PL_CHARS_UCS|PL_CHARS_MALLOC);
    } else if ( true(text, PL_CHARS_LOCAL) &&
	        (text->length+1)*sizeof(pl_wchar_t) < sizeof(text->buf) )
    { unsigned char buf[sizeof(text->buf)];
      unsigned char *f = buf;
      unsigned char *e = &buf[text->length];
      pl_wchar_t *t = (pl_wchar_t*)text->buf;

      memcpy(buf, text->buf, text->length*sizeof(char));
      while(f<e)
      { *t++ = *f++;
      }
      *t = EOS;
      text->flags = (PL_CHARS_UCS|PL_CHARS_LOCAL);
    } else if ( true(text, PL_CHARS_RING|PL_CHARS_HEAP|PL_CHARS_STACK) )
    { Buffer b = findBuffer(BUF_RING);
      const unsigned char *s = (const unsigned char *)text->text.t;
      const unsigned char *e = &s[text->length];

      for( ; s<e; s++)
	addBuffer(b, *s, pl_wchar_t);
      addBuffer(b, EOS, pl_wchar_t);

      text->text.w = baseBuffer(b, pl_wchar_t);
      text->flags = (PL_CHARS_UCS|PL_CHARS_RING);
    } else
    { assert(0);
    }
  }

  succeed;
}


int
PL_demote_text(PL_chars_t *text)
{ if ( false(text, PL_CHARS_LATIN) )
  { if ( true(text, PL_CHARS_MALLOC) )
    { char *new = PL_malloc(sizeof(char)*(text->length+1));
      char *t = new;
      const pl_wchar_t *s = (const pl_wchar_t *)text->text.t;
      const pl_wchar_t *e = &s[text->length];

      while(s<e)
      { if ( *s > 0xff )
	{ PL_free(new);
	  return FALSE;
	}
	*t++ = *s++;
      }
      *t = EOS;

      PL_free(text->text.t);
      text->text.t = new;
      
      text->flags = (PL_CHARS_LATIN|PL_CHARS_MALLOC);
    } else if ( true(text, PL_CHARS_LOCAL) )
    { pl_wchar_t buf[sizeof(text->buf)/sizeof(pl_wchar_t)];
      pl_wchar_t *f = buf;
      pl_wchar_t *e = &buf[text->length];
      char *t = text->buf;

      memcpy(buf, text->buf, text->length*sizeof(pl_wchar_t));
      while(f<e)
      { if ( *f > 0xff )
	  return FALSE;
	*t++ = *f++;
      }
      *t = EOS;
      text->flags = (PL_CHARS_LATIN|PL_CHARS_LOCAL);
    } else if ( true(text, PL_CHARS_RING|PL_CHARS_HEAP|PL_CHARS_STACK) )
    { Buffer b = findBuffer(BUF_RING);
      const pl_wchar_t *s = (const pl_wchar_t*)text->text.w;
      const pl_wchar_t *e = &s[text->length];

      for( ; s<e; s++)
      { if ( *s > 0xff )
	{ unfindBuffer(BUF_RING);
	  return FALSE;
	}
	addBuffer(b, *s, char);
      }
      addBuffer(b, EOS, char);

      text->text.t = baseBuffer(b, char);
      text->flags = (PL_CHARS_LATIN|PL_CHARS_RING);
    } else
    { assert(0);
    }
  }

  succeed;
}


static int
can_demote(PL_chars_t *text)
{ if ( false(text, PL_CHARS_LATIN) )
  { const pl_wchar_t *w = (const pl_wchar_t*)text->text.w;
    const pl_wchar_t *e = &w[text->length];

    for(; w<e; w++)
    { if ( *w > 0xff )
	return FALSE;
    }
  }

  return TRUE;
}


int
PL_canonise_text(PL_chars_t *text)
{ if ( false(text, PL_CHARS_LATIN) )
  { const pl_wchar_t *w = (const pl_wchar_t*)text->text.w;
    const pl_wchar_t *e = &w[text->length];

    for(; w<e; w++)
    { if ( *w > 0xff )
	return FALSE;
    }

    return PL_demote_text(text);
  }

  succeed;
}


void
PL_free_text(PL_chars_t *text)
{ if ( true(text, PL_CHARS_MALLOC) )
    PL_free(text->text.t);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_cmp_text(PL_chars_t *t1, unsigned o1,
	    PL_chars_t *t2, unsigned o2,
	    unsigned len)

Compares two substrings of two text representations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_cmp_text(PL_chars_t *t1, unsigned o1, PL_chars_t *t2, unsigned o2,
	    unsigned len)
{ int l = len;
  int ifeq = 0;

  if ( l > t1->length - o1 )
  { l = t1->length - o1;
    ifeq = -1;				/* first is short */
  }
  if ( l > t2->length - o2 )
  { l = t2->length - o2;
    if ( ifeq == 0 )
      ifeq = 1;
  }

  if ( l < 0 )				/* too long offsets */
    return ifeq;

  if ( true(t1, PL_CHARS_LATIN) && true(t2, PL_CHARS_LATIN) )
  { const unsigned char *s = t1->text.t+o1;
    const unsigned char *q = t2->text.t+o2;

    for(; l-- > 0 && *s == *q; s++, q++ )
      ;
    if ( l < 0 )
      return ifeq;
    else
      return *s > *q ? 1 : -1;
  } else if ( true(t1, PL_CHARS_UCS) && true(t2, PL_CHARS_UCS) )
  { const pl_wchar_t *s = t1->text.w+o1;
    const pl_wchar_t *q = t2->text.w+o2;

    for(; l-- > 0 && *s == *q; s++, q++ )
      ;
    if ( l < 0 )
      return ifeq;
    else
      return *s > *q ? 1 : -1;
  } else if ( true(t1, PL_CHARS_LATIN) && true(t2, PL_CHARS_UCS) )
  { const unsigned char *s = t1->text.t+o1;
    const pl_wchar_t *q = t2->text.w+o2;

    for(; l-- > 0 && *s == *q; s++, q++ )
      ;
    if ( l < 0 )
      return ifeq;
    else
      return *s > *q ? 1 : -1;
  } else
  { const pl_wchar_t *s = t1->text.w+o1;
    const unsigned char *q = t2->text.t+o2;

    for(; l-- > 0 && *s == *q; s++, q++ )
      ;
    if ( l < 0 )
      return ifeq;
    else
      return *s > *q ? 1 : -1;
  }  
}


int
PL_concat_text(int n, PL_chars_t **text, PL_chars_t *result)
{ int total_length = 0;
  int latin = TRUE;
  int i;

  for(i=0; i<n; i++)
  { if ( latin && !can_demote(text[i]) )
      latin = FALSE;
    total_length += text[i]->length;
  }

  result->length = total_length;

  if ( latin )
  { char *to;

    if ( total_length+1 < sizeof(result->buf) )
    { result->text.t = result->buf;
      result->flags = PL_CHARS_LATIN|PL_CHARS_LOCAL;
    } else
    { result->text.t = PL_malloc(total_length+1);
      result->flags = PL_CHARS_LATIN|PL_CHARS_MALLOC;
    }

    for(to=result->text.t, i=0; i<n; i++)
    { memcpy(to, text[i]->text.t, text[i]->length);
      to += text[i]->length;
    }
    *to = EOS;
  } else
  { pl_wchar_t *to;

    if ( total_length+1 < sizeof(result->buf)/sizeof(pl_wchar_t) )
    { result->text.w = (pl_wchar_t*)result->buf;
      result->flags = PL_CHARS_UCS|PL_CHARS_LOCAL;
    } else
    { result->text.w = PL_malloc((total_length+1)*sizeof(pl_wchar_t));
      result->flags = PL_CHARS_UCS|PL_CHARS_MALLOC;
    }

    for(to=result->text.w, i=0; i<n; i++)
    { if ( true(text[i], PL_CHARS_UCS) )
      { memcpy(to, text[i]->text.w, text[i]->length*sizeof(pl_wchar_t));
      } else
      { const unsigned char *f = text[i]->text.t;
	const unsigned char *e = &f[text[i]->length];

	while(f<e)
	  *to++ = *f++;
      }
      to += text[i]->length;
    }
    *to = EOS;
  }

  return TRUE;
}
