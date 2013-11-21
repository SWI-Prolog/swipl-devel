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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-utf8.h"
#include "pl-codelist.h"
#include <errno.h>
#include <stdio.h>
#if HAVE_LIMITS_H
#include <limits.h>			/* solaris compatibility */
#endif

#undef LD
#define LD LOCAL_LD


		 /*******************************
		 *	UNIFIED TEXT STUFF	*
		 *******************************/

static inline size_t
bufsize_text(PL_chars_t *text, size_t len)
{ size_t unit;

  switch(text->encoding)
  { case ENC_ISO_LATIN_1:
    case ENC_ASCII:
    case ENC_UTF8:
    case ENC_ANSI:
      unit = sizeof(char);
      break;
    case ENC_WCHAR:
      unit = sizeof(pl_wchar_t);
      break;
    default:
      assert(0);
      unit = sizeof(char);		/*NOTREACHED*/
  }

  return len*unit;
}


void
PL_save_text(PL_chars_t *text, int flags)
{ if ( (flags & BUF_MALLOC) && text->storage != PL_CHARS_MALLOC )
  { size_t bl = bufsize_text(text, text->length+1);
    void *new = PL_malloc(bl);

    memcpy(new, text->text.t, bl);
    text->text.t = new;
    text->storage = PL_CHARS_MALLOC;
  } else if ( text->storage == PL_CHARS_LOCAL )
  { Buffer b = findBuffer(BUF_RING);
    size_t bl = bufsize_text(text, text->length+1);

    addMultipleBuffer(b, text->text.t, bl, char);
    text->text.t = baseBuffer(b, char);

    text->storage = PL_CHARS_RING;
  } else if ( text->storage == PL_CHARS_MALLOC )
  { Buffer b = findBuffer(BUF_RING);
    size_t bl = bufsize_text(text, text->length+1);

    addMultipleBuffer(b, text->text.t, bl, char);
    PL_free_text(text);
    text->text.t = baseBuffer(b, char);

    text->storage = PL_CHARS_RING;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_from_stack_text() moves a string from  the   stack,  so  it won't get
corrupted if GC/shift comes along.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
PL_from_stack_text(PL_chars_t *text)
{ if ( text->storage == PL_CHARS_STACK )
  { size_t bl = bufsize_text(text, text->length+1);

    if ( bl < sizeof(text->buf) )
    { memcpy(text->buf, text->text.t, bl);
      text->text.t = text->buf;
      text->storage = PL_CHARS_LOCAL;
    } else
    { Buffer b = findBuffer(BUF_RING);

      addMultipleBuffer(b, text->text.t, bl, char);
      text->text.t = baseBuffer(b, char);
      text->storage = PL_CHARS_RING;
    }
  }
}


#define INT64_DIGITS 20

static char *
ui64toa(uint64_t val, char *out)
{ char tmpBuf[INT64_DIGITS + 1];
  char *ptrOrg = tmpBuf + INT64_DIGITS;
  char *ptr = ptrOrg;
  size_t nbDigs;

  do
  { int rem = val % 10;

    *--ptr = rem + '0';
    val /= 10;
  } while ( val );

  nbDigs = ptrOrg - ptr;
  memcpy(out, ptr, nbDigs);
  out += nbDigs;
  *out = '\0';

  return out;				/* points to the END */
};


static char *
i64toa(int64_t val, char *out)
{ if ( val < 0 )
  { *out++ = '-';
    val = -val;
  }

  return ui64toa((uint64_t)val, out);
}


int
PL_get_text__LD(term_t l, PL_chars_t *text, int flags ARG_LD)
{ word w = valHandle(l);

  if ( (flags & CVT_ATOM) && isAtom(w) )
  { if ( !get_atom_text(w, text) )
      goto maybe_write;
  } else if ( (flags & CVT_STRING) && isString(w) )
  { if ( !get_string_text(w, text PASS_LD) )
      goto maybe_write;
    PL_from_stack_text(text);
  } else if ( (flags & CVT_INTEGER) && isInteger(w) )
  { number n;

    PL_get_number(l, &n);
    switch(n.type)
    { case V_INTEGER:
      { char *ep = i64toa(n.value.i, text->buf);

        text->text.t    = text->buf;
	text->length    = ep-text->text.t;
	text->storage   = PL_CHARS_LOCAL;
	break;
      }
#ifdef O_GMP
      case V_MPZ:
      { size_t sz = mpz_sizeinbase(n.value.mpz, 10) + 2;
	Buffer b  = findBuffer(BUF_RING);

	if ( !growBuffer(b, sz) )
	  outOfCore();
	mpz_get_str(b->base, 10, n.value.mpz);
	b->top = b->base + strlen(b->base);
	text->text.t  = baseBuffer(b, char);
	text->length  = entriesBuffer(b, char);
	text->storage = PL_CHARS_RING;

	break;
      }
#endif
      default:
	assert(0);
    }
    text->encoding  = ENC_ISO_LATIN_1;
    text->canonical = TRUE;
  } else if ( (flags & CVT_FLOAT) && isFloat(w) )
  { format_float(valFloat(w), text->buf);
    text->text.t    = text->buf;
    text->length    = strlen(text->text.t);
    text->encoding  = ENC_ISO_LATIN_1;
    text->storage   = PL_CHARS_LOCAL;
    text->canonical = TRUE;
  } else if ( (flags & CVT_LIST) )
  { Buffer b;
    CVT_result result;

    if ( (b = codes_or_chars_to_buffer(l, BUF_RING, FALSE, &result)) )
    { text->length = entriesBuffer(b, char);
      addBuffer(b, EOS, char);
      text->text.t = baseBuffer(b, char);
      text->encoding = ENC_ISO_LATIN_1;
    } else if ( result.status == CVT_wide &&
		(b = codes_or_chars_to_buffer(l, BUF_RING, TRUE, &result)) )
    { text->length = entriesBuffer(b, pl_wchar_t);
      addBuffer(b, EOS, pl_wchar_t);
      text->text.w = baseBuffer(b, pl_wchar_t);
      text->encoding = ENC_WCHAR;
    } else if ( (flags & (CVT_WRITE|CVT_WRITE_CANONICAL)) )
    { goto case_write;
    } else
    { if ( (flags & CVT_VARNOFAIL) && result.status == CVT_partial )
	return 2;

      if ( (flags & CVT_EXCEPTION) )
      { switch(result.status)
	{ case CVT_partial:
	    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
	  case CVT_nolist:
	    goto error;
	  case CVT_nocode:
	  case CVT_nochar:
	  { term_t culprit = PL_new_term_ref();
	    atom_t type;

	    *valTermRef(culprit) = result.culprit;
	    if ( result.status == CVT_nocode )
	      type = ATOM_character_code;
	    else
	      type = ATOM_character;

	    return PL_error(NULL, 0, NULL, ERR_TYPE, type, culprit);
	  }
	  default:
	    break;
	}
      }
      goto error;
    }

    text->storage   = PL_CHARS_RING;
    text->canonical = TRUE;
  } else if ( (flags & CVT_VARIABLE) && isVar(w) )
  { text->text.t   = varName(l, text->buf);
    text->length   = strlen(text->text.t);
    text->encoding = ENC_ISO_LATIN_1;
    text->storage  = PL_CHARS_LOCAL;
    text->canonical = TRUE;
  } else if ( (flags & (CVT_WRITE|CVT_WRITE_CANONICAL)) )
  { IOENC encodings[3];
    IOENC *enc;
    char *r;
    int wflags;

  case_write:
    encodings[0] = ENC_ISO_LATIN_1;
    encodings[1] = ENC_WCHAR;
    encodings[2] = ENC_UNKNOWN;

    if ( (flags&CVT_WRITEQ) == CVT_WRITEQ )
      wflags = PL_WRT_QUOTED|PL_WRT_NUMBERVARS;
    else if ( (flags&CVT_WRITE_CANONICAL) )
      wflags = PL_WRT_QUOTED|PL_WRT_IGNOREOPS|PL_WRT_NUMBERVARS;
    else
      wflags = PL_WRT_NUMBERVARS;

    for(enc = encodings; *enc != ENC_UNKNOWN; enc++)
    { size_t size;
      IOSTREAM *fd;

      r = text->buf;
      size = sizeof(text->buf);
      fd = Sopenmem(&r, &size, "w");
      fd->encoding = *enc;
      if ( PL_write_term(fd, l, 1200, wflags) &&
	   Sputcode(EOS, fd) >= 0 &&
	   Sflush(fd) >= 0 )
      { text->encoding = *enc;
	text->storage = (r == text->buf ? PL_CHARS_LOCAL : PL_CHARS_MALLOC);
	text->canonical = TRUE;

	if ( *enc == ENC_ISO_LATIN_1 )
	{ text->length = size-1;
	  text->text.t = r;
	} else
	{ text->length = (size/sizeof(pl_wchar_t))-1;
	  text->text.w = (pl_wchar_t *)r;
	}

	Sclose(fd);

	return TRUE;
      } else
      { Sclose(fd);
	if ( r != text->buf )
	  Sfree(r);
      }
    }

    goto error;
  } else
  { goto error;
  }

  succeed;

maybe_write:
  if ( (flags & (CVT_WRITE|CVT_WRITE_CANONICAL)) )
    goto case_write;

error:
  if ( canBind(w) && (flags & CVT_VARNOFAIL) )
    return 2;

  if ( (flags & CVT_EXCEPTION) )
  { atom_t expected;

    if ( (flags & CVT_LIST) && !(flags&(CVT_ATOM|CVT_NUMBER)) )
      expected = ATOM_list;		/* List and/or string object */
    else if ( flags & CVT_LIST )
      expected = ATOM_text;
    else if ( flags & CVT_NUMBER )
      expected = ATOM_atomic;
    else
      expected = ATOM_atom;

    return PL_error(NULL, 0, NULL, ERR_TYPE, expected, l);
  }

  fail;
}


atom_t
textToAtom(PL_chars_t *text)
{ if ( !PL_canonicalise_text(text) )
    return 0;

  if ( text->encoding == ENC_ISO_LATIN_1 )
  { return lookupAtom(text->text.t, text->length);
  } else
  { return lookupUCSAtom(text->text.w, text->length);
  }
}


word
textToString(PL_chars_t *text)
{ if ( !PL_canonicalise_text(text) )
    return 0;

  if ( text->encoding == ENC_ISO_LATIN_1 )
  { return globalString(text->length, text->text.t);
  } else
  { return globalWString(text->length, text->text.w);
  }
}


int
PL_unify_text(term_t term, term_t tail, PL_chars_t *text, int type)
{ switch(type)
  { case PL_ATOM:
    { atom_t a = textToAtom(text);

      if ( a )
      { int rval = _PL_unify_atomic(term, a);

	PL_unregister_atom(a);
	return rval;
      }
      return FALSE;
    }
    case PL_STRING:
    { word w = textToString(text);

      if ( w )
	return _PL_unify_atomic(term, w);
      else
	return FALSE;
    }
    case PL_CODE_LIST:
    case PL_CHAR_LIST:
    { if ( text->length == 0 )
      { if ( tail )
	{ GET_LD
	  PL_put_term(tail, term);
	  return TRUE;
	} else
	{ return PL_unify_nil(term);
	}
      } else
      { GET_LD
	term_t l = PL_new_term_ref();
	Word p0, p;

	switch(text->encoding)
	{ case ENC_ISO_LATIN_1:
	  { const unsigned char *s = (const unsigned char *)text->text.t;
	    const unsigned char *e = &s[text->length];

            if ( !(p0 = p = INIT_SEQ_STRING(text->length)) )
	      return FALSE;

            if ( type == PL_CODE_LIST ) {
              for( ; s < e; s++)
                p = EXTEND_SEQ_CODES(p, *s);
            } else {
              for( ; s < e; s++)
                p = EXTEND_SEQ_CHARS(p, *s);
            }
	    break;
	  }
	  case ENC_WCHAR:
	  { const pl_wchar_t *s = (const pl_wchar_t *)text->text.t;
	    const pl_wchar_t *e = &s[text->length];

            if ( !(p0 = p = INIT_SEQ_STRING(text->length)) )
	      return FALSE;

            if ( type == PL_CODE_LIST ) {
              for( ; s < e; s++)
                p = EXTEND_SEQ_CODES(p, *s);
            } else {
              for( ; s < e; s++)
                p = EXTEND_SEQ_CHARS(p, *s);
            }
	    break;
	  }
	  case ENC_UTF8:
	  { const char *s = text->text.t;
	    const char *e = &s[text->length];
	    size_t len = utf8_strlen(s, text->length);

            if ( !(p0 = p = INIT_SEQ_STRING(len)) )
	      return FALSE;

            if ( type == PL_CODE_LIST ) {
              while (s < e) {
                int chr;

                s = utf8_get_char(s, &chr);
                p = EXTEND_SEQ_CODES(p, chr);
              }
            } else {
              while (s < e) {
                int chr;

                s = utf8_get_char(s, &chr);
                p = EXTEND_SEQ_CHARS(p, chr);
              }
            }
	    break;
	  }
	  case ENC_ANSI:
	  { const char *s = text->text.t;
	    size_t rc, n = text->length;
	    size_t len = 0;
	    mbstate_t mbs;
	    wchar_t wc;

	    memset(&mbs, 0, sizeof(mbs));
	    while( n > 0 )
	    { if ( (rc=mbrtowc(&wc, s, n, &mbs)) == (size_t)-1 || rc == 0 )
		return PL_error(NULL, 0, "cannot represent text in current locale",
				ERR_REPRESENTATION, ATOM_encoding);

	      len++;
	      n -= rc;
	      s += rc;
	    }

            if ( !(p0 = p = INIT_SEQ_STRING(len)) )
	      return FALSE;

	    n = text->length;
	    s = text->text.t;
	    memset(&mbs, 0, sizeof(mbs));
	    while(n > 0)
	    { rc = mbrtowc(&wc, s, n, &mbs);

	      if ( type == PL_CODE_LIST )
		p = EXTEND_SEQ_CODES(p, wc);
	      else
		p = EXTEND_SEQ_CHARS(p, wc);

	      s += rc;
	      n -= rc;
	    }
	    break;
	  }
	  default:
	  { assert(0);

	    return FALSE;
	  }
	}

	return CLOSE_SEQ_STRING(p, p0, tail, term, l );
      }
    }
    default:
    { assert(0);

      return FALSE;
    }
  }
}


int
PL_unify_text_range(term_t term, PL_chars_t *text,
		    size_t offset, size_t len, int type)
{ if ( offset == 0 && len == text->length )
  { return PL_unify_text(term, 0, text, type);
  } else
  { PL_chars_t sub;
    int rc;

    if ( offset > text->length || offset + len > text->length )
      return FALSE;

    if ( len == 1 && type == PL_ATOM )
    { GET_LD
      int c;

      if ( text->encoding == ENC_ISO_LATIN_1 )
	c = text->text.t[offset]&0xff;
      else
	c = text->text.w[offset];

      return PL_unify_atom(term, codeToAtom(c));
    }

    sub.length = len;
    sub.storage = PL_CHARS_HEAP;
    if ( text->encoding == ENC_ISO_LATIN_1 )
    { sub.text.t   = text->text.t+offset;
      sub.encoding = ENC_ISO_LATIN_1;
      sub.canonical = TRUE;
    } else
    { sub.text.w   = text->text.w+offset;
      sub.encoding = ENC_WCHAR;
      sub.canonical = FALSE;
    }

    rc = PL_unify_text(term, 0, &sub, type);

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
{ if ( text->encoding != ENC_WCHAR )
  { if ( text->storage == PL_CHARS_MALLOC )
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

      text->encoding = ENC_WCHAR;
    } else if ( text->storage == PL_CHARS_LOCAL &&
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
      text->encoding = ENC_WCHAR;
    } else
    { Buffer b = findBuffer(BUF_RING);
      const unsigned char *s = (const unsigned char *)text->text.t;
      const unsigned char *e = &s[text->length];

      for( ; s<e; s++)
	addBuffer(b, *s, pl_wchar_t);
      addBuffer(b, EOS, pl_wchar_t);

      text->text.w   = baseBuffer(b, pl_wchar_t);
      text->encoding = ENC_WCHAR;
      text->storage  = PL_CHARS_RING;
    }
  }

  succeed;
}


int
PL_demote_text(PL_chars_t *text)
{ if ( text->encoding != ENC_ISO_LATIN_1 )
  { if ( text->storage == PL_CHARS_MALLOC )
    { char *new = PL_malloc(sizeof(char)*(text->length+1));
      char *t = new;
      const pl_wchar_t *s = (const pl_wchar_t *)text->text.t;
      const pl_wchar_t *e = &s[text->length];

      while(s<e)
      { if ( *s > 0xff )
	{ PL_free(new);
	  return FALSE;
	}
	*t++ = *s++ & 0xff;
      }
      *t = EOS;

      PL_free(text->text.t);
      text->text.t = new;

      text->encoding = ENC_ISO_LATIN_1;
    } else if ( text->storage == PL_CHARS_LOCAL )
    { pl_wchar_t buf[sizeof(text->buf)/sizeof(pl_wchar_t)];
      pl_wchar_t *f = buf;
      pl_wchar_t *e = &buf[text->length];
      char *t = text->buf;

      memcpy(buf, text->buf, text->length*sizeof(pl_wchar_t));
      while(f<e)
      { if ( *f > 0xff )
	  return FALSE;
	*t++ = *f++ & 0xff;
      }
      *t = EOS;
      text->encoding = ENC_ISO_LATIN_1;
    } else
    { Buffer b = findBuffer(BUF_RING);
      const pl_wchar_t *s = (const pl_wchar_t*)text->text.w;
      const pl_wchar_t *e = &s[text->length];

      for( ; s<e; s++)
      { if ( *s > 0xff )
	{ unfindBuffer(BUF_RING);
	  return FALSE;
	}
	addBuffer(b, *s&0xff, char);
      }
      addBuffer(b, EOS, char);

      text->text.t   = baseBuffer(b, char);
      text->storage  = PL_CHARS_RING;
      text->encoding = ENC_ISO_LATIN_1;
    }
  }

  succeed;
}


static int
can_demote(PL_chars_t *text)
{ if ( text->encoding != ENC_ISO_LATIN_1 )
  { const pl_wchar_t *w = (const pl_wchar_t*)text->text.w;
    const pl_wchar_t *e = &w[text->length];

    for(; w<e; w++)
    { if ( *w > 0xff )
	return FALSE;
    }
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Convert text to 8-bit according to flags.   May hold REP_UTF8 to convert
to UTF-8, REP_MB to convert to locale 8-bit representation or nothing to
convert to ISO Latin-1. This predicate can   fail  of the text cannot be
represented.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
wctobuffer(wchar_t c, mbstate_t *mbs, Buffer buf)
{ char b[PL_MB_LEN_MAX];
  size_t n;

  if ( (n=wcrtomb(b, c, mbs)) != (size_t)-1 )
  { size_t i;

    for(i=0; i<n; i++)
      addBuffer(buf, b[i], char);

    return TRUE;
  }

  return FALSE;				/* cannot represent */
}


static void
utf8tobuffer(wchar_t c, Buffer buf)
{ if ( c <= 0x7f )
  { addBuffer(buf, (char)c, char);
  } else
  { char b[6];
    char *e = b;
    const char *s;

    e = utf8_put_char(e, c);
    for(s=b; s<e; s++)
      addBuffer(buf, *s, char);
  }
}


int
PL_mb_text(PL_chars_t *text, int flags)
{ int norep = -1;
  IOENC target = ((flags&REP_UTF8) ? ENC_UTF8 :
		  (flags&REP_MB)   ? ENC_ANSI : ENC_ISO_LATIN_1);

  if ( text->encoding != target )
  { Buffer b = findBuffer(BUF_RING);

    switch(text->encoding)
    { case ENC_ISO_LATIN_1:
      { const unsigned char *s = (const unsigned char*)text->text.t;
	const unsigned char *e = &s[text->length];

	if ( target == ENC_UTF8 )
	{ for( ; s<e; s++)
	  { utf8tobuffer(*s, b);
	  }
	  addBuffer(b, 0, char);
	} else /* if ( target == ENC_MB ) */
	{ mbstate_t mbs;

	  memset(&mbs, 0, sizeof(mbs));
	  for( ; s<e; s++)
	  { if ( !wctobuffer(*s, &mbs, b) )
	    { unfindBuffer(BUF_RING);
	      norep = *s;
	      goto rep_error;
	    }
	  }
	  wctobuffer(0, &mbs, b);
	}

        break;
      }
      case ENC_WCHAR:
      { if ( target == ENC_ISO_LATIN_1 )
	{ return PL_demote_text(text);
	} else
	{ const pl_wchar_t *w = (const pl_wchar_t*)text->text.w;
	  const pl_wchar_t *e = &w[text->length];

	  if ( target == ENC_UTF8 )
	  { for( ; w<e; w++)
	    { utf8tobuffer(*w, b);
	    }
	    addBuffer(b, 0, char);
	  } else /* if ( target == ENC_MB ) */
	  { mbstate_t mbs;

	    memset(&mbs, 0, sizeof(mbs));
	    for( ; w<e; w++)
	    { if ( !wctobuffer(*w, &mbs, b) )
	      { unfindBuffer(BUF_RING);
		norep = *w;
		goto rep_error;
	      }
	    }
	    wctobuffer(0, &mbs, b);
	  }
	}
	break;
      }
      default:
      { assert(0);
	fail;
      }
    }

    text->length   = sizeOfBuffer(b)-1;
    text->text.t   = baseBuffer(b, char);
    text->encoding = target;
    text->storage  = PL_CHARS_RING;
  }

  succeed;

rep_error:
  if ( (flags & CVT_EXCEPTION) )
  { char msg[128];

    sprintf(msg,
	    "Cannot represent char U%04x using %s encoding",
	    norep,
	    target == ENC_ISO_LATIN_1 ? "ISO Latin-1" : "current locale");

    return PL_error(NULL, 0, msg, ERR_REPRESENTATION, ATOM_encoding);
  }

  fail;
}


int
PL_canonicalise_text(PL_chars_t *text)
{ if ( !text->canonical )
  { switch(text->encoding )
    { case ENC_ISO_LATIN_1:
	break;				/* nothing to do */
      case ENC_WCHAR:
      { const pl_wchar_t *w = (const pl_wchar_t*)text->text.w;
	const pl_wchar_t *e = &w[text->length];

	for(; w<e; w++)
	{ if ( *w > 0xff )
	    return TRUE;
	}

	return PL_demote_text(text);
      }
      case ENC_UTF8:
      { const char *s = text->text.t;
	const char *e = &s[text->length];

	while(s<e && !(*s & 0x80))
	  s++;
	if ( s == e )
	{ text->encoding  = ENC_ISO_LATIN_1;
	  text->canonical = TRUE;
	} else
	{ int chr;
	  int wide = FALSE;
	  size_t len = s - text->text.t;

	  while(s<e)
	  { s = utf8_get_char(s, &chr);
	    if ( chr > 0xff )		/* requires wide characters */
	      wide = TRUE;
	    len++;
	  }

	  s = (const char *)text->text.t;
	  text->length = len;

	  if ( wide )
	  { pl_wchar_t *to = PL_malloc(sizeof(pl_wchar_t)*(len+1));

	    text->text.w = to;
	    while(s<e)
	    { s = utf8_get_char(s, &chr);
	      *to++ = chr;
	    }
	    *to = EOS;

	    text->encoding = ENC_WCHAR;
	    text->storage  = PL_CHARS_MALLOC;
	  } else
	  { char *to = PL_malloc(len+1);

	    text->text.t = to;
	    while(s<e)
	    { s = utf8_get_char(s, &chr);
	      *to++ = chr;
	    }
	    *to = EOS;

	    text->encoding = ENC_ISO_LATIN_1;
	    text->storage  = PL_CHARS_MALLOC;
	  }

	  text->canonical = TRUE;
	}

	succeed;
      }
      case ENC_ANSI:
      { mbstate_t mbs;
	size_t len = 0;
	int iso = TRUE;
	char *s = text->text.t;
	size_t rc, n = text->length;
	wchar_t wc;

	memset(&mbs, 0, sizeof(mbs));
	while( n > 0 )
	{ if ( (rc=mbrtowc(&wc, s, n, &mbs)) == (size_t)-1 || rc == 0)
	    return FALSE;		/* encoding error */

	  if ( wc > 0xff )
	    iso = FALSE;
	  len++;
	  n -= rc;
	  s += rc;
	}

	if ( n == 0 )
	{ const char *from = text->text.t;
	  void *do_free;

	  n = text->length;
	  memset(&mbs, 0, sizeof(mbs));

	  if ( text->storage == PL_CHARS_MALLOC )
	    do_free = text->text.t;
	  else
	    do_free = NULL;

	  if ( iso )
	  { char *to;

	    text->encoding = ENC_ISO_LATIN_1;
	    if ( len+1 < sizeof(text->buf) )
	    { text->text.t = text->buf;
	      text->storage = PL_CHARS_LOCAL;
	    } else
	    { text->text.t = PL_malloc(len+1);
	      text->storage = PL_CHARS_MALLOC;
	    }

	    to = text->text.t;
	    while( n > 0 )
	    { rc = mbrtowc(&wc, from, n, &mbs);

	      *to++ = (char)wc;
	      n -= rc;
	      from += rc;
	    }
	    *to = EOS;
	  } else
	  { wchar_t *to;
	    char b2[sizeof(text->buf)];

	    text->encoding = ENC_WCHAR;
	    if ( len+1 < sizeof(text->buf)/sizeof(wchar_t) )
	    { if ( text->text.t == text->buf )
	      { memcpy(b2, text->buf, sizeof(text->buf));
		from = b2;
	      }
	      text->text.w = (wchar_t*)text->buf;
	    } else
	    { text->text.w = PL_malloc((len+1)*sizeof(wchar_t));
	      text->storage = PL_CHARS_MALLOC;
	    }

	    to = text->text.w;
	    while( n > 0 )
	    { rc = mbrtowc(&wc, from, n, &mbs);

	      *to++ = wc;
	      n -= rc;
	      from += rc;
	    }
	    *to = EOS;
	  }

	  text->length = len;
	  text->canonical = TRUE;
	  if ( do_free )
	    PL_free(do_free);

	  succeed;
	}

	fail;
      }
      default:
	assert(0);
    }
  }

  succeed;
}


void
PL_free_text(PL_chars_t *text)
{ if ( text->storage == PL_CHARS_MALLOC )
    PL_free(text->text.t);
}


void
PL_text_recode(PL_chars_t *text, IOENC encoding)
{ if ( text->encoding != encoding )
  { switch(encoding)
    { case ENC_UTF8:
      { switch(text->encoding)
	{ case ENC_ASCII:
	    text->encoding = ENC_UTF8;
	    break;
	  case ENC_ISO_LATIN_1:
	  { Buffer b = findBuffer(BUF_RING);
	    const unsigned char *s = (const unsigned char *)text->text.t;
	    const unsigned char *e = &s[text->length];
	    char tmp[8];

	    for( ; s<e; s++)
	    { if ( *s&0x80 )
	      { const char *end = utf8_put_char(tmp, *s);
		const char *q = tmp;

		for(q=tmp; q<end; q++)
		  addBuffer(b, *q, char);
	      } else
	      { addBuffer(b, *s, char);
	      }
	    }
	    PL_free_text(text);
            text->length   = entriesBuffer(b, char);
	    addBuffer(b, EOS, char);
	    text->text.t   = baseBuffer(b, char);
	    text->encoding = ENC_UTF8;
	    text->storage  = PL_CHARS_RING;

	    break;
	  }
	  case ENC_WCHAR:
	  { Buffer b = findBuffer(BUF_RING);
	    const pl_wchar_t *s = text->text.w;
	    const pl_wchar_t *e = &s[text->length];
	    char tmp[8];

	    for( ; s<e; s++)
	    { if ( *s > 0x7f )
	      { const char *end = utf8_put_char(tmp, (int)*s);
		const char *q = tmp;

		for(q=tmp; q<end; q++)
		  addBuffer(b, *q&0xff, char);
	      } else
	      { addBuffer(b, *s&0xff, char);
	      }
	    }
	    PL_free_text(text);
            text->length   = entriesBuffer(b, char);
	    addBuffer(b, EOS, char);
	    text->text.t   = baseBuffer(b, char);
	    text->encoding = ENC_UTF8;
	    text->storage  = PL_CHARS_RING;

	    break;
	  }
	  default:
	    assert(0);
	}
	break;
	default:
	  assert(0);
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_cmp_text(PL_chars_t *t1, size_t o1,
	    PL_chars_t *t2, size_t o2,
	    size_t len)

Compares two substrings of two text representations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_cmp_text(PL_chars_t *t1, size_t o1, PL_chars_t *t2, size_t o2,
	    size_t len)
{ ssize_t l = len;
  int ifeq = 0;

  if ( l > (ssize_t)(t1->length - o1) )
  { l = t1->length - o1;
    ifeq = CMP_LESS;				/* first is short */
  }
  if ( l > (ssize_t)(t2->length - o2) )
  { l = t2->length - o2;
    if ( ifeq == 0 )
      ifeq = CMP_GREATER;
  }

  if ( l == 0 )				/* too long offsets */
    return ifeq;

  if ( t1->encoding == ENC_ISO_LATIN_1 && t2->encoding == ENC_ISO_LATIN_1 )
  { const unsigned char *s = (const unsigned char *)t1->text.t+o1;
    const unsigned char *q = (const unsigned char *)t2->text.t+o2;

    for(; l-- > 0 && *s == *q; s++, q++ )
      ;
    if ( l < 0 )
      return ifeq;
    else
      return *s > *q ? CMP_GREATER : CMP_LESS;
  } else if ( t1->encoding == ENC_WCHAR && t2->encoding == ENC_WCHAR )
  { const pl_wchar_t *s = t1->text.w+o1;
    const pl_wchar_t *q = t2->text.w+o2;

    for(; l-- > 0 && *s == *q; s++, q++ )
      ;
    if ( l < 0 )
      return ifeq;
    else
      return *s > *q ? CMP_GREATER : CMP_LESS;
  } else if ( t1->encoding == ENC_ISO_LATIN_1 && t2->encoding == ENC_WCHAR )
  { const unsigned char *s = (const unsigned char *)t1->text.t+o1;
    const pl_wchar_t *q = t2->text.w+o2;

    for(; l-- > 0 && *s == *q; s++, q++ )
      ;
    if ( l < 0 )
      return ifeq;
    else
      return *s > *q ? CMP_GREATER : CMP_LESS;
  } else
  { const pl_wchar_t *s = t1->text.w+o1;
    const unsigned char *q = (const unsigned char *)t2->text.t+o2;

    for(; l-- > 0 && *s == *q; s++, q++ )
      ;
    if ( l < 0 )
      return ifeq;
    else
      return *s > *q ? CMP_GREATER : CMP_LESS;
  }
}


int
PL_concat_text(int n, PL_chars_t **text, PL_chars_t *result)
{ size_t total_length = 0;
  int latin = TRUE;
  int i;

  for(i=0; i<n; i++)
  { if ( latin && !can_demote(text[i]) )
      latin = FALSE;
    total_length += text[i]->length;
  }

  result->canonical = TRUE;
  result->length = total_length;

  if ( latin )
  { char *to;

    result->encoding = ENC_ISO_LATIN_1;
    if ( total_length+1 < sizeof(result->buf) )
    { result->text.t = result->buf;
      result->storage = PL_CHARS_LOCAL;
    } else
    { result->text.t = PL_malloc(total_length+1);
      result->storage = PL_CHARS_MALLOC;
    }

    for(to=result->text.t, i=0; i<n; i++)
    { memcpy(to, text[i]->text.t, text[i]->length);
      to += text[i]->length;
    }
    *to = EOS;
  } else
  { pl_wchar_t *to;

    result->encoding = ENC_WCHAR;
    if ( total_length+1 < sizeof(result->buf)/sizeof(pl_wchar_t) )
    { result->text.w = (pl_wchar_t*)result->buf;
      result->storage = PL_CHARS_LOCAL;
    } else
    { result->text.w = PL_malloc((total_length+1)*sizeof(pl_wchar_t));
      result->storage = PL_CHARS_MALLOC;
    }

    for(to=result->text.w, i=0; i<n; i++)
    { if ( text[i]->encoding == ENC_WCHAR )
      { memcpy(to, text[i]->text.w, text[i]->length*sizeof(pl_wchar_t));
	to += text[i]->length;
      } else
      { const unsigned char *f = (const unsigned char *)text[i]->text.t;
	const unsigned char *e = &f[text[i]->length];

	while(f<e)
	  *to++ = *f++;
      }
    }
    assert((size_t)(to-result->text.w) == total_length);
    *to = EOS;
  }

  return TRUE;
}


IOSTREAM *
Sopen_text(PL_chars_t *txt, const char *mode)
{ IOSTREAM *stream;

  if ( !streq(mode, "r") )
  { errno = EINVAL;
    return NULL;
  }

  stream = Sopen_string(NULL,
			txt->text.t,
			bufsize_text(txt, txt->length),
			mode);
  stream->encoding = txt->encoding;

  return stream;
}
