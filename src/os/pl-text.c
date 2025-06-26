/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2025, University of Amsterdam
			      VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include "pl-incl.h"
#include "pl-arith.h"
#include "pl-ctype.h"
#include "pl-utf8.h"
#include "../pl-codelist.h"
#include "../pl-write.h"
#include "../pl-prims.h"
#include <errno.h>
#include <stdio.h>
#include <math.h>
#if HAVE_LIMITS_H
#include <limits.h>			/* solaris compatibility */
#endif

#undef LD
#define LD LOCAL_LD

static bool text_representation_error(PL_chars_t *text, IOENC enc);
static int text_error(PL_chars_t *text, int rc);

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
    case ENC_UTF16BE:
    case ENC_UTF16LE:
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


int
PL_save_text(PL_chars_t *text, int flags)
{ if ( (flags & BUF_MALLOC) && text->storage != PL_CHARS_MALLOC )
  { size_t bl = bufsize_text(text, text->length+1);
    void *new = PL_malloc(bl);

    if ( new )
    { memcpy(new, text->text.t, bl);
      text->text.t = new;
      text->storage = PL_CHARS_MALLOC;
    } else
    { return false;
    }
  } else if ( text->storage == PL_CHARS_LOCAL )
  { Buffer b = findBuffer(BUF_STACK);
    size_t bl = bufsize_text(text, text->length+1);

    addMultipleBuffer(b, text->text.t, bl, char);
    text->text.t = baseBuffer(b, char);

    text->storage = PL_CHARS_STACK;
  } else if ( text->storage == PL_CHARS_MALLOC )
  { Buffer b = findBuffer(BUF_STACK);
    size_t bl = bufsize_text(text, text->length+1);

    addMultipleBuffer(b, text->text.t, bl, char);
    PL_free_text(text);
    text->text.t = baseBuffer(b, char);

    text->storage = PL_CHARS_STACK;
  }

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_from_stack_text() moves a string from  the   stack,  so  it won't get
corrupted if GC/shift comes along.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
PL_from_stack_text(PL_chars_t *text, int flags)
{ if ( !(flags&BUF_ALLOW_STACK) )
  { if ( text->storage == PL_CHARS_PROLOG_STACK )
    { size_t bl = bufsize_text(text, text->length+1);

      if ( bl < sizeof(text->buf) )
      { memcpy(text->buf, text->text.t, bl);
	text->text.t = text->buf;
	text->storage = PL_CHARS_LOCAL;
      } else
      { Buffer b = findBuffer(BUF_STACK);

	addMultipleBuffer(b, text->text.t, bl, char);
	text->text.t = baseBuffer(b, char);
	text->storage = PL_CHARS_STACK;
      }
    }
  }

  return true;
}


#if SIZEOF_WCHAR_T == 2
size_t
utf16_text_length(const PL_chars_t *text)
{ assert(text->encoding == ENC_WCHAR);

  const wchar_t *s = (const wchar_t *)text->text.t;
  const wchar_t *e = &s[text->length];
  size_t count = 0;

  while(s < e)
  { int c = *s++;

    count++;
    if ( IS_UTF16_LEAD(c) )
      s++;
  }

  return count;
}
#endif

#define INT64_DIGITS 20

static char *
ui64toa(uint64_t val, char *out, int base)
{ char tmpBuf[INT64_DIGITS + 1];
  char *ptrOrg = tmpBuf + INT64_DIGITS;
  char *ptr = ptrOrg;
  size_t nbDigs;
  static const char digits[] = "0123456789abcdef";

  do
  { int rem = (int)(val % base);

    *--ptr = digits[rem];
    val /= base;
  } while ( val );

  nbDigs = ptrOrg - ptr;
  memcpy(out, ptr, nbDigs);
  out += nbDigs;
  *out = '\0';

  return out;				/* points to the END */
}


static char *
i64toa(int64_t val, char *out, int base)
{ if ( val < 0 )
  { *out++ = '-';
    val = -(uint64_t)val;
  }

  return ui64toa((uint64_t)val, out, base);
}


int
PL_get_text(DECL_LD term_t l, PL_chars_t *text, int flags)
{ word w = valHandle(l);

  if ( (flags & CVT_ATOM) && isAtom(w) )
  { if ( isNil(w) && (flags&CVT_LIST) )
      goto case_list;
    if ( !get_atom_text(word2atom(w), text) )
      goto maybe_write;
  } else if ( (flags & CVT_STRING) && isString(w) )
  { if ( !get_string_text(w, text) )
      goto maybe_write;
    if ( !PL_from_stack_text(text, flags) )
      return false;			/* no memory */
  } else if ( ((flags&CVT_RATIONAL) && isRational(w)) ||
	      ((flags&CVT_INTEGER)  && isInteger(w)) )
  { number n;
    int base = (flags&CVT_XINTEGER)==CVT_XINTEGER ? 16 : 10;

    PL_get_number(l, &n);
    switch(n.type)
    { case V_INTEGER:
      { char *ep = i64toa(n.value.i, text->buf, base);

	text->text.t    = text->buf;
	text->length    = ep-text->text.t;
	text->storage   = PL_CHARS_LOCAL;
	break;
      }
#ifdef O_BIGNUM
      case V_MPZ:
      { size_t sz = (size_t)((double)mpz_sizeinbase(n.value.mpz, 2)*log(10)/log(2)*1.2 + 2);
	Buffer b  = findBuffer(BUF_STACK);

	if ( !growBuffer(b, sz) )
	  outOfCore();
	mpz_get_str(b->base, base, n.value.mpz);
	b->top = b->base + strlen(b->base);
	text->text.t  = baseBuffer(b, char);
	text->length  = entriesBuffer(b, char);
	text->storage = PL_CHARS_STACK;

	break;
      }
      case V_MPQ:
      { size_t sz = (size_t)((double)( mpz_sizeinbase(mpq_numref(n.value.mpq), 2) +
				       mpz_sizeinbase(mpq_denref(n.value.mpq), 2) + 4 ) *
			     log(10)/log(2) * 1.2);
	Buffer b  = findBuffer(BUF_STACK);

	if ( !growBuffer(b, sz) )
	  outOfCore();
	mpz_get_str(b->base, base, mpq_numref(n.value.mpq));
	b->top = b->base + strlen(b->base);
	*b->top++ = 'r';			/* '/' under some condition? */
	mpz_get_str(b->top, base, mpq_denref(n.value.mpq));
	b->top += strlen(b->top);
	text->text.t  = baseBuffer(b, char);
	text->length  = entriesBuffer(b, char);
	text->storage = PL_CHARS_STACK;

	break;
      }
#endif
      default:
	assert(0);
    }
    text->encoding  = ENC_ISO_LATIN_1;
    text->canonical = true;
  } else if ( (flags & CVT_FLOAT) && isFloat(w) )
  { size_t sz = format_float(text->buf, sizeof(text->buf),
			     valFloat(w), 3, 'e');
    if ( sz < sizeof(text->buf) )
    { text->text.t    = text->buf;
      text->length    = sz;
      text->storage   = PL_CHARS_LOCAL;
    } else
    { Buffer b  = findBuffer(BUF_STACK);

      if ( !growBuffer(b, sz+1) )
	outOfCore();
      format_float(b->base, sz+1, valFloat(w), 3, 'e');
      text->text.t  = baseBuffer(b, char);
      text->length  = sz;
      text->storage = PL_CHARS_STACK;
    }
    text->encoding  = ENC_ISO_LATIN_1;
    text->canonical = true;
  } else if ( (flags & CVT_LIST) )
  { Buffer b;
    CVT_result result;

  case_list:
    if ( (b = codes_or_chars_to_buffer(l, BUF_STACK, false, &result)) )
    { text->length = entriesBuffer(b, char);
      addBuffer(b, EOS, char);
      text->text.t = baseBuffer(b, char);
      text->encoding = ENC_ISO_LATIN_1;
    } else if ( result.status == CVT_wide &&
		(b = codes_or_chars_to_buffer(l, BUF_STACK, true, &result)) )
    { text->length = entriesBuffer(b, pl_wchar_t);
      addBuffer(b, EOS, pl_wchar_t);
      text->text.w = baseBuffer(b, pl_wchar_t);
      text->encoding = ENC_WCHAR;
    } else if ( (flags & (CVT_WRITE|CVT_WRITE_CANONICAL|CVT_WRITEQ)) )
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
	  case CVT_representation:
	    return PL_representation_error("character_code");
	  default:
	    break;
	}
      }
      goto error;
    }

    text->storage   = PL_CHARS_STACK;
    text->canonical = true;
  } else if ( (flags & CVT_VARIABLE) && isVar(w) )
  { text->text.t   = varName(l, text->buf);
    text->length   = strlen(text->text.t);
    text->encoding = ENC_ISO_LATIN_1;
    text->storage  = PL_CHARS_LOCAL;
    text->canonical = true;
  } else if ( (flags & (CVT_WRITE|CVT_WRITE_CANONICAL|CVT_WRITEQ)) )
  { IOENC encodings[3];
    IOENC *enc;
    char *r;
    int wflags;

  case_write:
    if ( (flags&REP_UTF8) )
    { encodings[0] = ENC_UTF8;
      encodings[1] = ENC_UNKNOWN;
    } else
    { encodings[0] = ENC_ISO_LATIN_1;
      encodings[1] = ENC_WCHAR;
      encodings[2] = ENC_UNKNOWN;
    }

    if ( (flags&CVT_WRITEQ) )
      wflags = PL_WRT_QUOTED|PL_WRT_NUMBERVARS;
    else if ( (flags&CVT_WRITE_CANONICAL) )
      wflags = (PL_WRT_QUOTED|PL_WRT_QUOTE_NON_ASCII|
		PL_WRT_IGNOREOPS|PL_WRT_VARNAMES|
		PL_WRT_NODOTINATOM|PL_WRT_BRACETERMS);
    else
      wflags = PL_WRT_NUMBERVARS;

    int rc = false;
    BEGIN_NUMBERVARS(true);
    nv_options options =
    { .functor = FUNCTOR_isovar1,
      .on_attvar = AV_SKIP,
      .singletons = PL_is_acyclic(l),
      .numbered_check = false
    };
    if ( numberVars(l, &options, 0) == NV_ERROR )
      goto error;

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
	text->canonical = true;

	if ( *enc == ENC_WCHAR )
	{ text->length = (size/sizeof(pl_wchar_t))-1;
	  text->text.w = (pl_wchar_t *)r;
	} else
	{ text->length = size-1;
	  text->text.t = r;
	}

	Sclose(fd);

	rc = true;
	break;
      } else
      { Sclose(fd);
	if ( *enc == ENC_ISO_LATIN_1 && enc[1] != ENC_UNKNOWN )
	  PL_clear_exception();

	if ( r != text->buf )
	  Sfree(r);
      }
    }
    END_NUMBERVARS(true);
    if ( rc )
      goto out;
    goto error;
  } else
  { goto error;
  }

out:
  return true;

maybe_write:
  if ( (flags & (CVT_WRITE|CVT_WRITE_CANONICAL|CVT_WRITEQ)) )
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
    else if ( (flags & CVT_ATOM) && w == ATOM_nil )
      expected = ATOM_atom;		/* [] \== '[]' */
    else if ( flags & CVT_NUMBER )
      expected = ATOM_atomic;
    else
      expected = ATOM_atom;

    return PL_error(NULL, 0, NULL, ERR_TYPE, expected, l);
  }

  fail;
}


atom_t
textToAtom(DECL_LD PL_chars_t *text)
{ atom_t a;
  int rc;

  PL_STRINGS_MARK();
  if ( (rc=PL_canonicalise_text(text)) == true )
  { if ( text->encoding == ENC_ISO_LATIN_1 )
      a = lookupAtom(text->text.t, text->length);
    else
      a = lookupUCSAtom(text->text.w, text->length);
  } else
  { text_error(text, rc);
    a = 0;
  }
  PL_STRINGS_RELEASE();

  return a;
}


word
textToString(DECL_LD PL_chars_t *text)
{ word w;
  int rc;

  PL_STRINGS_MARK();
  if ( (rc=PL_canonicalise_text(text)) == true )
  { if ( text->encoding == ENC_ISO_LATIN_1 )
      w = globalString(text->length, text->text.t);
    else
      w = globalWString(text->length, text->text.w);
  } else
  { text_error(text, rc);
    w = 0;
  }
  PL_STRINGS_RELEASE();

  return w;
}


static size_t
globalSpaceRequirement(PL_chars_t *text)
{ size_t len;

  if ( text->encoding == ENC_ISO_LATIN_1 )
  { len = text->length+1;
  } else
  { len = (text->length+1)*sizeof(pl_wchar_t);
  }

  return 2 + (len+sizeof(word))/sizeof(word);
}



#define unify_text(term, tail, text, type) \
	LDFUNC(unify_text, term, tail, text, type)

static bool
unify_text(DECL_LD term_t term, term_t tail, PL_chars_t *text, int type)
{ switch(type)
  { case PL_ATOM:
    { atom_t a = textToAtom(text);

      if ( a )
      { bool rval = PL_unify_atomic(term, a);

	PL_unregister_atom(a);
	return rval;
      }
      return false;
    }
    case PL_STRING:
    { word w;
      int rc;

      if ( (rc=PL_canonicalise_text(text)) == true )
      { if ( hasGlobalSpace(globalSpaceRequirement(text)) ||
	     PL_from_stack_text(text, 0) )
	{ if ( (w = textToString(text)) )
	    return PL_unify_atomic(term, w);
	}

	return false;
      } else
	return text_error(text, rc);
    }
    case PL_CODE_LIST:
    case PL_CHAR_LIST:
    { if ( text->length == 0 )
      { if ( tail )
	{ PL_put_term(tail, term);
	  return true;
	} else
	{ return PL_unify_nil(term);
	}
      } else
      { Word p0, p;

	if ( text->storage == PL_CHARS_PROLOG_STACK &&   /* text from a string */
	     ( !hasGlobalSpace(text->length*3+1) ||
	       !hasLocalSpace(sizeof(word)) ) &&  /* no shift/GC needed */
	     !PL_from_stack_text(text, 0) )	  /* shift/gc; if we cannot */
	  return false;				  /* copy we must fail */

	term_t l = PL_new_term_ref();
	if ( !l )
	  return false;

	switch(text->encoding)
	{ case ENC_ISO_LATIN_1:
	  { const unsigned char *s = (const unsigned char *)text->text.t;
	    const unsigned char *e = &s[text->length];

	    if ( !(p0 = p = INIT_SEQ_STRING(text->length)) )
	      return false;

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

	    if ( !(p0 = p = INIT_SEQ_STRING(PL_text_length(text))) )
	      return false;

	    if ( type == PL_CODE_LIST )
	    { while(s < e)
	      { int c;

		s = get_wchar(s, &c);
		p = EXTEND_SEQ_CODES(p, c);
	      }
	    } else
	    { while(s < e)
	      { int c;

		s = get_wchar(s, &c);
		p = EXTEND_SEQ_CHARS(p, c);
	      }
	    }
	    break;
	  }
	  case ENC_UTF8:
	  { const char *s = text->text.t;
	    const char *e = &s[text->length];
	    size_t len = utf8_strlen(s, text->length);

	    if ( !(p0 = p = INIT_SEQ_STRING(len)) )
	      return false;

	    if ( type == PL_CODE_LIST ) {
	      while (s < e) {
		int chr;

		PL_utf8_code_point(&s, e, &chr);
		p = EXTEND_SEQ_CODES(p, chr);
	      }
	    } else {
	      while (s < e) {
		int chr;

		PL_utf8_code_point(&s, e, &chr);
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
	      return false;

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

	    return false;
	  }
	}

	bool rc = CLOSE_SEQ_STRING(p, p0, tail, term, l );
	PL_reset_term_refs(l);
	return rc;
      }
    }
    default:
    { assert(0);

      return false;
    }
  }
}


bool
PL_unify_text(term_t term, term_t tail, PL_chars_t *text, int type)
{ GET_LD
  bool rc;

  PL_STRINGS_MARK();
  rc = unify_text(term, tail, text, type);
  PL_STRINGS_RELEASE();
  return rc;
}


static const void*
PL_seek_text_from(const PL_chars_t *t, const void *start, ssize_t offset)
{ if ( offset == 0 )
    return start;

  switch(t->encoding)
  { case ENC_ISO_LATIN_1:
    { const char *s = start;
      s += offset;
      if ( s < t->text.t || s > t->text.t+t->length )
	return NULL;
      return s;
    }
    case ENC_WCHAR:
    {
#if SIZEOF_WCHAR_T == 2
      const wchar_t *s = start;

      if ( offset > 0 )
      { const wchar_t *e = &t->text.w[t->length];

	while ( offset-- > 0 && s < e )
	{ int c = *s++;
	  if ( IS_UTF16_LEAD(c) )
	    s++;
	}
	return s <= e ? s : NULL;
      } else
      { while ( offset++ < 0 && s > t->text.w )
	{ int c = *--s;
	  if ( IS_UTF16_TRAIL(c) )
	    s--;
	}
	return offset == 0 ? s : NULL;
      }
#else
      const wchar_t *s = start;

      s += offset;
      if ( s < t->text.w || s > t->text.w+t->length )
	return NULL;
      return s;
#endif
    }
    default:
      assert(0);
      return NULL;
  }
}



static const void*
PL_seek_text(const PL_chars_t *t, size_t offset)
{ return PL_seek_text_from(t, t->text.t, offset);
}


static const void*
PL_seek_text_rev(const PL_chars_t *t, size_t offset)
{ ssize_t off = offset;
  const void *from;

  switch(t->encoding)
  { case ENC_ISO_LATIN_1:
      from = &t->text.t[t->length];
      break;
    case ENC_WCHAR:
      from = &t->text.w[t->length];
      break;
    default:
      assert(0);
      from = NULL;
  }

  return PL_seek_text_from(t, from, -off);
}


int
PL_unify_text_range(term_t term, const PL_chars_t *text,
		    size_t offset, size_t len, int type)
{ const void *as = PL_seek_text(text, offset);
  const void *ae = PL_seek_text_from(text, as, len);

  assert(text->canonical);

  if ( !as || !ae )
    return false;			/* offset > length */

  if ( len == 1 && type == PL_ATOM && ae > as )
  { GET_LD
    int c;

    if ( text->encoding == ENC_ISO_LATIN_1 )
    { const unsigned char *s = as;
      c = s[0];
    } else
    { const wchar_t *s = as;
      get_wchar(s, &c);
    }

    return PL_unify_atom(term, codeToAtom(c));
  }

  PL_chars_t sub;
  sub.storage = text->storage == PL_CHARS_PROLOG_STACK ? PL_CHARS_PROLOG_STACK : PL_CHARS_HEAP;
  if ( text->encoding == ENC_ISO_LATIN_1 )
  { sub.text.t    = (char*)as;
    sub.length    = (const char*)ae - (const char *)as;
    sub.encoding  = ENC_ISO_LATIN_1;
    sub.canonical = true;
  } else
  { sub.text.w    = (wchar_t*)as;
    sub.length    = (const wchar_t*)ae - (const wchar_t*)as;
    sub.encoding  = ENC_WCHAR;
    sub.canonical = false;
  }

  int rc = PL_unify_text(term, 0, &sub, type);
  PL_free_text(&sub);
  return rc;
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
    { Buffer b = findBuffer(BUF_STACK);
      const unsigned char *s = (const unsigned char *)text->text.t;
      const unsigned char *e = &s[text->length];

      for( ; s<e; s++)
	addBuffer(b, *s, pl_wchar_t);
      addBuffer(b, EOS, pl_wchar_t);

      text->text.w   = baseBuffer(b, pl_wchar_t);
      text->encoding = ENC_WCHAR;
      text->storage  = PL_CHARS_STACK;
    }
  }

  succeed;
}


static bool
PL_demote_text(PL_chars_t *text, int flags)
{ if ( text->encoding != ENC_ISO_LATIN_1 )
  { if ( text->storage == PL_CHARS_MALLOC )
    { char *new = PL_malloc(sizeof(char)*(text->length+1));
      char *t = new;
      const pl_wchar_t *s = (const pl_wchar_t *)text->text.t;
      const pl_wchar_t *e = &s[text->length];

      while(s<e)
      { if ( *s > 0xff )
	{ PL_free(new);
	reperr:
	  if ( (flags&CVT_EXCEPTION) )
	    return text_representation_error(text, ENC_ISO_LATIN_1);
	  return false;
	}
	*t++ = (char)(*s++ & 0xff);
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
	  goto reperr;
	*t++ = (char)(*f++ & 0xff);
      }
      *t = EOS;
      text->encoding = ENC_ISO_LATIN_1;
    } else
    { Buffer b = findBuffer(BUF_STACK);
      const pl_wchar_t *s = (const pl_wchar_t*)text->text.w;
      const pl_wchar_t *e = &s[text->length];

      for( ; s<e; s++)
      { if ( *s > 0xff )
	{ unfindBuffer(b, BUF_STACK);
	  goto reperr;
	}
	addBuffer(b, (char)(*s&0xff), char);
      }
      addBuffer(b, EOS, char);

      text->text.t   = baseBuffer(b, char);
      text->storage  = PL_CHARS_STACK;
      text->encoding = ENC_ISO_LATIN_1;
    }
  }

  text->canonical = true;
  return true;
}


static int
can_demote(PL_chars_t *text)
{ if ( text->encoding != ENC_ISO_LATIN_1 )
  { const pl_wchar_t *w = (const pl_wchar_t*)text->text.w;
    const pl_wchar_t *e = &w[text->length];

    for(; w<e; w++)
    { if ( *w > 0xff )
	return false;
    }
  }

  return true;
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

    return true;
  }

  return false;				/* cannot represent */
}


static void
utf8tobuffer(int c, Buffer buf)
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
  { Buffer b = findBuffer(BUF_STACK);

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
	    { unfindBuffer(b, BUF_STACK);
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
	{ return PL_demote_text(text, flags);
	} else
	{ const pl_wchar_t *w = (const pl_wchar_t*)text->text.w;
	  const pl_wchar_t *e = &w[text->length];

	  if ( target == ENC_UTF8 )
	  { while( w < e )
	    { int c;

	      w = get_wchar(w, &c);
	      utf8tobuffer(c, b);
	    }
	    addBuffer(b, 0, char);
	  } else /* if ( target == ENC_MB ) */
	  { mbstate_t mbs;

	    memset(&mbs, 0, sizeof(mbs));
	    for( ; w<e; w++)
	    { if ( !wctobuffer(*w, &mbs, b) )
	      { unfindBuffer(b, BUF_STACK);
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

    PL_free_text(text);

    text->length    = sizeOfBuffer(b)-1;
    text->text.t    = baseBuffer(b, char);
    text->encoding  = target;
    text->canonical = (target == ENC_ISO_LATIN_1);
    text->storage   = PL_CHARS_STACK;
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


static void
flip_shorts(unsigned char *s, size_t len)
{ unsigned char *e = s+len;

  for(; s<e; s+=2)
  { unsigned char t = s[0];
    s[0] = s[1];
    s[1] = t;
  }
}


static int
native_byte_order(IOENC enc)
{
#ifdef WORDS_BIGENDIAN
  return enc == ENC_UTF16BE;
#else
  return enc == ENC_UTF16LE;
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_canonicalise_text()  recodes  the  text  to   one  of  the  canonical
encodings (ENC_ISO_LATIN_1 or ENC_WCHAR). If   the encoding is ENC_WCHAR
we also make sure it  consists  of   valid  Unicode  code points and, if
sizeof(wchar_t) == 2 (Windows) UTF-16 surrogate pairs are valid.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_canonicalise_text(PL_chars_t *text)
{ if ( !text->canonical )
  { switch(text->encoding )
    { case ENC_OCTET:
	text->encoding = ENC_ISO_LATIN_1;
      case ENC_ISO_LATIN_1:
	text->canonical = true;
	break;				/* nothing to do */
      case ENC_WCHAR:
      { const pl_wchar_t *w;
	const pl_wchar_t *e;
	bool wide;

#if SIZEOF_WCHAR_T == 2
      case_wchar:
#endif
	w = (const pl_wchar_t*)text->text.w;
	e = &w[text->length];
	wide = false;

#if SIZEOF_WCHAR_T == 2
	for(; w<e; w++)
	{ if ( *w > 0xff )
	    wide = true;
	  if ( IS_UTF16_LEAD(*w) )
	  { if ( w+1<e && IS_UTF16_TRAIL(w[1]) )
	      w++;
	    else
	      return ERR_TEXT_ILLEGAL_UTF16;
	  }
	}
#else
	for(; w<e; w++)
	{ if ( *w > 0xff )
	    wide = true;
	  if ( !VALID_CODE_POINT(*w) )
	    return ERR_TEXT_INVALID_CODE_POINT;
	}
#endif
	if ( !wide )
	  return PL_demote_text(text, 0);

	text->canonical = true;
	return true;
      }
      case ENC_UTF16LE:		/* assume text->length is in bytes */
      case ENC_UTF16BE:
      { if ( text->length%2 != 0 )
	  return ERR_TEXT_INCOMPLETE_MULTIBYTE_SEQUENCE;
#if SIZEOF_WCHAR_T == 2
	if ( !native_byte_order(text->encoding) )
	{ if ( text->storage == PL_CHARS_HEAP )
	    PL_save_text(text, BUF_MALLOC);
	  flip_shorts((unsigned char*)text->text.t, text->length);
	}
	text->encoding = ENC_WCHAR;
	text->length /= sizeof(wchar_t);
	goto case_wchar;
#else /*SIZEOF_WCHAR_T!=2*/
	size_t len = text->length/sizeof(short);
	size_t code_points = 0;
	const unsigned short *w, *e;
	int wide = false;

	if ( !native_byte_order(text->encoding) )
	{ if ( text->storage == PL_CHARS_HEAP )
	    PL_save_text(text, BUF_MALLOC);
	  flip_shorts((unsigned char*)text->text.t, text->length);
	}

	w = (const unsigned short *)text->text.t;
	e = &w[len];
	for(; w<e; w++)
	{ code_points++;

	  if ( *w > 0xff )
	    wide = true;
	  if ( IS_UTF16_LEAD(*w) )
	  { if ( w+1<e && IS_UTF16_TRAIL(w[1]) )
	      w++;
	    else
	      return ERR_TEXT_ILLEGAL_UTF16;
	  }
	}
	w = (const unsigned short*)text->text.t;

	if ( wide )
	{ pl_wchar_t *t, *to = PL_malloc(sizeof(pl_wchar_t)*(code_points+1));

	  for(t=to; w<e; )
	  { if ( IS_UTF16_LEAD(*w) )
	    { *t++ = utf16_decode(w[0], w[1]);
	      w++;
	    } else
	    { *t++ = *w++;
	    }
	  }
	  *t = EOS;

	  text->length = len;
	  text->encoding = ENC_WCHAR;
	  if ( text->storage == PL_CHARS_MALLOC )
	    PL_free(text->text.t);
	  else
	    text->storage  = PL_CHARS_MALLOC;

	  text->text.w = to;
	} else
	{ unsigned char *t, *to = PL_malloc(code_points+1);

	  for(t=to; w<e; )
	    *t++ = (unsigned char)*w++;
	  *t = EOS;

	  text->length = len;
	  text->encoding = ENC_ISO_LATIN_1;
	  if ( text->storage == PL_CHARS_MALLOC )
	    PL_free(text->text.t);
	  else
	    text->storage = PL_CHARS_MALLOC;

	  text->text.t = (char*)to;
	}

	return true;
#endif /*SIZEOF_WCHAR_T==2*/
      }
      case ENC_UTF8:
      { const char *s = text->text.t;
	const char *e = &s[text->length];

	while(s<e && !(*s & 0x80))
	  s++;
	if ( s == e )
	{ text->encoding  = ENC_ISO_LATIN_1;
	  text->canonical = true;
	} else
	{ int chr;
	  int wide = false;
	  size_t len = s - text->text.t;

	  while(s<e)
	  { PL_utf8_code_point(&s, e, &chr);
	    if ( chr > 0xff )		/* requires wide characters */
	      wide = true;
	    len++;
#if SIZEOF_WCHAR_T == 2
	    if ( chr > 0xffff )
	      len++;
#endif
	  }

	  s = (const char *)text->text.t;
	  text->length = len;

	  if ( wide )
	  { pl_wchar_t *t, *to = PL_malloc(sizeof(pl_wchar_t)*(len+1));

	    for(t=to; s<e; )
	    { PL_utf8_code_point(&s, e, &chr);
	      t = put_wchar(t, chr);
	    }
	    *t = EOS;

	    text->encoding = ENC_WCHAR;
	    if ( text->storage == PL_CHARS_MALLOC )
	      PL_free(text->text.t);
	    text->text.w  = to;
	    text->storage = PL_CHARS_MALLOC;
	  } else
	  { char *t, *to = PL_malloc(len+1);

	    for(t=to; s<e;)
	    { PL_utf8_code_point(&s, e, &chr);
	      *t++ = (char)chr;
	    }
	    *t = EOS;

	    text->encoding = ENC_ISO_LATIN_1;
	    if ( text->storage == PL_CHARS_MALLOC )
	      PL_free(text->text.t);
	    text->text.t  = to;
	    text->storage = PL_CHARS_MALLOC;
	  }

	  text->canonical = true;
	}

	return true;
      }
      case ENC_ANSI:
      { mbstate_t mbs;
	size_t len = 0;
	int wide = false;
	char *s = text->text.t;
	size_t rc, n = text->length;
	wchar_t wc;

	memset(&mbs, 0, sizeof(mbs));
	while( n > 0 )
	{ if ( (rc=mbrtowc(&wc, s, n, &mbs)) == (size_t)-1 || rc == 0)
	    return ERR_TEXT_ILLEGAL_MULTIBYTE_SEQUENCE;

	  if ( wc > 0xff )
	    wide = true;
	  len++;
#if SIZEOF_WCHAR_T == 2
	  if ( wc > 0xffff )
	    len++;
#endif
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

	  if ( !wide )
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

	      to = put_wchar(to, wc);
	      n -= rc;
	      from += rc;
	    }
	    *to = EOS;
	  }

	  text->length = len;
	  text->canonical = true;
	  if ( do_free )
	    PL_free(do_free);

	  return true;
	}

	fail;
      }
      default:
	assert(0);
    }
  }

  return true;
}


static bool
text_representation_error(PL_chars_t *text, IOENC enc)
{ char msg[100];

  Ssnprintf(msg, sizeof(msg), "cannot represent text using encoding %s",
	    PL_atom_chars(PL_encoding_to_atom(enc)));

  return PL_error(NULL, 0, msg, ERR_REPRESENTATION, ATOM_encoding);
}


static int
text_error(PL_chars_t *text, int rc)
{ (void)text;

  switch(rc)
  { case ERR_TEXT_ILLEGAL_UTF8:
      return PL_syntax_error("illegal_utf8_sequence", NULL);
    case ERR_TEXT_ILLEGAL_UTF16:
      return PL_syntax_error("illegal_utf16_sequence", NULL);
    case ERR_TEXT_ILLEGAL_MULTIBYTE_SEQUENCE:
      return PL_syntax_error("illegal_multibyte_sequence", NULL);
    case ERR_TEXT_INCOMPLETE_MULTIBYTE_SEQUENCE:
      return PL_syntax_error("incomplete_multibyte_sequence", NULL);
    case ERR_TEXT_INVALID_CODE_POINT:
      return PL_representation_error("code_point");
    default:
      assert(0);
      return false;
  }
}

int
PL_canonicalise_text_ex(PL_chars_t *text)
{ int rc;

  if ( (rc=PL_canonicalise_text(text)) == true )
    return true;

  return text_error(text, rc);
}


void
PL_free_text(PL_chars_t *text)
{ if ( text->storage == PL_CHARS_MALLOC && text->text.t )
    PL_free(text->text.t);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Recode a text to the given   encoding. Currrenly only supports re-coding
to UTF-8 for ENC_ASCII, ENC_ISO_LATIN_1, ENC_WCHAR and ENC_ANSI.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
addUTF16Buffer_unit(Buffer b, int c, IOENC enc)
{ if ( native_byte_order(enc) )
  { addBuffer(b, (unsigned short)c, unsigned short);
  } else
  { union
    { unsigned short s;
      char  c[2];
    } swap;

    swap.s = (unsigned short)c;
    char t = swap.c[0];
    swap.c[0] = swap.c[1];
    swap.c[1] = t;
    addBuffer(b, swap.s, unsigned short);
  }
}


static void
addUTF16Buffer(Buffer b, int c, IOENC enc)
{ if ( c > 0xffff )
  { int l, t;

    utf16_encode(c, &l, &t);
    addUTF16Buffer_unit(b, l, enc);
    addUTF16Buffer_unit(b, t, enc);
  } else
    addUTF16Buffer_unit(b, c, enc);
}


int
PL_text_recode(PL_chars_t *text, IOENC encoding)
{ if ( text->encoding != encoding )
  { switch(encoding)
    { case ENC_UTF8:
      { Buffer b;

	switch(text->encoding)
	{ case ENC_ASCII:
	    text->encoding = ENC_UTF8;
	    break;
	  case ENC_ISO_LATIN_1:
	  { const unsigned char *s = (const unsigned char *)text->text.t;
	    const unsigned char *e = &s[text->length];

	    for( ; s<e; s++)
	    { if ( *s&0x80 )
	      { s = (const unsigned char *)text->text.t;
		goto convert_utf8;
	      }
	    }
					/* ASCII; nothing to do */
	    text->encoding = ENC_UTF8;
	    break;

	  convert_utf8:
	    b = findBuffer(BUF_STACK);
	    for( ; s<e; s++)
	      utf8tobuffer(*s, b);
	  swap_to_utf8:
	    PL_free_text(text);
	    text->length   = entriesBuffer(b, char);
	    addBuffer(b, EOS, char);
	    text->text.t   = baseBuffer(b, char);
	    text->encoding = ENC_UTF8;
	    text->storage  = PL_CHARS_STACK;

	    break;
	  }
	  case ENC_WCHAR:
	  { const pl_wchar_t *s = text->text.w;
	    const pl_wchar_t *e = &s[text->length];

	    b = findBuffer(BUF_STACK);
	    while( s < e )
	    { int c;

	      s = get_wchar(s, &c);
	      utf8tobuffer(c, b);
	    }
	    goto swap_to_utf8;
	  }
	  case ENC_ANSI:
	  { mbstate_t mbs;
	    size_t rc, n = text->length;
	    wchar_t wc;
	    const char *s = (const char *)text->text.t;

	    b = findBuffer(BUF_STACK);
	    memset(&mbs, 0, sizeof(mbs));
	    while( n > 0 )
	    { if ( (rc=mbrtowc(&wc, s, n, &mbs)) == (size_t)-1 || rc == 0)
		return false;		/* encoding error */

	      utf8tobuffer(wc, b);
	      n -= rc;
	      s += rc;
	    }
	    if ( n == 0 )
	      goto swap_to_utf8;

	    return false;
	  }
	  default:
	    assert(0);
	    return false;
	}
	return true;
	case ENC_ISO_LATIN_1:		/* --> ISO Latin 1 */
	case ENC_OCTET:			/* --> bytes */
	case ENC_ASCII:			/* --> ASCII */
	{ assert(text->canonical);
	  switch(text->encoding)
	  { case ENC_WCHAR:
	      return text_representation_error(text, encoding);
	    case ENC_ISO_LATIN_1:
	      if ( encoding == ENC_ASCII )
	      { const unsigned char *s =(const unsigned char *)text->text.t;
		const unsigned char *e = &s[text->length];

		while(s<e)
		{ int c = *s++;
		  if ( c >= 128 )
		    return text_representation_error(text, encoding);
		}
	      }
	      text->canonical = false;
	      text->encoding = encoding;

	      return true;
	    default:
	      assert(0);
	      return false;
	  }
	}
	case ENC_UTF16LE:		/* --> UTF-16 */
	case ENC_UTF16BE:
	{ Buffer b;

	  assert(text->canonical);
	  switch(text->encoding)
	  { case ENC_ISO_LATIN_1:
	    { b = findBuffer(BUF_STACK);
	      const unsigned char *s = (const unsigned char *)text->text.t;
	      const unsigned char *e = &s[text->length];

	      for( ; s<e; s++)
		addUTF16Buffer(b, *s, encoding);

	    swap_to_utf16:
	      PL_free_text(text);
	      text->length   = entriesBuffer(b, char);
	      addBuffer(b, EOS, short);
	      text->text.t   = baseBuffer(b, char);
	      text->encoding = encoding;
	      text->storage  = PL_CHARS_STACK;

	      return true;
	    }
	    case ENC_WCHAR:
	    { b = findBuffer(BUF_STACK);
	      const wchar_t *s = text->text.w;
	      const wchar_t *e = &s[text->length];

	      for( ; s<e; s++)
		addUTF16Buffer(b, *s, encoding);
	      goto swap_to_utf16;
	    }
	    default:
	      assert(0);
	      return false;
	  }
	}
	case ENC_ANSI:
	{ assert(text->canonical);
	  return PL_mb_text(text, REP_MB);
	}
	default:
	  assert(0);
	  return false;
      }
    }
  } else
    return true;
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
{ const void *s1 = PL_seek_text(t1, o1);
  const void *s2 = PL_seek_text(t2, o2);

#define CMP_LENGTH \
	if ( len-- == 0 ) return CMP_EQUAL; \
	if ( (const void*)s >= e1 ) return CMP_LESS; \
	if ( (const void*)q >= e2 ) return CMP_GREATER
#define CMP_CHARS \
	if ( c1 != c2 ) \
	  return (c1 > c2 ? CMP_GREATER : CMP_LESS)

  if ( s1 && s2 )
  { const void *e1 = PL_seek_text_rev(t1, 0);
    const void *e2 = PL_seek_text_rev(t2, 0);

    if ( t1->encoding == ENC_ISO_LATIN_1 && t2->encoding == ENC_ISO_LATIN_1 )
    { const unsigned char *s = s1;
      const unsigned char *q = s2;

      for(;;)
      { int c1, c2;

	CMP_LENGTH;
	c1 = *s++;
	c2 = *q++;
	CMP_CHARS;
      }
    } else if ( t1->encoding == ENC_WCHAR && t2->encoding == ENC_WCHAR )
    { const pl_wchar_t *s = s1;
      const pl_wchar_t *q = s2;

      for(;;)
      { int c1, c2;

	CMP_LENGTH;
	s = get_wchar(s, &c1);
	q = get_wchar(q, &c2);
	CMP_CHARS;
      }
    } else if ( t1->encoding == ENC_ISO_LATIN_1 && t2->encoding == ENC_WCHAR )
    { const unsigned char *s = s1;
      const pl_wchar_t *q = s2;

      for(;;)
      { int c1, c2;

	CMP_LENGTH;
	c1 = *s++;
	q = get_wchar(q, &c2);
	CMP_CHARS;
      }
    } else
    { const pl_wchar_t *s = s1;
      const unsigned char *q = s2;

      for(;;)
      { int c1, c2;

	CMP_LENGTH;
	s = get_wchar(s, &c1);
	c2 = *q++;
	CMP_CHARS;
      }
    }
  } else if ( s1 /*&& !s2*/ )
  { return CMP_GREATER;
  } else if ( s2 /*&& !s1*/ )
  { return CMP_LESS;
  } else
  { return CMP_EQUAL;
  }
}


int
PL_concat_text(int n, PL_chars_t **text, PL_chars_t *result)
{ size_t total_length = 0;
  int latin = true;
  int i;

  for(i=0; i<n; i++)
  { if ( latin && !can_demote(text[i]) )
      latin = false;
    total_length += text[i]->length;
  }

  result->canonical = true;
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

  return true;
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
