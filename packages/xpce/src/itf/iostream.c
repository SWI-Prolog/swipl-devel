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
#include <h/unix.h>
#include <errno.h>

#undef ENC_WCHAR			/* conflict str.h/stream.h */

		 /*******************************
		 *      OBJECT --> IOSTREAM	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reading and writing to objects is  done   using  the `wchar' encoding of
streams to fully support international character   sets. To simplify the
interface we will translate the size of   the read and write requests to
n/sizeof(wchar_t) and do the translation to/from the buffer here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ Any	object;				/* The client (opened) object */
  long	point;				/* Current location */
  IOENC encoding;			/* used encoding */
} open_object, *OpenObject;


static int
Sread_object(void *handle, char *buf, int size)
{ OpenObject h = handle;
  Any argv[2];
  CharArray sub;
  int chread;
  int advance;

  if ( isFreedObj(h->object) )
  { errno = EIO;
    return -1;
  }

  if ( h->encoding == ENC_WCHAR )
  { advance = size/sizeof(wchar_t);
  } else if ( h->encoding == ENC_OCTET )
  { advance = size;
  } else
  { assert(0);
    errno = EIO;
    return -1;
  }

  argv[0] = toInt(h->point);
  argv[1] = toInt(advance);

  if ( (sub = getv(h->object, NAME_readAsFile, 2, argv)) &&
       instanceOfObject(sub, ClassCharArray) )
  { String s = &sub->data;

    assert(s->size <= advance);

    if ( h->encoding == ENC_WCHAR )
    { if ( isstrA(s) )
      { charW *dest = (charW*)buf;
	const charA *f = s->s_textA;
	const charA *e = &f[s->size];
      
	while(f<e)
	  *dest++ = *f++;
      } else
      { memcpy(buf, s->s_textW, s->size*sizeof(charW));
      }
      chread = s->size * sizeof(wchar_t);
    } else
    { if ( isstrA(s) )
      { memcpy(buf, s->s_textA, s->size);
      } else
      { errno = EIO;
	chread = -1;
      }
      chread = s->size;
    }

    h->point += s->size;
  } else
  { errno = EIO;
    chread = -1;
  }

  return chread;
}


static int
Swrite_object(void *handle, char *buf, int size)
{ OpenObject h = handle;
  string s;
  CharArray ca;
  status rval;
  Int where = toInt(h->point);
  int advance;

  if ( isFreedObj(h->object) )
  { errno = EIO;
    return -1;
  }

  if ( h->encoding == ENC_WCHAR )
  { const wchar_t *wbuf = (const wchar_t*)buf;
    const wchar_t *end = (const wchar_t*)&buf[size];
    const wchar_t *f;

    assert(size%sizeof(wchar_t) == 0);
    advance = size/sizeof(wchar_t);

    for(f=wbuf; f<end; f++)
    { if ( *f > 0xff )
	break;
    }
  
    if ( f == end )
    { charA *asc = alloca(size);
      charA *t = asc;
  
      for(f=wbuf; f<end; )
	*t++ = (charA)*f++;
  
      str_set_n_ascii(&s, advance, asc);
    } else
    { str_set_n_wchar(&s, advance, (wchar_t*)wbuf);
    }
  } else if ( h->encoding == ENC_OCTET )
  { advance = size;
    str_set_n_ascii(&s, size, buf);
  } else
  { assert(0);
    errno = EIO;
    return -1;
  }

  ca = StringToScratchCharArray(&s);

  if ( (rval = send(h->object, NAME_writeAsFile, where, ca, EAV)) )
    h->point += advance;
  doneScratchCharArray(ca);

  if ( rval )
    return size;

  errno = EIO;
  return -1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: pos is measured  in  bytes.  If   we  use  wchar  encoding we must
compensate for this.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long
Sseek_object(void *handle, long pos, int whence)
{ OpenObject h = handle;
  Int size;
  int usize = (h->encoding == ENC_WCHAR ? sizeof(wchar_t) : 1);

  pos /= usize;

  if ( isFreedObj(h->object) )
  { errno = EIO;
    return -1;
  }

  switch(whence)
  { case SIO_SEEK_SET:
      h->point = pos;
      break;
    case SIO_SEEK_CUR:
      h->point += pos;			/* check for end!? */
      break;
    case SIO_SEEK_END:
    { if ( hasGetMethodObject(h->object, NAME_sizeAsFile) &&
	   (size = get(h->object, NAME_sizeAsFile, EAV)) )
      { h->point = valInt(size) - pos;
	break;
      } else
      { errno = EPIPE;			/* better idea? */
	return -1;
      }
    }
    default:
    { errno = EINVAL;
      return -1;
    }
  }

  return h->point * usize;
}


static int
Sclose_object(void *handle)
{ OpenObject h = handle;

  if ( isFreedObj(h->object) )
  { errno = EIO;
    return -1;
  }

  delCodeReference(h->object);
  freeableObj(h->object);

  unalloc(sizeof(*h), h);

  return 0;
}


static IOFUNCTIONS Sobjectfunctions =
{ Sread_object,
  Swrite_object,
  Sseek_object,
  Sclose_object
};


IOSTREAM *
Sopen_object(Any obj, const char *mode)
{ if ( instanceOfObject(obj, ClassFile) )
  { Name name = getOsNameFile(obj);
    IOSTREAM *s;

    if ( (s=Sopen_file(nameToFN(name), mode)) )
    { if ( !strchr(mode, 'b') )
	setStreamEncodingSourceSink(obj, s);
      return s;
    }
    
    errorPce(obj, NAME_openFile,
	     mode[0] == 'r' ? NAME_read : NAME_write,
	     getOsErrorPce(PCE));

    return s;
  } else if ( instanceOfObject(obj, ClassRC) &&
	      TheCallbackFunctions.rc_open )
  { IOSTREAM *s;
    RC rc = obj;
    char *rc_class;

    if ( notDefault(rc->rc_class) )
      rc_class = strName(rc->rc_class);
    else
      rc_class = NULL;

    if ( notNil(rc->context) && TheCallbackFunctions.setHostContext )
    { Any savedcontext =
	(*TheCallbackFunctions.setHostContext)(rc->context);

      s = (*TheCallbackFunctions.rc_open)(strName(rc->name),
					  rc_class,
					  mode);
      (*TheCallbackFunctions.setHostContext)(savedcontext);
    } else
      s = (*TheCallbackFunctions.rc_open)(strName(rc->name),
					  rc_class,
					  mode);
					  
    if ( !s )
      errorPce(obj, NAME_openFile,
	       mode[0] == 'r' ? NAME_read : NAME_write,
	       getOsErrorPce(PCE));

    return s;
  } else
  { int flags = SIO_TEXT|SIO_RECORDPOS;
    OpenObject h;
    IOSTREAM *stream;

    switch(mode[0])
    { case 'r':
	flags |= SIO_INPUT;
        break;
      case 'w':
	flags |= SIO_OUTPUT;
        break;
      default:
	errno = EINVAL;
        return NULL;
    }

    for(mode++; *mode; mode++)
    { switch(*mode)
      { case 'b':			/* binary */
	  flags &= ~SIO_TEXT;
	  break;
	case 'r':			/* no record */
	  flags &= ~SIO_RECORDPOS;
	  break;
	default:
	  errno = EINVAL;
	  return NULL;
      }
    }

    h = alloc(sizeof(*h));
    h->point = 0;
    h->object = obj;
    addCodeReference(obj);

    stream = Snew(h, flags, &Sobjectfunctions);

    if ( (flags&SIO_TEXT) )
      stream->encoding = ENC_WCHAR;	/* see comment above */
    else
      stream->encoding = ENC_OCTET;
    h->encoding = stream->encoding;

    return stream;
  }
}
