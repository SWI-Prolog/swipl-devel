/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>
#include <errno.h>

		 /*******************************
		 *      FILE --> IOSTREAM	*
		 *******************************/

static int
Sread_FILE(void *handle, char *buf, int size)
{ FILE *fd = handle;

  return fread(buf, sizeof(char), size, fd);
}


static int
Swrite_FILE(void *handle, char *buf, int size)
{ FILE *fd = handle;

  return fwrite(buf, sizeof(char), size, fd);
}


static long
Sseek_FILE(void *handle, long pos, int whence)
{ FILE *fd = handle;

  if ( fseek(fd, pos, whence) == 0 )
    return ftell(fd);

  return -1;
}


static int
Sclose_FILE(void *handle)
{ return 0;
}


static IOFUNCTIONS SFILEfunctions =
{ Sread_FILE,
  Swrite_FILE,
  Sseek_FILE,
  Sclose_FILE
};



IOSTREAM *
Sopen_FILE(FILE *fd, int flags)
{ return Snew(fd, flags, &SFILEfunctions);
}

		 /*******************************
		 *      OBJECT --> IOSTREAM	*
		 *******************************/

typedef struct
{ Any	object;				/* The client (opened) object */
  long	point;				/* Current location */
} open_object, *OpenObject;


static int
Sread_object(void *handle, char *buf, int size)
{ OpenObject h = handle;
  Any argv[2];
  CharArray sub;
  int chread;

  if ( isFreedObj(h->object) )
  { errno = EIO;
    return -1;
  }

  argv[0] = toInt(h->point);
  argv[1] = toInt(size);

  if ( (sub = getv(h->object, NAME_readAsFile, 2, argv)) &&
       instanceOfObject(sub, ClassCharArray) )
  { chread = sub->data.size;

    assert(chread <= size);
    memcpy(buf, sub->data.s_text8, chread);
    h->point += chread;
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

  if ( isFreedObj(h->object) )
  { errno = EIO;
    return -1;
  }

  str_inithdr(&s, ENC_ASCII);
  s.size     = size;
  s.s_text8  = (unsigned char *)buf;

  ca = StringToScratchCharArray(&s);
  if ( (rval = send(h->object, NAME_writeAsFile, where, ca, 0)) )
    h->point += size;
  doneScratchCharArray(ca);

  if ( rval )
    return size;

  errno = EIO;
  return -1;
}


static long
Sseek_object(void *handle, long pos, int whence)
{ OpenObject h = handle;
  Int size;

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
	   (size = get(h->object, NAME_sizeAsFile, 0)) )
      { h->point = valInt(size) - pos;
	break;
      } else
      { errno = EPIPE;		/* better idea? */
	return -1;
      }
    }
    default:
    { errno = EINVAL;
      return -1;
    }
  }

  return h->point;
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

    if ( (s=Sopen_file(strName(name), mode)) )
      return s;
    
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

    return Snew(h, flags, &Sobjectfunctions);
  }
}
