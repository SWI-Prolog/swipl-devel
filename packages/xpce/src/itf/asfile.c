/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>
#include <fcntl.h>
#include <h/interface.h>
#include <errno.h>

typedef struct pce_file_handle * PceFileHandle;

struct pce_file_handle
{ Any		object;			/* object `filified' */
  long		point;			/* current position */
  int		flags;			/* general flags field */
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open flags recognised:

	PCE_RDONLY	Reading only
	PCE_WRONLY	Writing only
	PCE_RDWR	Reading and writing
	PCE_APPEND	Keep appending
	PCE_TRUNC	Tuncate object prioir to writing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceFileHandle *handles;		/* array of handles */
static int max_handles=0;		/* # handles allocated */

static int
allocFileHandle()
{ int handle;

  for(handle = 0; handle < max_handles; handle++)
    if ( handles[handle] == NULL )
      return handle;

  { PceFileHandle *newhandles;
    int n;

    if ( max_handles == 0 )
    { n = 16;
      newhandles = malloc(sizeof(PceFileHandle) * n);
    } else
    { n = max_handles*2;
      newhandles = realloc(handles, sizeof(PceFileHandle) * n);
    }

    if ( newhandles )
    { int rval = max_handles;

      memset(&newhandles[max_handles], 0,
	     sizeof(PceFileHandle) * (n-max_handles));
      max_handles = n;
      handles = newhandles;
      
      return rval;
    }
    
    errno = ENOMEM;
    return -1;
  }
}


int
pceOpen(Any obj, int flags)
{ int handle = allocFileHandle();
  PceFileHandle h;

  if ( handle < 0 )
  { errno = ENOSR;
    return -1;
  }

  if ( !isProperObject(obj) )
  { errno = EINVAL;
    return -1;
  }

  if ( flags & (PCE_WRONLY|PCE_RDWR) )
  { if ( (flags & PCE_APPEND) )
    { if ( !hasSendMethodObject(obj, NAME_append) )
      { errno = EACCES;
	return -1;
      }
    } else
    { if ( !hasSendMethodObject(obj, NAME_insert) )
      { errno = EACCES;
	return -1;
      }
    }

    if ( flags & PCE_TRUNC )
    { if ( !hasSendMethodObject(obj, NAME_clear) ||
	   !send(obj, NAME_clear, 0) )
      { errno = EACCES;
	return -1;
      }
    }
  }

  h = alloc(sizeof(struct pce_file_handle));
  h->object = obj;
  addRefObj(obj);			/* so existence check is safe */
  h->flags = flags;
  h->point = 0L;
  handles[handle] = h;

  return handle;
}


int
pceClose(int handle)
{ PceFileHandle h;

  if ( handle >= 0 && handle < max_handles &&
       (h = handles[handle]) )
  { delRefObject(NIL, h->object);	/* handles deferred unalloc() */
    unalloc(sizeof(struct pce_file_handle), h);
    handles[handle] = NULL;

    return 0;
  }

  errno = EBADF;
  return -1;
}


int
pceWrite(int handle, const char *buf, int size)
{ PceFileHandle h;

  if ( handle >= 0 && handle < max_handles &&
       (h = handles[handle]) &&
       h->flags & (PCE_RDWR|PCE_WRONLY) )
  { string s;
    CharArray ca;
    status rval;

    s.s_text8  = (char *)buf;
    s.b16      = FALSE;
    s.size     = size;
    s.encoding = ENC_ASCII;

    ca = StringToScratchCharArray(&s);

    if ( h->flags & PCE_APPEND )
      rval = send(h->object, NAME_append, ca, 0);
    else
    { if ( (rval = send(h->object, NAME_insert, toInt(h->point), ca, 0)) )
	h->point += size;
    }
    
    doneScratchCharArray(ca);
    if ( rval )
      return size;

    errno = EIO;
    return -1;
  } else
  { errno = EBADF;
    return -1;
  }
}


long
pceSeek(int handle, long offset, int whence)
{ PceFileHandle h;

  if ( handle >= 0 && handle < max_handles &&
       (h = handles[handle]) &&
       h->flags & (PCE_RDWR|PCE_RDONLY) )
  { switch(whence)
    { case PCE_SEEK_SET:
	h->point = offset;
        return h->point;
      case PCE_SEEK_CUR:
	h->point += offset;
        if ( h->point < 0 )
	  h->point = 0;
        return h->point;
      case PCE_SEEK_END:
      { Any size;

	if ( hasGetMethodObject(h->object, NAME_size) &&
	     (size = get(h->object, NAME_size, 0)) &&
	     isInteger(size) )
	{ h->point = valInt(size) - offset;
	  if ( h->point < 0 )
	    h->point = 0;
	  return h->point;
	} else
	{ errno = EPIPE;		/* better idea? */
	  return -1;
	}
      }
      default:
	errno = EINVAL;
        return -1;
    }
  } else
  { errno = EBADF;
    return -1;
  }
}


int
pceRead(int handle, char *buf, int size)
{ PceFileHandle h;

  if ( handle >= 0 && handle < max_handles &&
       (h = handles[handle]) &&
       h->flags & (PCE_RDWR|PCE_RDONLY) )
  { Any argv[2];
    CharArray sub;

    argv[0] = toInt(h->point);
    argv[1] = toInt(h->point + size);

    if ( (sub = getv(h->object, NAME_sub, 2, argv)) &&
	 instanceOfObject(sub, ClassCharArray) )
    { int chread = sub->data.size;

      assert(chread <= size);
      memcpy(buf, sub->data.s_text8, chread);
      h->point += chread;

      return chread;
    }

    errno = EIO;
    return -1;
  } else
  { errno = EBADF;
    return -1;
  }
}


const char *
pceOsError()
{ return strName(getOsErrorPce(PCE));
}
