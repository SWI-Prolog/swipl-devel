/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>
#include <fcntl.h>
#include <h/interface.h>
#include <errno.h>

typedef struct pce_file_handle * PceFileHandle;

struct pce_file_handle
{ Any		object;			/* object `file-i-fied' */
  long		point;			/* current position */
  int		flags;			/* general flags field */
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open flags recognised:

	PCE_RDONLY	Reading only
	PCE_WRONLY	Writing only
	PCE_RDWR	Reading and writing
	PCE_APPEND	Keep appending
	PCE_TRUNC	Tuncate object prior to writing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceFileHandle *handles;		/* array of handles */
static int max_handles=0;		/* # handles allocated */

static int
allocFileHandle()
{ int handle;

  for(handle = 0; handle < max_handles; handle++)
  { if ( handles[handle] == NULL )
      return handle;
  }

  { PceFileHandle *newhandles;
    int n;

    if ( max_handles == 0 )
    { n = 16;
      newhandles = pceMalloc(sizeof(PceFileHandle) * n);
    } else
    { n = max_handles*2;
      newhandles = pceRealloc(handles, sizeof(PceFileHandle) * n);
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
    return handle;

  if ( !isProperObject(obj) )
  { errno = EINVAL;
    return -1;
  }

  if ( flags & PCE_WRONLY )
  { if ( !hasSendMethodObject(obj, NAME_writeAsFile) )
    { errno = EACCES;
      return -1;
    }

    if ( flags & PCE_TRUNC )
    { if ( !hasSendMethodObject(obj, NAME_truncateAsFile) ||
	   !send(obj, NAME_truncateAsFile, EAV) )
      { errno = EACCES;
	return -1;
      }
    }
  }
  if ( flags & PCE_RDONLY )
  { if ( !hasGetMethodObject(obj, NAME_readAsFile) )
    { errno = EACCES;
      return -1;
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
    Int where = (h->flags & PCE_APPEND ? (Int) DEFAULT : toInt(h->point));

    if ( isFreedObj(h->object) )
    { errno = EIO;
      return -1;
    }

    str_inithdr(&s, ENC_ASCII);
    s.size     = size;
    s.s_text8  = (unsigned char *)buf;

    ca = StringToScratchCharArray(&s);
    if ( (rval = send(h->object, NAME_writeAsFile, where, ca, EAV)) )
      h->point += size;
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

  if ( handle >= 0 && handle < max_handles && (h = handles[handle]) )
  { Int size;

    if ( isFreedObj(h->object) )
    { errno = EIO;
      return -1;
    }

    switch(whence)
    { case PCE_SEEK_SET:
	h->point = offset;
        break;
      case PCE_SEEK_CUR:
        h->point += offset;
        break;
      case PCE_SEEK_END:
      { if ( hasGetMethodObject(h->object, NAME_sizeAsFile) &&
	     (size = get(h->object, NAME_sizeAsFile, EAV)) )
	{ h->point = valInt(size) - offset;
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
  } else
  { errno = EBADF;
    return -1;
  }
}


const char *
pceOsError()
{ return strName(getOsErrorPce(PCE));
}
