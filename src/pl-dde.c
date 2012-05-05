/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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

#define O_DEBUG 1
#ifdef __WINDOWS__

#define INCLUDE_DDEML_H
#include "pl-incl.h"

#if O_DDE
#include <string.h>

#define FASTBUFSIZE 512			/* use local buffer upto here */
#define MAX_CONVERSATIONS 32		/* Max. # of conversations */
#define TIMEOUT_VERY_LONG 0x7fffffff;	/* largest positive int */

static HCONV conv_handle[MAX_CONVERSATIONS];
static HCONV server_handle[MAX_CONVERSATIONS];

static Module	 MODULE_dde;		/* win_dde */
static functor_t FUNCTOR_dde_connect3;
static functor_t FUNCTOR_dde_connect_confirm3;
static functor_t FUNCTOR_dde_disconnect1;
static functor_t FUNCTOR_dde_request4;
static functor_t FUNCTOR_dde_execute3;
static functor_t FUNCTOR_error1;

#define LOCK()   PL_LOCK(L_DDE)
#define UNLOCK() PL_UNLOCK(L_DDE)

static const char *
dde_error_message(int errn)
{ GET_LD
  const char *err;

  if ( errn <= 0 )
    errn = DdeGetLastError(LD->os.dde_instance);

  switch(errn)
  { case DMLERR_ADVACKTIMEOUT:
    case DMLERR_DATAACKTIMEOUT:
    case DMLERR_EXECACKTIMEOUT:
    case DMLERR_POKEACKTIMEOUT:
    case DMLERR_UNADVACKTIMEOUT:	err = "Timeout";		break;
    case DMLERR_BUSY:			err = "Service busy";		break;
    case DMLERR_DLL_NOT_INITIALIZED:	err = "DDL not initialised";	break;
    case DMLERR_INVALIDPARAMETER:	err = "Invalid parameter";	break;
    case DMLERR_MEMORY_ERROR:		err = "Memory error";		break;
    case DMLERR_NO_CONV_ESTABLISHED:	err = "No conversation";	break;
    case DMLERR_NO_ERROR:		err = "No error???";		break;
    case DMLERR_NOTPROCESSED:		err = "Not processed";		break;
    case DMLERR_POSTMSG_FAILED:		err = "PostMessage() failed";	break;
    case DMLERR_REENTRANCY:		err = "Reentrance";		break;
    case DMLERR_SERVER_DIED:		err = "Server died";		break;
    default:				err = "Unknown error";		break;
  }

  return err;
}


static int
dde_warning(const char *cmd)
{ const char *err = dde_error_message(-1);

  return PL_error(NULL, 0, NULL, ERR_DDE_OP, cmd, err);
}


static int
unify_hsz(DWORD ddeInst, term_t term, HSZ hsz)
{ wchar_t buf[FASTBUFSIZE];
  int len;

  if ( !(len=DdeQueryStringW(ddeInst, hsz, buf,
			     sizeof(buf)/sizeof(wchar_t)-1, CP_WINUNICODE)) )
  { dde_warning("string handle");
    return NULL_ATOM;
  }

  if ( len == sizeof(buf)/sizeof(wchar_t)-1 )
  { if ( (len=DdeQueryStringW(ddeInst, hsz, NULL, 0, CP_WINUNICODE)) > 0 )
    { wchar_t *b2;
      int rc;

      if ( !(b2 = malloc((len+1)*sizeof(wchar_t))) )
	return PL_no_memory();

      DdeQueryStringW(ddeInst, hsz, b2, len+1, CP_WINUNICODE);
      rc = PL_unify_wchars(term, PL_ATOM, len, b2);
      free(b2);

      return rc;
    }

    dde_warning("string handle");
  }

  return PL_unify_wchars(term, PL_ATOM, len, buf);
}


static word
unify_hdata(term_t t, HDDEDATA data)
{ BYTE buf[FASTBUFSIZE];
  DWORD len;

  if ( !(len=DdeGetData(data, buf, sizeof(buf), 0)) )
    return dde_warning("data handle");

  DEBUG(1, Sdprintf("DdeGetData() returned %ld bytes\n", (long)len));

  if ( len == sizeof(buf) )
  { if ( (len=DdeGetData(data, NULL, 0, 0)) > 0 )
    { LPBYTE b2;
      int rval;

      if ( !(b2 = malloc(len)) )
	return PL_no_memory();

      DdeGetData(data, b2, len, 0);
      rval = PL_unify_wchars(t, PL_ATOM, len/sizeof(wchar_t)-1, (wchar_t*)b2);
      free(b2);

      return rval;
    }

    return dde_warning("data handle");
  }

  return PL_unify_wchars(t, PL_ATOM, len/sizeof(wchar_t)-1, (wchar_t*)buf);
}


static int
get_hsz(DWORD ddeInst, term_t data, HSZ *rval)
{ wchar_t *s;
  size_t len;

  if ( PL_get_wchars(data, &len, &s, CVT_ALL|CVT_EXCEPTION) )
  { HSZ h;

    assert(s[len] == 0);			/* Must be 0-terminated */

    DEBUG(2, Sdprintf("Get HSZ for %Ws ...\n", s));
    if ( (h=DdeCreateStringHandleW(ddeInst, s, CP_WINUNICODE)) )
    { DEBUG(2, Sdprintf("\tHSZ = %p\n", h));
      *rval = h;
      succeed;
    }

    return PL_error(NULL, 0, WinError(), ERR_SYSCALL, "DdeCreateStringHandleW");
  }

  fail;
}


static int
allocServerHandle(HCONV handle)
{ int i;

  LOCK();
  for(i=0; i<MAX_CONVERSATIONS; i++)
  { if ( !server_handle[i] )
    { server_handle[i] = handle;
      break;
    }
  }
  UNLOCK();

  if ( i<MAX_CONVERSATIONS )
    return i;

  PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_max_dde_handles);
  return -1;
}


static int
findServerHandle(HCONV handle)
{ int i;

  for(i=0; i<MAX_CONVERSATIONS; i++)		/* can be unlocked */
  { if ( server_handle[i] == handle )
      return i;
  }

  return -1;
}


static HDDEDATA CALLBACK
DdeCallback(UINT type, UINT fmt, HCONV hconv, HSZ hsz1, HSZ hsz2,
            HDDEDATA hData, DWORD dwData1, DWORD dwData2)
{ GET_LD
  DWORD ddeInst = LD->os.dde_instance;

  switch(type)
  {  case XTYP_CONNECT:
     { fid_t cid = PL_open_foreign_frame();
       term_t argv = PL_new_term_refs(3);
       predicate_t pred = PL_pred(FUNCTOR_dde_connect3, MODULE_dde);
       int rval;

       if ( unify_hsz(ddeInst, argv+0, hsz2) &&		/* topic */
	    unify_hsz(ddeInst, argv+1, hsz1) &&		/* service */
	    PL_unify_integer(argv+2, dwData2 ? 1 : 0) )	/* same instance */
       { rval = PL_call_predicate(MODULE_dde, TRUE, pred, argv);
       } else
       { rval = FALSE;
       }
       PL_discard_foreign_frame(cid);

       return (void *)(intptr_t)rval;
     }
     case XTYP_CONNECT_CONFIRM:
     { int plhandle;

       if ( (plhandle = allocServerHandle(hconv)) >= 0 )
       { fid_t cid = PL_open_foreign_frame();
	 term_t argv = PL_new_term_refs(3);
	 predicate_t pred = PL_pred(FUNCTOR_dde_connect_confirm3, MODULE_dde);

	 if ( unify_hsz(ddeInst, argv+0, hsz2) &&		/* topic */
	      unify_hsz(ddeInst, argv+1, hsz1) &&		/* service */
	      PL_unify_integer(argv+2, plhandle) )
	   PL_call_predicate(MODULE_dde, TRUE, pred, argv);

	 PL_discard_foreign_frame(cid);
       }

       return NULL;
     }
     case XTYP_DISCONNECT:
     { fid_t cid = PL_open_foreign_frame();
       term_t argv = PL_new_term_refs(1);
       predicate_t pred = PL_pred(FUNCTOR_dde_disconnect1, MODULE_dde);
       int plhandle = findServerHandle(hconv);

       if ( plhandle >= 0 && plhandle < MAX_CONVERSATIONS )
	 server_handle[plhandle] = (HCONV)NULL;

       PL_put_integer(argv+0, plhandle);
       PL_call_predicate(MODULE_dde, TRUE, pred, argv);
       PL_discard_foreign_frame(cid);

       return NULL;
     }
     case XTYP_EXECUTE:
     { int plhandle = findServerHandle(hconv);
       HDDEDATA rval = DDE_FNOTPROCESSED;
       fid_t cid = PL_open_foreign_frame();
       term_t argv = PL_new_term_refs(3);
       predicate_t pred = PL_pred(FUNCTOR_dde_execute3, MODULE_dde);

       DEBUG(1, Sdprintf("Got XTYP_EXECUTE request\n"));

       PL_put_integer(argv+0, plhandle);
       unify_hsz(ddeInst, argv+1, hsz1);
       unify_hdata(   argv+2, hData);
       if ( PL_call_predicate(MODULE_dde, TRUE, pred, argv) )
	 rval = (void *) DDE_FACK;
       PL_discard_foreign_frame(cid);
       DdeFreeDataHandle(hData);
       return rval;
     }
     case XTYP_REQUEST:
     { HDDEDATA data = (HDDEDATA) NULL;

       if ( fmt == CF_UNICODETEXT )
       { fid_t cid = PL_open_foreign_frame();
	 term_t argv = PL_new_term_refs(4);
	 predicate_t pred = PL_pred(FUNCTOR_dde_request4, MODULE_dde);
	 int plhandle = findServerHandle(hconv);

	 PL_put_integer( argv+0, plhandle);
	 unify_hsz(ddeInst, argv+1, hsz1);	/* topic */
	 unify_hsz(ddeInst, argv+2, hsz2);	/* item */

	 if ( PL_call_predicate(MODULE_dde, TRUE, pred, argv) )
	 { wchar_t *s;
	   size_t len;

					/* TBD: error handling */
	   if ( PL_get_wchars(argv+3, &len, &s, CVT_ALL) )
	     data = DdeCreateDataHandle(ddeInst,
					(unsigned char*) s,
					(DWORD)(len+1)*sizeof(wchar_t),
					0, hsz2, CF_UNICODETEXT, 0);
	 }
	 PL_discard_foreign_frame(cid);
       }

       return data;
     }
     default:
       ;
  }

  return (HDDEDATA)NULL;
}


static void
dde_init_constants(void)
{ static int done = FALSE;

  if ( !done )				/* no worries if this happens twice */
  { MODULE_dde = lookupModule(PL_new_atom("win_dde"));

    FUNCTOR_dde_connect3  =
	lookupFunctorDef(PL_new_atom("$dde_connect"), 3);
    FUNCTOR_dde_connect_confirm3 =
	lookupFunctorDef(PL_new_atom("$dde_connect_confirm"), 3);
    FUNCTOR_dde_disconnect1 =
        lookupFunctorDef(PL_new_atom("$dde_disconnect"), 1);
    FUNCTOR_dde_request4  =
	lookupFunctorDef(PL_new_atom("$dde_request"), 4);
    FUNCTOR_dde_execute3  =
	lookupFunctorDef(PL_new_atom("$dde_execute"), 3);
    FUNCTOR_error1        =
        lookupFunctorDef(ATOM_error, 1);

    done = TRUE;
  }
}

static void
dde_uninitialise(void *closure)
{ GET_LD
  DWORD ddeInst;

  if ( (ddeInst=LD->os.dde_instance) )
  { LD->os.dde_instance = 0;

    DdeUninitialize(ddeInst);
  }
}


static DWORD
dde_initialise(void)
{ GET_LD
  DWORD ddeInst;

  dde_init_constants();

  if ( !(ddeInst=LD->os.dde_instance) )
  { if ( DdeInitializeW(&ddeInst, (PFNCALLBACK)DdeCallback,
			APPCLASS_STANDARD|CBF_FAIL_ADVISES|CBF_FAIL_POKES|
			CBF_SKIP_REGISTRATIONS|CBF_SKIP_UNREGISTRATIONS,
			0L) == DMLERR_NO_ERROR)
    { LD->os.dde_instance = ddeInst;
#ifdef O_PLMT
      PL_thread_at_exit(dde_uninitialise, NULL, FALSE);
#endif
    } else
    { dde_warning("initialise");
    }

    DEBUG(1, Sdprintf("Thread %d: created ddeInst %d\n",
		      PL_thread_self(), ddeInst));

  }

  return ddeInst;
}


static
PRED_IMPL("$dde_register_service", 2, dde_register_service, 0)
{ HSZ t;
  int a;
  DWORD ddeInst;

  term_t topic = A1;
  term_t onoff = A2;

  if ( !(ddeInst=dde_initialise()) )
    return FALSE;
  if ( !get_hsz(ddeInst, topic, &t) )
    fail;
  if ( !PL_get_bool(onoff, &a) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, onoff);

  if ( !a )
  { int rval = (intptr_t)DdeNameService(ddeInst, t, 0L, DNS_UNREGISTER);
    DdeFreeStringHandle(ddeInst, t);
    return rval ? TRUE : FALSE;
  } else
  { if ( DdeNameService(ddeInst, t, 0L, DNS_REGISTER|DNS_FILTERON) )
      succeed;				/* should we free too? */

    DdeFreeStringHandle(ddeInst, t);
    return dde_warning("register_request");
  }
}

static
PRED_IMPL("open_dde_conversation", 3, open_dde_conversation, 0)
{ PRED_LD
  UINT i;
  HSZ Hservice = 0, Htopic = 0;
  int rc = TRUE;
  DWORD ddeInst;

  term_t service = A1;
  term_t topic   = A2;
  term_t handle  = A3;

  if ( !(ddeInst=dde_initialise()) )
    fail;

  if ( !get_hsz(ddeInst, service, &Hservice) ||
       !get_hsz(ddeInst, topic, &Htopic) )
  { rc = FALSE;
    goto out;
  }

  /* Establish a connection and get a handle for it */
  LOCK();
  for (i=0; i < MAX_CONVERSATIONS; i++)		/* Find an open slot */
  { if (conv_handle[i] == (HCONV)NULL)
    { conv_handle[i] = (HCONV)~0;		/* reserve it */
      break;
    }
  }
  UNLOCK();
  if (i == MAX_CONVERSATIONS)
  { rc = PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_max_dde_handles);
    goto out;
  }

  if ( !(conv_handle[i] = DdeConnect(ddeInst, Hservice, Htopic, 0)) )
  { rc = dde_warning("connect");
    goto out;
  }

  rc = PL_unify_integer(handle, i);

out:
  if ( Hservice )
    DdeFreeStringHandle(ddeInst, Hservice);
  if ( Htopic )
    DdeFreeStringHandle(ddeInst, Htopic);

  return rc;
}


static int
get_conv_handle(term_t handle, int *theh)
{ GET_LD
  int h;

  if ( !PL_get_integer(handle, &h) || h < 0 || h >= MAX_CONVERSATIONS )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_dde_handle, handle);

  if ( !conv_handle[h] )
    return PL_error(NULL, 0, 0, ERR_EXISTENCE, ATOM_dde_handle, handle);

  *theh = h;
  succeed;
}


static
PRED_IMPL("close_dde_conversation", 1, close_dde_conversation, 0)
{ int hdl;

  term_t handle = A1;

  if ( !get_conv_handle(handle, &hdl) )
    fail;

  DdeDisconnect(conv_handle[hdl]);
  conv_handle[hdl] = (HCONV)NULL;

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: Windows-XP gives the wrong value for valuelen below. Hence we will
use nul-terminated strings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("dde_request", 4, dde_request, 0)
{ int hdl;
  int rval;
  HSZ Hitem;
  DWORD result, valuelen;
  HDDEDATA Hvalue;
  long tmo;
  static UINT fmt[] = {CF_UNICODETEXT, CF_TEXT};
  int fmti;
  DWORD ddeInst;

  term_t handle  = A1;
  term_t item    = A2;
  term_t value   = A3;
  term_t timeout = A4;

  if ( !(ddeInst=dde_initialise()) )
    return FALSE;

  if ( !get_conv_handle(handle, &hdl) ||
       !get_hsz(ddeInst, item, &Hitem) ||
       !PL_get_long_ex(timeout, &tmo) )
    fail;

  if ( tmo <= 0 )
    tmo = TIMEOUT_VERY_LONG;

  for(fmti = 0; fmti<2; fmti++)
  { Hvalue = DdeClientTransaction(NULL, 0, conv_handle[hdl], Hitem, fmt[fmti],
				  XTYP_REQUEST, (DWORD)tmo, &result);
    if ( Hvalue )
      break;
  }
  DdeFreeStringHandle(ddeInst, Hitem);

  if ( Hvalue)
  { LPBYTE valuedata;

    if ( (valuedata = DdeAccessData(Hvalue, &valuelen)) )
    { DEBUG(1, Sdprintf("valuelen = %ld; format = %d\n", valuelen, fmti));
      if ( fmt[fmti] == CF_TEXT )
      { DEBUG(1, Sdprintf("ANSI text\n"));
	rval = PL_unify_string_chars(value, (char*)valuedata);
      } else
	rval = PL_unify_wchars(value, PL_STRING, -1, (wchar_t*)valuedata);
      DdeUnaccessData(Hvalue);
      return rval;
    } else
    { return dde_warning("access_data");
    }
  } else
  { return dde_warning("request");
  }
}


static
PRED_IMPL("dde_execute", 3, dde_execute, 0)
{ int hdl;
  wchar_t *cmdstr;
  size_t cmdlen;
  HDDEDATA Hvalue, data;
  DWORD result;
  DWORD ddeInst;
  long tmo;

  term_t handle  = A1;
  term_t command = A2;
  term_t timeout = A3;

  if ( !(ddeInst=dde_initialise()) ||
       !get_conv_handle(handle, &hdl) ||
       !PL_get_wchars(command, &cmdlen, &cmdstr, CVT_ALL|CVT_EXCEPTION) ||
       !PL_get_long_ex(timeout, &tmo) )
    fail;

  if ( tmo <= 0 )
    tmo = TIMEOUT_VERY_LONG;

  if ( !(data = DdeCreateDataHandle(ddeInst,
				    (unsigned char*)cmdstr,
				    (DWORD)(cmdlen+1)*sizeof(wchar_t),
				    0, 0, CF_UNICODETEXT, 0)) )
    return dde_warning("dde_execute/3");

  Hvalue = DdeClientTransaction((LPBYTE) data, (DWORD) -1,
				conv_handle[hdl], 0L, 0,
				XTYP_EXECUTE, (DWORD) tmo, &result);
  if ( Hvalue )
    succeed;

  return dde_warning("execute");
}


static
PRED_IMPL("dde_poke", 4, dde_poke, 0)
{ int hdl;
  wchar_t *datastr;
  size_t datalen;
  HDDEDATA Hvalue;
  HSZ Hitem;
  long tmo;
  DWORD ddeInst;

  term_t handle  = A1;
  term_t item    = A2;
  term_t data    = A3;
  term_t timeout = A4;

  if ( !(ddeInst=dde_initialise()) ||
       !get_conv_handle(handle, &hdl) ||
       !get_hsz(ddeInst, item, &Hitem) )
    fail;
  if ( !PL_get_wchars(data, &datalen, &datastr, CVT_ALL|CVT_EXCEPTION) )
    fail;
  if ( !PL_get_long_ex(timeout, &tmo) )
    fail;

  if ( tmo <= 0 )
    tmo = TIMEOUT_VERY_LONG;

  Hvalue = DdeClientTransaction((unsigned char*)datastr,
				(DWORD)(datalen+1)*sizeof(wchar_t),
				conv_handle[hdl], Hitem, CF_UNICODETEXT,
				XTYP_POKE, (DWORD)tmo, NULL);

  if ( !Hvalue )
    return dde_warning("poke");

  succeed;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(dde)
  PRED_DEF("$dde_register_service",  2, dde_register_service,   0)
  PRED_DEF("open_dde_conversation",  3, open_dde_conversation,  0)
  PRED_DEF("close_dde_conversation", 1, close_dde_conversation, 0)
  PRED_DEF("dde_request",	     4, dde_request,		0)
  PRED_DEF("dde_execute",	     3, dde_execute,		0)
  PRED_DEF("dde_poke",		     4, dde_poke,		0)
EndPredDefs

#endif /*O_DDE*/
#endif /*__WINDOWS__*/
