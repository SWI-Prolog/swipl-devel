/*  $Id$

    Part of SWI-Prolog.
    Contributed by Dwig (dwig@markv.com)
    Copying policy: ???

    Purpose: Windows DDE interface (client side)
*/

#if defined(__WINDOWS__) || defined(__WIN32__)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Extension of SWI-Prolog:
   Primitives to support interprocess communication via DDE, for those
   platforms that support DDEML.  Currently, this is the Windows family (3.1
   and above) and Unix platforms with Bristol's Windows support.

   Eventually, this should turn into a full DDE capability.  For the
   present, I'm just implementing the client side of conversation
   management, and only providing request transactions, as follows:

   open_dde_conversation(+Service, +Topic, -Handle)
   Open a conversation with a server supporting the given service name and
   topic (atoms).  If successful, Handle may be used to send transactions to
   the server.  If no willing server is found, fails.

   close_dde_conversation(+Handle)
   Close the conversation associated with Handle.  All opened conversations
   should be closed when they're no longer needed, although the system
   will close any that remain open on process termination.

   dde_request(+Handle, +Item, -Value)
   Request a value from the server.  Item is an atom that identifies the
   requested data, and Value will be an atom (CF_TEXT data in DDE parlance)
   representing that data, if the request is successful.  If unsuccessful,
   Value will be unified with a term of form error(reason), identifying the
   problem.

   It could be argued that the atoms above should be strings; I may go that
   way sometime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define INCLUDE_DDEML_H
#include "windows.h"
#ifdef __LCC__
#include "ddeml.h"
#endif
#include "pl-incl.h"

#if O_DDE
#include <string.h>

#ifdef __WATCOMC__			/* at least version 9.5 */
#define WATCOM_DDEACCESS_BUG 1
#endif

#define FASTBUFSIZE 512			/* use local buffer upto here */
#define MAX_CONVERSATIONS 32		/* Max. # of conversations */
#define TIMEOUT_VERY_LONG 0x7fffffff;	/* largest positive int */

static HCONV conv_handle[MAX_CONVERSATIONS];
static HCONV server_handle[MAX_CONVERSATIONS];
static DWORD ddeInst;			/* Instance of this process */

static Module	 MODULE_dde;		/* win_dde */
static functor_t FUNCTOR_dde_connect3;
static functor_t FUNCTOR_dde_connect_confirm3;
static functor_t FUNCTOR_dde_disconnect1;
static functor_t FUNCTOR_dde_request4;
static functor_t FUNCTOR_dde_execute3;
static functor_t FUNCTOR_error1;

static char *
dde_error_message(int errn)
{ char *err;

  if ( errn <= 0 )
    errn = DdeGetLastError(ddeInst);

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


static word
dde_warning(char *cmd)
{ char *err = dde_error_message(-1);

  return warning("%s: DDE operation failed: %s", cmd, err);
}


static atom_t
hszToAtom(HSZ hsz)
{ char buf[FASTBUFSIZE];
  int len;

  if ( !(len=DdeQueryString(ddeInst, hsz, buf, sizeof(buf)-1, CP_WINANSI)) )
  { dde_warning("string handle");
    return NULL_ATOM;
  }

  if ( len == sizeof(buf)-1 )
  { if ( (len=DdeQueryString(ddeInst, hsz, NULL, 0, CP_WINANSI)) > 0 )
    { char *b2 = malloc(len+1);
      atom_t a;
      
      DdeQueryString(ddeInst, hsz, b2, len+1, CP_WINANSI);
      a = lookupAtom(b2);
      free(b2);

      return a;
    }

    dde_warning("string handle");
  }

  return lookupAtom(buf);
}


static int
unify_hdata(term_t t, HDDEDATA data)
{ char buf[FASTBUFSIZE];
  int len;

  if ( !(len=DdeGetData(data, buf, sizeof(buf)-1, 0)) )
  { dde_warning("data handle");
    fail;
  }

  if ( len == sizeof(buf)-1 )
  { if ( (len=DdeGetData(data, NULL, 0, 0)) > 0 )
    { char *b2 = malloc(len+1);
      int rval;
      
      DdeGetData(data, b2, len, 0);
      b2[len] = '\0';
      rval = PL_unify_list_chars(t, b2);
      free(b2);

      return rval;
    }

    dde_warning("data handle");
  }

  return PL_unify_list_chars(t, buf);
}


static int
get_hsz(term_t data, HSZ *rval)
{ char *s;

  if ( PL_get_chars(data, &s, CVT_ALL) )
  { HSZ h = DdeCreateStringHandle(ddeInst, s, CP_WINANSI);
    if ( h )
    { *rval = h;
      succeed;
    }
  }

  fail;
}


static int
allocServerHandle(HCONV handle)
{ int i;

  for(i=0; i<MAX_CONVERSATIONS; i++)
  { if ( !server_handle[i] )
    { server_handle[i] = handle;
      return i;
    }
  }

  return -1;
}


static int
findServerHandle(HCONV handle)
{ int i;

  for(i=0; i<MAX_CONVERSATIONS; i++)
  { if ( server_handle[i] == handle )
      return i;
  }

  return -1;
}


static HDDEDATA CALLBACK 
DdeCallback(UINT type, UINT fmt, HCONV hconv, HSZ hsz1, HSZ hsz2,
            HDDEDATA hData, DWORD dwData1, DWORD dwData2)
{
  switch(type)
  {  case XTYP_CONNECT:
     { fid_t cid = PL_open_foreign_frame();
       term_t argv = PL_new_term_refs(3);
       predicate_t pred = PL_pred(FUNCTOR_dde_connect3, MODULE_dde);
       int rval;

       PL_put_atom(   argv+0, hszToAtom(hsz2)); /* topic */
       PL_put_atom(   argv+1, hszToAtom(hsz1)); /* service */
       PL_put_integer(argv+2, dwData2 ? 1 : 0); /* same instance */
       rval = PL_call_predicate(MODULE_dde, TRUE, pred, argv);
       PL_discard_foreign_frame(cid);

       return (void *)rval;
     }
     case XTYP_CONNECT_CONFIRM:
     { fid_t cid = PL_open_foreign_frame();
       term_t argv = PL_new_term_refs(3);
       predicate_t pred = PL_pred(FUNCTOR_dde_connect_confirm3, MODULE_dde);
       int plhandle;

       if ( (plhandle = allocServerHandle(hconv)) >= 0 )
       { fid_t cid = PL_open_foreign_frame();
	 term_t argv = PL_new_term_refs(3);
	 predicate_t pred = PL_pred(FUNCTOR_dde_connect_confirm3, MODULE_dde);

	 PL_put_atom(   argv+0, hszToAtom(hsz2)); /* topic */
	 PL_put_atom(   argv+1, hszToAtom(hsz1)); /* service */
	 PL_put_integer(argv+2, plhandle);

	 PL_call_predicate(MODULE_dde, TRUE, pred, argv);
	 PL_discard_foreign_frame(cid);
       } else
       { warning("No more DDE server handles");
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

       PL_put_integer(argv+0, plhandle);
       PL_put_atom(   argv+1, hszToAtom(hsz1));
       unify_hdata(   argv+2, hData);
       if ( PL_call_predicate(MODULE_dde, TRUE, pred, argv) )
	 rval = (void *) DDE_FACK;
       PL_discard_foreign_frame(cid);
       DdeFreeDataHandle(hData);
       return rval;
     }
     case XTYP_REQUEST:
     { HDDEDATA data = (HDDEDATA) NULL;

       if ( fmt == CF_TEXT )
       { fid_t cid = PL_open_foreign_frame();
	 term_t argv = PL_new_term_refs(4);
	 predicate_t pred = PL_pred(FUNCTOR_dde_request4, MODULE_dde);
	 int plhandle = findServerHandle(hconv);

	 PL_put_integer( argv+0, plhandle);
	 PL_put_atom(	 argv+1, hszToAtom(hsz1)); /* topic */
	 PL_put_atom(    argv+2, hszToAtom(hsz2)); /* item */
	 PL_put_variable(argv+3);

	 if ( PL_call_predicate(MODULE_dde, TRUE, pred, argv) )
	 { char *s;

	   if ( PL_get_chars(argv+3, &s, CVT_ALL) )
	     data = DdeCreateDataHandle(ddeInst, s, strlen(s)+1,
					0, hsz2, CF_TEXT, 0);
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


static int
dde_initialise()
{ if ( ddeInst == (DWORD)NULL )
  { if (DdeInitialize(&ddeInst, (PFNCALLBACK)DdeCallback,
		      APPCLASS_STANDARD|CBF_FAIL_ADVISES|CBF_FAIL_POKES|
		      CBF_SKIP_REGISTRATIONS|CBF_SKIP_UNREGISTRATIONS,
		      0L)
	!= DMLERR_NO_ERROR)
    { ddeInst = (DWORD) -1;
      return dde_warning("initialise");
    }

    MODULE_dde = lookupModule(lookupAtom("win_dde"));

    FUNCTOR_dde_connect3  =
	lookupFunctorDef(lookupAtom("$dde_connect"), 3);
    FUNCTOR_dde_connect_confirm3 =
	lookupFunctorDef(lookupAtom("$dde_connect_confirm"), 3);
    FUNCTOR_dde_disconnect1 =
        lookupFunctorDef(lookupAtom("$dde_disconnect"), 1);
    FUNCTOR_dde_request4  =
	lookupFunctorDef(lookupAtom("$dde_request"), 4);
    FUNCTOR_dde_execute3  =
	lookupFunctorDef(lookupAtom("$dde_execute"), 3);
    FUNCTOR_error1        =
        lookupFunctorDef(lookupAtom("error"), 1);
  }

  succeed;
}


word
pl_dde_register_service(term_t topic, term_t onoff)
{ HSZ t;
  atom_t a;

  TRY(dde_initialise());

  if ( !get_hsz(topic, &t) ||
       !PL_get_atom(onoff, &a) )
    return warning("dde_register_topic/1: instantiation fault");

  if ( a == ATOM_off )
  { int rval = (int)DdeNameService(ddeInst, t, 0L, DNS_UNREGISTER);
    DdeFreeStringHandle(ddeInst, t);
    return rval ? TRUE : FALSE;
  } else
  { if ( DdeNameService(ddeInst, t, 0L, DNS_REGISTER|DNS_FILTERON) )
      succeed;				/* should we free too? */

    DdeFreeStringHandle(ddeInst, t);
    return dde_warning("dde_register_request");
  }
}


word
pl_open_dde_conversation(term_t service, term_t topic,
			 term_t handle)
{ UINT i;
  HSZ Hservice, Htopic;

  if ( !dde_initialise() )
    fail;

  if ( !get_hsz(service, &Hservice) ||
       !get_hsz(topic, &Htopic) )
    return warning("open_dde_conversation/3: instantion fault");

  /* Establish a connection and get a handle for it */
  for (i=0; i < MAX_CONVERSATIONS; i++)   /* Find an open slot */
  { if (conv_handle[i] == (HCONV)NULL)
      break;
  }
  if (i == MAX_CONVERSATIONS)
    return warning("open_dde_conversation/3: too many conversations");

  if ( !(conv_handle[i] = DdeConnect(ddeInst, Hservice, Htopic, 0)) )
    fail;

  DdeFreeStringHandle(ddeInst, Hservice);
  DdeFreeStringHandle(ddeInst, Htopic);

  return PL_unify_integer(handle, i);
}


static int
get_conv_handle(term_t handle, int *theh)
{ int h;

  if ( PL_get_integer(handle, &h) &&
       h >= 0 && h < MAX_CONVERSATIONS &&
       conv_handle[h] )
  { *theh = h;

    succeed;
  }

  fail;
}


word
pl_close_dde_conversation(term_t handle)
{ int hdl;

  if ( !get_conv_handle(handle, &hdl) )
    return warning("close_dde_conversation/1: invalid handle");

  DdeDisconnect(conv_handle[hdl]);
  conv_handle[hdl] = (HCONV)NULL;

  succeed;
}


word
pl_dde_request(term_t handle, term_t item,
	       term_t value, term_t timeout)
{ int hdl;
  int rval;
  int ddeErr;
  HSZ Hitem;
  DWORD result, valuelen;
  HDDEDATA Hvalue;
  long tmo;

  if ( !get_conv_handle(handle, &hdl) )
    return warning("dde_request/4: invalid handle");
  if ( !get_hsz(item, &Hitem) )
    return warning("dde_request/4: invalid item");
  if ( !PL_get_long(timeout, &tmo) )
    return warning("dde_request/4: invalid timeout");

  if ( tmo <= 0 )
    tmo = TIMEOUT_VERY_LONG;

  Hvalue = DdeClientTransaction(NULL, 0, conv_handle[hdl], Hitem, CF_TEXT,
				XTYP_REQUEST, (DWORD)tmo, &result);
  ddeErr = DdeGetLastError(ddeInst);
  DdeFreeStringHandle(ddeInst, Hitem);

  if ( Hvalue)
  { char * valuebuf;
    char * valuedata;
    valuedata = DdeAccessData(Hvalue, &valuelen);
    valuebuf = (char *)malloc((size_t)valuelen+1);
    strncpy(valuebuf, valuedata, valuelen+1);
    DdeUnaccessData(Hvalue);
    valuebuf[valuelen] = EOS;
    rval = PL_unify_string_chars(value, valuebuf);
    free(valuebuf);
    return rval;
  } else
  { char * errmsg = dde_error_message(ddeErr);

    return PL_unify_term(value,
			 PL_FUNCTOR, FUNCTOR_error1, /* error(Message) */
			 PL_CHARS,   errmsg);
  }
}



word
pl_dde_execute(term_t handle, term_t command, term_t timeout)
{ int hdl;
  char *cmdstr;
  HDDEDATA Hvalue, data;
  DWORD result;
  long tmo;

  if ( !get_conv_handle(handle, &hdl) )
    return warning("dde_execute/3: invalid handle");
  if ( !PL_get_chars(command, &cmdstr, CVT_ALL) )
    return warning("dde_execute/3: invalid command");
  if ( !PL_get_long(timeout, &tmo) )
    return warning("dde_execute/2: invalid timeout");

  if ( tmo <= 0 )
    tmo = TIMEOUT_VERY_LONG;

  if ( !(data = DdeCreateDataHandle(ddeInst, cmdstr, strlen(cmdstr)+1,
				    0, 0, CF_TEXT, 0)) )
    return dde_warning("dde_execute/3");

  Hvalue = DdeClientTransaction((LPBYTE) data, (DWORD) -1,
				conv_handle[hdl], 0L, 0,
				XTYP_EXECUTE, (DWORD) tmo, &result);
  if ( Hvalue )
    succeed;

  return dde_warning("dde_execute/2");
}


word
pl_dde_poke(term_t handle, term_t item, term_t data, term_t timeout)
{ int hdl;
  char *datastr;
  HDDEDATA Hvalue;
  HSZ Hitem;
  long tmo;

  if ( !get_conv_handle(handle, &hdl) )
    return warning("dde_poke/4: invalid handle");
  if ( !get_hsz(item, &Hitem) )
    return warning("dde_poke/4: invalid item");
  if ( !PL_get_chars(data, &datastr, CVT_ALL) )
    return warning("dde_poke/4: invalid data");
  if ( !PL_get_long(timeout, &tmo) )
    return warning("dde_poke/4: invalid timeout");

  if ( tmo <= 0 )
    tmo = TIMEOUT_VERY_LONG;

  Hvalue = DdeClientTransaction(datastr, strlen(datastr)+1,
				conv_handle[hdl], Hitem, CF_TEXT,
				XTYP_POKE, (DWORD)tmo, NULL);
  if ( !Hvalue )
    return dde_warning("dde_poke/2");

  succeed;
}

#endif /*O_DDE*/
#endif /*__WINDOWS__*/
