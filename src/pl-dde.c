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
#undef TRANSPARENT       /* defined in win16.h (which we can't avoid);
                            redef'd in pl-incl.h (we don't need
			    the win16 version) */
#include "pl-incl.h"

#if O_DDE

#include "pl-itf.h"
#include "string.h"

#ifdef __WATCOMC__			/* at least version 9.5 */
#define WATCOM_DDEACCESS_BUG 1
#endif

#define FASTBUFSIZE 512			/* use local buffer upto here */
#define MAX_CONVERSATIONS 32		/* Max. # of conversations */
#define TIMEOUT_VERY_LONG 0x7fffffff;	/* largest positive int */

static HCONV conv_handle[MAX_CONVERSATIONS];
static HCONV server_handle[MAX_CONVERSATIONS];
static DWORD ddeInst;			/* Instance of this process */

static Module	  MODULE_dde;		/* win_dde */
static FunctorDef FUNCTOR_dde_connect3;
static FunctorDef FUNCTOR_dde_connect_confirm3;
static FunctorDef FUNCTOR_dde_disconnect1;
static FunctorDef FUNCTOR_dde_request4;
static FunctorDef FUNCTOR_dde_execute3;
static FunctorDef FUNCTOR_error1;

static word
dde_warning(char *cmd)
{ char *err;

  switch( DdeGetLastError(ddeInst) )
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
  }

  return warning("%s: DDE operation failed: %s", cmd, err);
}


static Atom
hszToAtom(HSZ hsz)
{ char buf[FASTBUFSIZE];
  int len;

  if ( !(len=DdeQueryString(ddeInst, hsz, buf, sizeof(buf)-1, CP_WINANSI)) )
  { dde_warning("string handle");
    return NULL;
  }

  if ( len == sizeof(buf)-1 )
  { if ( (len=DdeQueryString(ddeInst, hsz, NULL, 0, CP_WINANSI)) > 0 )
    { char *b2 = malloc(len+1);
      Atom a;
      
      DdeQueryString(ddeInst, hsz, b2, len+1, CP_WINANSI);
      a = lookupAtom(b2);
      free(b2);

      return a;
    }

    dde_warning("string handle");
  }

  return lookupAtom(buf);
}


static word
hdataToString(HDDEDATA data)
{ char buf[FASTBUFSIZE];
  int len;

  if ( !(len=DdeGetData(data, buf, sizeof(buf)-1, 0)) )
  { dde_warning("data handle");
    return (word)NULL;
  }

  if ( len == sizeof(buf)-1 )
  { if ( (len=DdeGetData(data, NULL, 0, 0)) > 0 )
    { char *b2 = malloc(len+1);
      word s;
      
      DdeGetData(data, b2, len, 0);
      b2[len] = '\0';
      s = globalString(b2);
      free(b2);

      return s;
    }

    dde_warning("data handle");
  }

  return globalString(buf);
}


static HSZ
wordToHsz(word data)
{ char *s;

  if ( (s = primitiveToString(data, FALSE)) )
    return DdeCreateStringHandle(ddeInst, s, CP_WINANSI);

  return (HSZ) NULL;
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
     { word goal;
       mark m;
       int rval;

       Mark(m);
       goal = globalFunctor(FUNCTOR_dde_connect3);
       argTerm(goal, 0) = (word) hszToAtom(hsz2); /* topic */
       argTerm(goal, 1) = (word) hszToAtom(hsz1); /* service */
       argTerm(goal, 2) = (dwData2 ? consNum(1) : consNum(0)); /* same inst */
       rval = callGoal(MODULE_dde, goal, TRUE);
       Undo(m);
       
       return rval ? TRUE : FALSE;
     }
     case XTYP_CONNECT_CONFIRM:
     { word goal;
       mark m;
       int rval;
       int plhandle;

       if ( (plhandle = allocServerHandle(hconv)) < 0 )
       { warning("No more DDE server handles");
	 return FALSE;
       }

       Mark(m);
       goal = globalFunctor(FUNCTOR_dde_connect_confirm3);
       argTerm(goal, 0) = (word) hszToAtom(hsz2); /* topic */
       argTerm(goal, 1) = (word) hszToAtom(hsz1); /* service */
       argTerm(goal, 2) = consNum(plhandle);
       rval = callGoal(MODULE_dde, goal, TRUE);
       Undo(m);
       
       return rval ? TRUE : FALSE; 
     }
     case XTYP_DISCONNECT:
     { word goal;
       mark m;
       int rval;
       int plhandle = findServerHandle(hconv);
       
       if ( plhandle >= 0 && plhandle < MAX_CONVERSATIONS )
	 server_handle[plhandle] = (HCONV)NULL;

       Mark(m);
       goal = globalFunctor(FUNCTOR_dde_disconnect1);
       argTerm(goal, 0) = consNum(plhandle);      /* handle */
       rval = callGoal(MODULE_dde, goal, TRUE);
       Undo(m);
       
       return rval ? TRUE : FALSE;
     }
     case XTYP_EXECUTE:
     { Atom topic = hszToAtom(hsz1);
       word data;
       mark m;
       int plhandle = findServerHandle(hconv);
       HDDEDATA rval = DDE_FNOTPROCESSED;

       Mark(m);

       if ( (data = hdataToString(hData)) )
       { word goal = globalFunctor(FUNCTOR_dde_execute3);

	 argTerm(goal, 0) = consNum(plhandle);
	 argTerm(goal, 1) = (word) topic;
	 argTerm(goal, 2) = data;
	 if ( callGoal(MODULE_dde, goal, TRUE) )
	   rval = DDE_FACK;
	 
	 DdeFreeDataHandle(hData);
       } else
	 dde_warning("DdeAccessData()");

       Undo(m);
       return rval;
     }
     case XTYP_REQUEST:
     { HDDEDATA data = (HDDEDATA) NULL;

       if ( fmt == CF_TEXT )
       { Atom topic = hszToAtom(hsz1);
	 Atom item  = hszToAtom(hsz2);
	 word goal;
	 mark m;
	 int plhandle = findServerHandle(hconv);
	   
	 Mark(m);
	 goal = globalFunctor(FUNCTOR_dde_request4);
	 argTerm(goal, 0) = consNum(plhandle);
	 argTerm(goal, 1) = (word) topic;
	 argTerm(goal, 2) = (word) item;
	 if ( callGoal(MODULE_dde, goal, TRUE) )
	 { Word rval = argTermP(goal, 3);
	   char *s;

	   if ( (s = primitiveToString(*rval, FALSE)) )
	   { data = DdeCreateDataHandle(ddeInst, s, strlen(s)+1,
					0, hsz2, CF_TEXT, 0);
	   }
	 }
	 Undo(m);
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
      fail;
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
pl_dde_register_service(Word topic, Word onoff)
{ HSZ t;

  TRY(dde_initialise());

  if ( !(t=wordToHsz(*topic)) )
    return warning("dde_register_topic/1: instantiation fault");

  if ( *onoff == (word)ATOM_off )
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
pl_open_dde_conversation(Word service, Word topic, Word handle)
{ UINT i;
  HSZ Hservice, Htopic;

  if (!isAtom(*service) || !isAtom(*topic)) {
    warning("open_dde_conversation/3: input arguments must be atoms");
    fail;
  }

  if ( !dde_initialise() )
    fail;

  /* Establish a connection and get a handle for it */
  for (i=0; i < MAX_CONVERSATIONS; i++)   /* Find an open slot */
    if (conv_handle[i] == (HCONV)NULL) break;
  if (i == MAX_CONVERSATIONS) {
    warning("open_dde_conversation/3: too many conversations");
    fail;
  }

  Hservice = DdeCreateStringHandle(ddeInst, stringAtom(*service), CP_WINANSI);
  Htopic = DdeCreateStringHandle(ddeInst, stringAtom(*topic), CP_WINANSI);

  if ( !(conv_handle[i] = DdeConnect(ddeInst, Hservice, Htopic, 0)) )
    fail;
  else
    TRY(unifyAtomic(handle, consNum(i)));

  DdeFreeStringHandle(ddeInst, Hservice);
  DdeFreeStringHandle(ddeInst, Htopic);
  succeed;
}


static int
wordToConvHandle(word handle)
{ if ( isInteger(handle) )
  { int h = valNum(handle);

    if ( h >= 0 && h < MAX_CONVERSATIONS && conv_handle[h] )
      return h;
  }

  return -1;
}


word
pl_close_dde_conversation(Word handle)
{ int hdl;

  if ( (hdl = wordToConvHandle(*handle)) < 0 )
    return warning("close_dde_conversation/1: invalid handle");

  DdeDisconnect(conv_handle[hdl]);
  conv_handle[hdl] = (HCONV)NULL;

  succeed;
}


word
pl_dde_request(Word handle, Word item, Word value, Word timeout)
{ int hdl;
  int rval;
  int ddeErr;
  HSZ Hitem;
  DWORD result, valuelen;
  HDDEDATA Hvalue;
  DWORD tmo;

  if ( (hdl = wordToConvHandle(*handle)) < 0 )
    return warning("dde_request/4: invalid handle");
  if ( !(Hitem = wordToHsz(*item)) )
    return warning("dde_request/4: invalid item");
  if ( !isInteger(*timeout) )
    return warning("dde_request/4: invalid timeout");
  tmo = valNum(*timeout);
  if ( tmo <= 0 )
    tmo = TIMEOUT_VERY_LONG;

  Hvalue = DdeClientTransaction(NULL, 0, conv_handle[hdl], Hitem, CF_TEXT,
				XTYP_REQUEST, tmo, &result);
  ddeErr = DdeGetLastError(ddeInst);
  DdeFreeStringHandle(ddeInst, Hitem);

  if ( ddeErr == DMLERR_NO_ERROR)
  { char * valuebuf;
#ifdef WATCOM_DDEACCESS_BUG
    /* Watcom's DdeAccessData is declared to return a 32-bit near ptr, but actually
       delivers a 16-bit far; the following is to work around this */
    LPARAM valuefp;
    char far * valuedata;
    valuefp = (LPARAM)DdeAccessData(Hvalue, &valuelen);
    valuedata = MK_FP32((void *)valuefp);
    valuebuf = (char *)malloc((size_t)valuelen+1);
    _fstrncpy(valuebuf, valuedata, valuelen+1);
#else
    char * valuedata;
    valuedata = DdeAccessData(Hvalue, &valuelen);
    valuebuf = (char *)malloc((size_t)valuelen+1);
    strncpy(valuebuf, valuedata, valuelen+1);
#endif
    DdeUnaccessData(Hvalue);
    valuebuf[valuelen] = '\0';
    rval = unifyAtomic(value, lookupAtom(valuebuf));
    free(valuebuf);
    return rval;
  } else
  { char * errmsg;

    switch (ddeErr)
    { case DMLERR_BUSY:            errmsg = "Server busy"; break;
      case DMLERR_DATAACKTIMEOUT:  errmsg = "Request timed out"; break;
      case DMLERR_SERVER_DIED:     errmsg = "Server disconnected"; break;
      default:                     errmsg = "Unknown error"; break;
    }

    if ( unifyFunctor(value, FUNCTOR_error1) &&
	 unifyAtomic(argTermP(value, 1), lookupAtom(errmsg)) )
      succeed;

    fail;
  }
}



word
pl_dde_execute(Word handle, Word command, Word timeout)
{ int hdl;
  char *cmdstr;
  HDDEDATA Hvalue, data;
  DWORD result;
  DWORD tmo;

  if ( (hdl = wordToConvHandle(*handle)) < 0 )
    return warning("dde_execute/3: invalid handle");
  if ( (cmdstr = primitiveToString(*command, FALSE)) == NULL )
    return warning("dde_execute/3: invalid command");
  if ( !isInteger(*timeout) )
    return warning("dde_execute/2: invalid timeout");

  tmo = valNum(*timeout);
  if ( tmo <= 0 )
    tmo = TIMEOUT_VERY_LONG;

  if ( !(data = DdeCreateDataHandle(ddeInst, cmdstr, strlen(cmdstr)+1,
				    0, 0, CF_TEXT, 0)) )
    return dde_warning("dde_execute/3");

  Hvalue = DdeClientTransaction((LPBYTE) data, (DWORD) -1,
				conv_handle[hdl], 0L, 0,
				XTYP_EXECUTE, tmo, &result);
  if ( Hvalue )
    succeed;

  return dde_warning("dde_execute/2");
}

#endif /*O_DDE*/
#endif /*__WINDOWS__*/
