/*  $Id$

    Part of SWI-Prolog.
    Contributed by Dwig (dwig@markv.com)
    Copying policy: ???

    Purpose: Windows DDE interface (client side)
*/

#if defined(__WINDOWS__)

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

#define MAX_CONVERSATIONS 10   /* Max. number of simultaneous conversations */
static HCONV conv_handle[MAX_CONVERSATIONS];  /* I assume initialized to 0 */

static DWORD ddeInst;    /* Instance identifier of this process; set by
                            DdeInitialize.  Zero if initialization not done,
                            -1 if initialization failed */

/* Callback function, required by API, but not used for anything in this
   case */
#pragma off(unreferenced)
HDDEDATA CALLBACK 
DdeCallback(UINT type, UINT fmt, HCONV hconv, HSZ hsz1, HSZ hsz2,
            HDDEDATA hData, DWORD dwData1,  DWORD dwData2)
{
  switch(type)
  {
     default:
        return (HDDEDATA)NULL;

  }  /* end switch */
}
#pragma on(unreferenced)


word
pl_open_dde_conversation(Word service, Word topic, Word handle)
{
  UINT i;
  HSZ Hservice, Htopic;

  if (!isAtom(*service) || !isAtom(*topic)) {
    warning("open_dde_conversation/3: input arguments must be atoms");
    fail;
  }

  /* Initialize for DDE operations, if not already done */
  if (ddeInst == (DWORD)NULL)
    if (DdeInitialize(&ddeInst, (PFNCALLBACK)DdeCallback,
                      APPCMD_CLIENTONLY | CBF_SKIP_ALLNOTIFICATIONS, 0L)
        != DMLERR_NO_ERROR) {
      /* Should probably do better job here; on the other hand, this should
         never fail */
      ddeInst = -1;
      warning("open_dde_conversation/3: DDE initialization failed");
      fail;
    }

  /* Establish a connection and get a handle for it */
  for (i=0; i < MAX_CONVERSATIONS; i++)   /* Find an open slot */
    if (conv_handle[i] == (HCONV)NULL) break;
  if (i == MAX_CONVERSATIONS) {
    warning("open_dde_conversation/3: too many conversations");
    fail;
  }

  Hservice = DdeCreateStringHandle(ddeInst, stringAtom(*service), CP_WINANSI);
  Htopic = DdeCreateStringHandle(ddeInst, stringAtom(*topic), CP_WINANSI);
  if ((conv_handle[i] = DdeConnect(ddeInst, Hservice, Htopic, 0))
      == (HCONV)NULL )
    fail;
  else
    TRY(unifyAtomic(handle, consNum(i)));

  DdeFreeStringHandle(ddeInst, Hservice);
  DdeFreeStringHandle(ddeInst, Htopic);
  succeed;
}


word
pl_close_dde_conversation(Word handle)
{
  int hdl = valNum(*handle);

  if (!isInteger(*handle)) {
    warning("close_dde_conversation/1: argument not an integer");
    fail;
  }
  if ( MAX_CONVERSATIONS <= hdl || conv_handle[hdl] == (HCONV)NULL) {
    warning("close_dde_conversation/1: argument not a conversation handle");
    fail;
  }
  DdeDisconnect(conv_handle[hdl]);
  conv_handle[hdl] = (HCONV)NULL;
  succeed;
}

#define REQ_TIMEOUT 2000   /* Time in milliseconds to wait for response
                              (should probably be an optional arg. to call) */
word
pl_dde_request(Word handle, Word item, Word value)
{
  int hdl = valNum(*handle);
  int rval;
  int ddeErr;
  HSZ Hitem;
  char * itemstr = NULL;
  DWORD result, valuelen;
  HDDEDATA Hvalue;

  /* Should do more validation here: active handle */
  if (isAtom(*item)) itemstr = stringAtom(*item);
  else if (isString(*item)) itemstr = valString(*item);
  else ddeErr = DMLERR_FIRST - 1;  /* Bad argument type */
  
  /* Make the request */
  if (itemstr != NULL) {
      Hitem = DdeCreateStringHandle(ddeInst, itemstr, CP_WINANSI);
      Hvalue = DdeClientTransaction(NULL, 0, conv_handle[hdl], Hitem, CF_TEXT,
                                       XTYP_REQUEST, REQ_TIMEOUT, &result);
      ddeErr = DdeGetLastError(ddeInst);
      DdeFreeStringHandle(ddeInst, Hitem);
  }

  /* If successful, copy the data to a local string and "atomize" it */
  if (ddeErr == DMLERR_NO_ERROR) {
    char * valuebuf;
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
  }
  else {
    /* Unsuccessful, put together an error term telling why */
    term errterm;
    char * errmsg;

    switch (ddeErr) {
    case DMLERR_BUSY:            errmsg = "Server busy"; break;
    case DMLERR_DATAACKTIMEOUT:  errmsg = "Request timed out"; break;
    case DMLERR_SERVER_DIED:     errmsg = "Server disconnected"; break;
    case DMLERR_FIRST -1:        errmsg = "Bad item argument type"; break;   
    default:                     errmsg = "Unknown error"; break;
    }
    errterm = PL_new_term();
    PL_unify_functor(errterm, PL_new_functor(PL_new_atom("error"), 1));
    unifyAtomic(PL_arg(errterm, 1), lookupAtom(errmsg));
    return pl_unify(value, errterm);
  }
}

#endif /*O_DDE*/
#endif /*__WINDOWS__*/
