/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include "version.h"			/* get MACHINE and PCE_VERSION */
#include <h/kernel.h>
#include "alloc.h"
#include <h/interface.h>
#include <h/trace.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <h/graphics.h>
#include <h/unix.h>
#include <errno.h>

#if defined(__linux__) || (defined(__sun__) && !STDC_HEADERS)
extern int gethostname(char *__name, size_t __len);
#endif

static void	callExitMessagesPce(int, Pce);
#ifndef O_RUNTIME
static status	debuggingPce(Pce pce, Bool val);
#endif

static status
initialisePce(Pce pce)
{ if ( PCE && notNil(PCE) )
    return errorPce(classOfObject(pce), NAME_cannotCreateInstances);

#ifndef O_RUNTIME
  assign(pce, debugging,          OFF);
  assign(pce, trace, 	          NAME_never);
#endif
  assign(pce, catched_errors,	  newObject(ClassChain, 0));
  assign(pce, catch_error_signals,OFF);
  assign(pce, print_c_stack,	  OFF);
  DEBUG_BOOT(assign(pce, print_c_stack, ON));

  assign(pce, exit_messages,	  newObject(ClassChain, 0));
  assign(pce, exception_handlers, newObject(ClassSheet, 0));

  assign(pce, home,		  CtoName("/usr/local/src/xpce"));

  assign(pce, version,            CtoName(PCE_VERSION));
  assign(pce, machine,            CtoName(MACHINE));
  assign(pce, operating_system,   CtoName(OS));
  assign(pce, xt_version,	  toInt(ws_version()));
  assign(pce, xt_revision,	  toInt(ws_revision()));
  assign(pce, features,		  newObject(ClassChain, 0));

  hostAction(HOST_ONEXIT, callExitMessagesPce, pce);

  succeed;
}


static status
writePcev(Pce pce, int argc, Any *argv)
{ int i;
  static char space[] = " ";

  for(i=0; i<argc; i++)
  { if ( i > 0 )
      hostAction(HOST_WRITE, space);

    if ( instanceOfObject(argv[i], ClassCharArray) )
      hostAction(HOST_WRITE, strName(argv[i]));
    else if ( isInteger(argv[i]) )
    { char buf[50];
      sprintf(buf, "%ld", valInt(argv[i]));
      hostAction(HOST_WRITE, buf);
    } else
    { char *s = pp(argv[i]);
      hostAction(HOST_WRITE, s);
      free_string(s);
    }
  }

  succeed;
}


static status
writeLnPcev(Pce pce, int argc, Any *argv)
{ static char nl[] = "\n";

  writePcev(pce, argc, argv);
  hostAction(HOST_WRITE, nl);

  succeed;
}


status
formatPcev(Pce pce, CharArray fmt, int argc, Any *argv)
{ char buf[FORMATSIZE];

  TRY(swritefv(buf, fmt, argc, argv));
  hostAction(HOST_WRITE, buf);
  
  succeed;
}

		 /*******************************
		 *      CONSOLE OPERATIONS	*
		 *******************************/

static status
exposeConsolePce(Pce pce)
{ ws_expose_console();

  succeed;
}


static status
iconifyConsolePce(Pce pce)
{ ws_iconify_console();

  succeed;
}


static status
consoleLabelPce(Pce pce, CharArray title)
{ ws_console_label(title);

  succeed;
}


		 /*******************************
		 *	  ERROR HANDLING	*
		 *******************************/

Name
getOsErrorPce(Pce pce)
{
#if HAVE_STRERROR
  return CtoName(strerror(errno));
#else
  static char errmsg[64];
  extern int sys_nerr;
  extern char *sys_errlist[];
  extern int errno;

  if ( errno < sys_nerr )
    return CtoName(sys_errlist[errno]);

  sprintf(errmsg, "Unknown OS Error (%d)", errno);
  return CtoName(errmsg);
#endif /*HAVE_STRERROR*/
}

#ifndef O_RUNTIME
static Chain
getUnresolvedTypesPce(Pce pce)
{ Chain ch = answerObject(ClassChain, 0);
  
  for_hash_table(TypeTable, s,
	         { Type t = s->value;
		   if ( t->kind == NAME_class )
		   { Class class = t->context;
		     if ( isNil(class->super_class) )
		       appendChain(ch, t);
		     if ( isName(class) )
		     { if ( (class = getMemberHashTable(classTable, class)) )
			 assign(t, context, class);
		       else
			 appendChain(ch, t);
		     }
		   }
		 });
  
  answer(ch);
}
#endif /*O_RUNTIME*/


static status
catchErrorPce(Pce pce, Any ids)
{ assign(pce, last_error, NIL);

  return prependChain(pce->catched_errors, ids);
}


static status
catchPopPce(Pce pce)
{ return deleteHeadChain(pce->catched_errors);
}


status
catchedErrorPce(Pce pce, Name id)
{ Cell cell;

  for_cell(cell, pce->catched_errors)
  { if ( (Name) cell->value == id )
      succeed;

    if ( instanceOfObject(cell->value, ClassChain) &&
	 memberChain(cell->value, id) )
      succeed;
  }

  fail;
}


Name
getEnvironmentVariablePce(Pce pce, Name name)
{ char *s;

  if ( (s = getenv(strName(name))) )
    answer(CtoName(s));
#if defined(__DOS__) || defined(__WINDOWS__)
  if ( stricmp(strName(name), "PCEHOME") == 0 )
#else
  if ( streq(strName(name), "PCEHOME") )
#endif
    answer(get(PCE, NAME_home, 0));

  fail;
}


static status
exitMessagePce(Pce pce, Code code)
{ return prependChain(pce->exit_messages, code);
}


static void
callExitMessagesPce(int stat, Pce pce)
{ Cell cell;

  for_cell(cell, pce->exit_messages)
    forwardCode(cell->value, toInt(stat), 0);
}

		/********************************
		*            DEBUGGING		*
		********************************/

static Bool
getIsRuntimeSystemPce(Pce pce)
{
#ifdef O_RUNTIME
  answer(ON);
#else
  answer(OFF);
#endif
}


#ifndef O_RUNTIME

static status
debugSubjectPce(Pce pce, Name what)
{ if ( !memberChain(PCEdebugSubjects, what) )
    appendChain(PCEdebugSubjects, what);

  return debuggingPce(pce, ON);
}


static status
nodebugSubjectPce(Pce pce, Name what)
{ deleteChain(PCEdebugSubjects, what);

  succeed;
}


static status
debuggingPce(Pce pce, Bool val)
{ assign(pce, debugging, val);

  PCEdebugging = (PCE->debugging == ON);

  succeed;
}


status
tracePce(Pce pce, Name val)
{ if ( isDefault(val) )
    val = NAME_user;

  assign(pce, trace, val);

  if ( val == NAME_never )
    TraceMode = TRACE_NEVER;
  else if ( val == NAME_user )
    TraceMode = TRACE_USER;
  else
    TraceMode = TRACE_ALWAYS;

  succeed;
}


static status
printStackPce(Pce pce, Int depth, Bool all)
{ traceBackPce(depth, all == ON ? NAME_always : NAME_user);

  succeed;
}
#endif /*O_RUNTIME*/


static status
maxGoalDepthPce(Pce pce, Int depth)
{ MaxGoalDepth = (isInteger(depth) ? valInt(depth) : PCE_MAX_INT);

  succeed;
}


static Int
getMaxGoalDepthPce(Pce pce)
{ answer(MaxGoalDepth == PCE_MAX_INT ? NIL : toInt(MaxGoalDepth));
}



		/********************************
		*           EXCEPTIONS		*
		********************************/

status
exceptionPcev(Pce pce, Name name, int argc, Any *argv)
{ Code msg;

  if ( (msg = checkType(getValueSheet(pce->exception_handlers, (Any)name),
			TypeCode, pce)) )
    return forwardCodev(msg, argc, argv);

  fail;
}


status
exceptionPce(Pce pce, Name kind, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc = 0;

  va_start(args, kind);
  for(; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);
  
  return exceptionPcev(pce, kind, argc, argv);
}


		/********************************
		*           STATISTICS		*
		********************************/

#ifndef HAVE_GETDTABLESIZE
#ifdef(HAVE_SYS_RESOURCE_H)
#include <sys/resource.h>
#if defined(__sun__) && !defined(STDC_HEADERS)
extern int getrlimit(int resource, struct rlimit *rlp);
#endif

int
getdtablesize(void)
{ struct rlimit rlp;
  (void) getrlimit(RLIMIT_NOFILE, &rlp);

  return (rlp.rlim_cur);
}

#else

int
getdtablesize(void)
{ return 32;				/* old *nix systems */
}

#endif /*HAVE_SYS_RESOURCE_H*/
#endif /*HAVE_GETDTABLESIZE*/

static Int
getFdPce(Pce pce)
{
#ifdef __WINDOWS__
  return toInt(ws_free_file_descriptors());
#else
  int i, cntr = 0;
  struct stat buf;
  int mx = getdtablesize();

  for (i=0; i<mx; i++)
  { if (fstat(i, &buf) == -1)
    { cntr++;
      continue;
    }
  }
  answer(toInt(cntr));
#endif
}


static Int
getCoreUsagePce(Pce pce)
{ answer(toInt(allocbytes));
}


static Int
getWastedCorePce(Pce pce)
{ answer(toInt(wastedbytes));
}


static Int
getDeferredUnallocedPce(Pce pce)
{ answer(toInt(deferredUnalloced));
}


static Int
getAnswerStackSizePce(Pce pce)
{ answer(countAnswerStack());
}


static Int
getNoCreatedPce(Pce pce)
{ answer(getNoCreatedClass(ClassObject));
}


static Int
getNoFreedPce(Pce pce)
{ answer(getNoFreedClass(ClassObject));
}


static Int
getMethodCallsPce(Pce pce)
{ answer(toInt(ExecuteCalls));
}


static status
bannerPce(Pce pce)
{ Name host = get(HostObject(), NAME_system, 0);

#ifdef __WINDOWS__
  writef("PCE %s (%s for %I%IMS-Windows %d.%d)\n",
#else
  writef("PCE %s (%s for %s-%s and X%dR%d)\n",
#endif
	 CtoString(getIsRuntimeSystemPce(pce) == ON
		   	? "Runtime system"
		        : "Development environment"),
	 pce->version,
	 pce->machine,
	 pce->operating_system,
	 pce->xt_version,
	 pce->xt_revision);

  writef("Copyright 1993, 1994, University of Amsterdam.  All rights reserved.\n");

  if ( host != NAME_unknown )
    writef("The host-language is %s\n", host);

  succeed;
}


static Int
count_subclasses(Class class)
{ Int rval = ONE;
  Cell cell;

  if ( notNil(class->sub_classes) )
    for_cell(cell, class->sub_classes)
      rval = add(rval, count_subclasses(cell->value));

  return rval;
}


static status
infoPce(Pce pce)
{ int classes;

  classes = valInt(count_subclasses(ClassObject));

  writef("Version:\n");
  writef("	Release:            %s\n", pce->version);
  writef("	System:             %s\n", pce->machine);
  writef("	Operating System:   %s\n", pce->operating_system);
#ifdef __WINDOWS__
  writef("	Window System:      MS-Windows %s.%s\n", pce->xt_version,
	 				       		 pce->xt_revision);
#else
  writef("	Window System:      X%sR%s\n", pce->xt_version,
	 				       pce->xt_revision);
#endif
  writef("\n");
  writef("Memory allocation:\n");
  writef("	Core in use:        %d Bytes\n", getCoreUsagePce(pce));
  writef("	Core wasted:        %d Bytes\n", getWastedCorePce(pce));
  writef("	Objects allocated:  %d\n",       getNoCreatedPce(pce));
  writef("	Objects freed:	    %d\n",       getNoFreedPce(pce));
  writef("	Objects in use:	    %d\n",       sub(getNoCreatedPce(pce),
						     getNoFreedPce(pce)));
  writef("\n");
  writef("Statistics:\n");
  writef("	Messages sent:	    %d\n",	 toInt(ExecuteCalls));

  writef("\n");
  writef("Other info:\n");
  writef("	Classes:            %d\n", toInt(classes));
  writef("\n");
  writef("Designed and implemented by:\n");
  writef("	Anjo Anjewierden\n");
  writef("	Jan Wielemaker\n");
  writef("\n");

  writef("Copyright (c) 1993,1994 University of Amsterdam.  All rights reserved.");
  writef("\n\n");

  succeed;
}

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>

static Real
getCpuTimePce(Pce pce, Name which)
{ struct tms buffer;
  float f;

  times(&buffer);
  if ( which == NAME_user )
    f = (float) buffer.tms_utime / 60.0;
  else if ( which == NAME_system )
    f = (float) buffer.tms_stime / 60.0;
  else
    f = (float) (buffer.tms_utime + buffer.tms_stime) / 60.0;

  answer(CtoReal(f));
}

#else /*HAVE_SYS_TIMES_H*/

/* DOS/Windows version */

static Real
getCpuTimePce(Pce pce, Name which)
{				/* TBD: warn on bad type? */
  return CtoReal((float) clock()/(float)CLOCKS_PER_SEC);
}

#endif /*HAVE_SYS_TIMES_H*/

static Int
getMaxIntegerPce(Pce pce)
{ answer(toInt(PCE_MAX_INT));
}


static Int
getMinIntegerPce(Pce pce)
{ answer(toInt(PCE_MIN_INT));
}


status
featurePce(Pce pce, Any feature)
{ return appendChain(pce->features, feature);
}


status
hasFeaturePce(Pce pce, Any feature)
{ return memberChain(pce->features, feature);
}


static StringObj
getDatePce(Pce pce)
{ time_t clock;
  char tmp[27];

  clock = time(0);
  strcpy(tmp, ctime(&clock));
  tmp[24] = '\0';
  answer(CtoString(tmp));
}

#ifdef __msdos__

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define getlogin() and gethostname() for the DOS/Windows version.
These functions are renamed to fool mkproto adding a possibly
conflicting prototype for them.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define gethostname _dosemu_gethostname
#define getlogin _dosemu_getlogin

static char *
_dosemu_getlogin()
{ char *user = getenv("USER");

  if ( !user )
    user = "unknown";

  return user;
}

static int
_dosemu_gethostname(char *buf, int len)
{ char *s;
  
  if ( (s = getenv("HOSTNAME")) != NULL )
    strcpy(buf, s);
  else
    strcpy(buf, "doshost");

  return 0;
}

#endif /*__msdos__*/

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

static Name
getUserPce(Pce pce)
{ char *s;

  if ( (s = getlogin()) )
    answer(CtoName(s));
#if HAVE_PWD_H
  { struct passwd *pwd;

    if ( (pwd = getpwuid(getuid())) )
      answer(CtoName(pwd->pw_name));
  }
#endif

  fail;
}


static Any
getUserInfoPce(Pce pce, Name what, Name user)
{ 
#if HAVE_PWD_H
  struct passwd *pwd;

  if ( isDefault(user) )
    pwd = getpwuid(getuid());
  else
    pwd = getpwnam(strName(user));

  if ( pwd )
  { if ( what == NAME_name )
      answer(CtoName(pwd->pw_name));
    else if ( what == NAME_password )
      answer(CtoName(pwd->pw_passwd));
    else if ( what == NAME_userId )
      answer(toInt(pwd->pw_uid));
    else if ( what == NAME_groupId )
      answer(toInt(pwd->pw_gid));
    else if ( what == NAME_gecos )
      answer(CtoName(pwd->pw_gecos));
    else if ( what == NAME_home )
      answer(CtoName(pwd->pw_dir));
    else if ( what == NAME_shell )
      answer(CtoName(pwd->pw_shell));
  }
#endif /*HAVE_PWD_H*/

  fail;
} 


Name
getHostnamePce(Pce pce)
{ char buf[LINESIZE];

  if ( gethostname(buf, LINESIZE) )
  { errorPce(pce, NAME_hostname, getOsErrorPce(pce));
    fail;
  }

  answer(CtoName(buf));
}


static Int
getPidPce(Pce pce)
{ answer(toInt(getpid()));
}

/* (JW)	When switched on pce will catch all normal error signals, print the
	C-stack and exit normally. Otherwise a core dump is produced.
 */

status
catchErrorSignalsPce(Pce pce, Bool val)
{ if ( pce->catch_error_signals != val )
  { assign(pce, catch_error_signals, val);
    catchErrorSignals(val);
  }

  succeed;
}



		/********************************
		*       DISPLAY MANAGEMENT	*
		********************************/

static status
informPce(Pce pce, CharArray fmt, int argc, Any *argv)
{ Any d = CurrentDisplay(NIL);
  char buf[FORMATSIZE];

  if ( d != NULL && getOpenDisplay(d) == ON )
    return informDisplay(d, fmt, argc, argv);

  TRY(swritefv(buf, fmt, argc, argv));
  strcat(buf, "\n");
  hostAction(HOST_WRITE, buf);

  succeed;
}


static status
confirmPce(Pce pce, CharArray fmt, int argc, Any *argv)
{ Any d = CurrentDisplay(NIL);
  char buf[FORMATSIZE];
  int try;

  if ( d != NULL && getOpenDisplay(d) == ON )
    return confirmDisplay(d, fmt, argc, argv);

  TRY(swritefv(buf, fmt, argc, argv));
  strcat(buf, " (y/n) ? ");
  
  for(try = 0; try < 3; try++)
  { int c;

    hostAction(HOST_WRITE, buf);
    hostAction(HOST_FLUSH);

    switch(c = hostGetc())
    { case EOF:
      case 04:
	break;
      case 'n':
	fail;
      case 'y':
	succeed;
      default:
	writef("Please answer `y' or `n'\n");
    }
  }
  
  hostAction(HOST_HALT);
  exit(1);
  fail;					/* fool compiler */
}


		/********************************
		*         CASE HANDLING		*
		********************************/

static status
syntaxPce(Pce pce, Name casemap, Int ws)
{ Code msg;

					/* realise all classes */
  msg = answerObject(ClassMessage, Arg(2), NAME_realise, 0);
  send(classTable, NAME_forAll, msg, 0);
  doneObject(msg);

  if ( isDefault(ws) )
    ws = toInt('_');

  msg = answerObject(ClassMessage, Arg(1), NAME_syntax, casemap, ws, 0);
  DEBUG(NAME_name, checkNames(1));
  TRY(forNamePce(pce, msg));
  DEBUG(NAME_name, checkNames(1));
  doneObject(msg);

  char_flags[(int)syntax.word_separator] = PU;
  char_flags[valInt(ws)] = WS;
  syntax.word_separator = valInt(ws);
  syntax.uppercase = (casemap == NAME_uppercase);

  succeed;
}


		/********************************
		*         MISCELLANEOUS		*
		********************************/

static status
failPce(Pce pce)
{ fail;
}


static status
succeedPce(Pce pce)
{ succeed;
}


#ifndef O_RUNTIME
static status
benchPce(Pce pce, Message msg, Int count, Name how)
{ int cnt = valInt(count);

  if ( how == NAME_forward )
  { while( cnt-- > 0 )
      forwardCodev((Code) msg, 0, NULL);
  } else if ( how == NAME_execute )
  { while( cnt-- > 0 )
      ExecuteMessage(msg);
  } else
  { Any receiver  = msg->receiver;
    Name selector = msg->selector;
    Any *argv;
    int argc;

    if ( isNil(msg->arguments) )
    { argv = NULL;
      argc = 0;
    } else
    { argv = msg->arguments->elements;
      argc = valInt(msg->arguments->size);
    }

    if ( how == NAME_send )
    { while( cnt-- > 0 )
	sendv(receiver, selector, argc, argv);
    } else if ( how == NAME_qad )
    { while( cnt-- > 0 )
	qadSendv(receiver, selector, argc, argv);
    } else /* if ( how == NAME_invoke ) */
    { Any implementation;
      Class cl;

      TRY(implementation = resolveSendMethodObject(receiver, NULL, selector,
						   &receiver, NULL));
      cl = classOfObject(implementation);
      while( cnt-- > 0 )
	(*cl->send_function)(implementation, receiver, argc, argv);
    }
  }

  succeed;
}
#endif /*O_RUNTIME*/

status
resetPce(Pce pce)
{ Any dm;

  if ( notNil(pce) )
  {
#ifndef O_RUNTIME
    tracePce(pce, NAME_never);
#endif
    clearChain(pce->catched_errors);
  }

  changedLevel = 0;
  resetDebugger();
  resetAnswerStack();
  resetTypes();
  resetVars();
  resetDraw();

  if ( (dm = getObjectAssoc(NAME_displayManager)) )
    send(dm, NAME_reset, 0);

  succeed;
}


static status
diePce(Pce pce, Int rval)
{ if ( isDefault(rval) )
    rval = ZERO;

  hostAction(HOST_HALT, valInt(rval));
					/* should not get here */
  callExitMessagesPce(valInt(rval), pce);
  killAllProcesses();			/* should be done by above */
  exit(valInt(rval));
  fail;					/* should not be reached */
}


static Name
getPostscriptHeaderPce(Pce pce)
{ char buf[LINESIZE];

  sprintf(buf, "%s/postscript/pce.ps",
	  strName(get(pce, NAME_home, 0)));
  
  answer(CtoName(buf));
}


static Any
getInstancePcev(Pce pce, Class class, int argc, Any *argv)
{ answer(answerObjectv(class, argc, argv));
}


static Any
getConvertPce(Pce pce, Any obj, Type type)
{ answer(checkType(obj, type, pce));
}


static status
makeClassUsingCode(Class class)
{ if ( notNil(class->make_class_message) )
    return forwardCode(class->make_class_message, class->name, 0);

  fail;
}


static status
defineClassPce(Pce pce, Name name, Name super, StringObj summary, Code msg)
{ Class class;

  TRY(class = nameToTypeClass(name));

  if ( notDefault(class->realised) )	/* already existing */
  { Class superclass;

    TRY(superclass = nameToTypeClass(super));
    if ( notNil(class->super_class) && class->super_class->name != super )
      return errorPce(class, NAME_cannotChangeSuperClass);
  } else
  { class = defineClass(name, super, summary, makeClassUsingCode);
    assign(class, make_class_message, msg);
  }

  succeed;
}


		/********************************
		*           REFERENCES		*
		********************************/

Any
getObjectFromReferencePce(Pce pce, Any ref)
{ Any rval;

  if ( isInteger(ref) )
  { rval = IntToPointer(ref);

    if ( isProperObject(rval) && !isFreedObj(rval) )
      answer(rval);
  } else
  { assert(isName(ref));

    answer(findGlobal(ref));
  }

  fail;
}


static status
forNameReferencePce(Pce pce, Code msg)
{ return forSomeAssoc(msg);
}


static status
renameReferencePce(Pce pce, Name old, Name new)
{ return renameAssoc(old, new);
}


status
makeClassPce(Class class)
{ sourceClass(class, makeClassPce, __FILE__, "$Revision$");

#ifndef O_RUNTIME
  localClass(class, NAME_debugging, NAME_debugging, "bool", NAME_get,
	     "Add consistency checks");
  localClass(class, NAME_trace,NAME_debugging, "{never,user,always}", NAME_get,
	     "Trace message passing");
#endif
  localClass(class, NAME_lastError, NAME_exception, "name*", NAME_both,
	     "Id of last occurred error");
  localClass(class, NAME_catchedErrors, NAME_exception, "chain", NAME_get,
	     "Errors are expected by code");
  localClass(class, NAME_catchErrorSignals, NAME_debugging, "bool", NAME_get,
	     "Trap Unix signals to deal with errors");
  localClass(class, NAME_printCStack, NAME_debugging, "bool", NAME_both,
	     "Print C-stack on fatal error");

  localClass(class, NAME_exitMessages, NAME_quit, "chain", NAME_get,
	     "Executed when the process terminates");
  localClass(class, NAME_exceptionHandlers, NAME_exception, "sheet", NAME_get,
	     "Exception-name -> handler mapping");
  localClass(class, NAME_home, NAME_environment, "name", NAME_both,
	     "PCE's home directory");

  localClass(class, NAME_version, NAME_version, "name", NAME_get,
	     "Version indication");
  localClass(class, NAME_machine, NAME_version, "name", NAME_get,
	     "Name of this machine/architecture");
  localClass(class, NAME_operatingSystem, NAME_version, "name", NAME_get,
	     "Name of operating system");
  localClass(class, NAME_xtVersion, NAME_version, "int", NAME_get,
	     "Version of Xt library used to compile xpce");
  localClass(class, NAME_xtRevision, NAME_version, "int", NAME_get,
	     "Revision of Xt library used to compile xpce");
  localClass(class, NAME_features, NAME_version, "chain", NAME_get,
	     "List of installed features");

  termClass(class, "pce", 1, NAME_version);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  storeMethod(class, NAME_catchErrorSignals, catchErrorSignalsPce);
#ifndef O_RUNTIME
  storeMethod(class, NAME_debugging, debuggingPce);
#endif

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create @pce (done only once)",
	     initialisePce);
#ifndef O_RUNTIME
  sendMethod(class, NAME_trace, NAME_debugging, 1,
	     "level=[{never,user,always}]",
	     "Set trace mode (default = user)",
	     tracePce);
  sendMethod(class, NAME_printStack, NAME_debugging, 2,
	     "depth=[int]", "always=[bool]",
	     "Print PCE message stack to host-window",
	     printStackPce);
  sendMethod(class, NAME_debugSubject, NAME_debugging, 1, "subject=name",
	     "Report internal event on terminal",
	     debugSubjectPce);
  sendMethod(class, NAME_nodebugSubject, NAME_debugging, 1, "subject=name",
	     "Don't Report internal event on terminal",
	     nodebugSubjectPce);
#endif
  sendMethod(class, NAME_banner, NAME_initialise, 0,
	     "Write standard banner to terminal",
	     bannerPce);
  sendMethod(class, NAME_die, NAME_quit, 1, "status=[int]",
	     "Exit this (Unix) process with status",
	     diePce);
  sendMethod(class, NAME_info, NAME_debugging, 0,
	     "Write statistics/info to terminal",
	     infoPce);
  sendMethod(class, NAME_confirm, NAME_report, 2,
	     "format=char_array", "argument=any ...",
	     "Test if the user confirms string",
	     confirmPce);
  sendMethod(class, NAME_inform, NAME_report, 2,
	     "format=char_array", "argument=any ...",
	     "Inform the user of something",
	     informPce);
  sendMethod(class, NAME_write, NAME_report, 1, "argument=any ...",
	     "Write arguments, separated by spaces",
	     writePcev);
  sendMethod(class, NAME_writeLn, NAME_report, 1, "argument=any ...",
	     "Write arguments, separated by spaces, add nl",
	     writeLnPcev);
  sendMethod(class, NAME_format, NAME_report, 2,
	     "format=char_array", "argument=any ...",
	     "Formatted like C's printf",
	     formatPcev);
  sendMethod(class, NAME_exposeConsole, NAME_console, 0,
	     "Expose the PCE/host console window",
	     exposeConsolePce);
  sendMethod(class, NAME_iconifyConsole, NAME_console, 0,
	     "Make PCE/host console window an icon",
	     iconifyConsolePce);
  sendMethod(class, NAME_consoleLabel, NAME_console, 1, "char_array",
	     "Set the label for the console window",
	     consoleLabelPce);
  sendMethod(class, NAME_exception, NAME_exception, 2,
	     "identifier=name", "context=any ...",
	     "Raise an exception",
	     exceptionPcev);
  sendMethod(class, NAME_exitMessage, NAME_quit, 1, "message=code",
	     "Execute code while dying",
	     exitMessagePce);
  sendMethod(class, NAME_defineClass, NAME_class, 4,
	     "name=name", "super=name", "summary=[string]", "realise=code",
	     "Declare a class without details",
	     defineClassPce);

{ extern listWastedCorePce(Pce pce, Bool ppcells);
  extern forNamePce(Pce pce, Code code);

#ifndef O_RUNTIME
  sendMethod(class, NAME_listWastedCore, NAME_debugging, 1,
	     "list_content=[bool]",
	     "List wasted core map",
	     listWastedCorePce);
#endif
  sendMethod(class, NAME_forName, NAME_name, 1, "message=code",
	     "Execute code on all defined names",
	     forNamePce);
}

  sendMethod(class, NAME_syntax, NAME_host, 2,
	     "syntax={uppercase}", "word_separator=[char]",
	     "Specify language compatible syntax",
	     syntaxPce);

#ifndef O_RUNTIME
  sendMethod(class, NAME_bench, NAME_statistics, 3,
	     "message=message", "times=int",
	     "how={forward,execute,qad,send,invoke}",
	     "Benchmark for message passing",
	     benchPce);
#endif
  sendMethod(class, NAME_fail, NAME_control, 0,
	     "Simply fails",
	     failPce);	
  sendMethod(class, NAME_succeed, NAME_control, 0,
	     "Simply succeeds",
	     succeedPce);

  sendMethod(class, NAME_catchError, NAME_exception, 1, "identifier=name|chain",
	     "Indicate code is prepared to handle errors",
	     catchErrorPce);
  sendMethod(class, NAME_catchPop, NAME_exception, 0,
	     "Pop pushed error handlers",
	     catchPopPce);
  sendMethod(class, NAME_catched, NAME_exception, 1, "identifier=name",
	     "Test if error_id is catched",
	     catchedErrorPce);
  sendMethod(class, NAME_forNameReference, NAME_reference, 1, "message=code",
	     "Run code on all name references (global objects)",
	     forNameReferencePce);
  sendMethod(class, NAME_renameReference, NAME_reference, 2,
	     "old=name", "new=name",
	     "Rename global reference",
	     renameReferencePce);
  sendMethod(class, NAME_maxGoalDepth, NAME_debugging, 1, "[int]*",
	     "Set maximum recursion level",
	     maxGoalDepthPce);
  sendMethod(class, NAME_feature, NAME_version, 1, "any",
	     "Define new feature",
	     featurePce);
  sendMethod(class, NAME_hasFeature, NAME_version, 1, "any",
	     "Test if feature is defined",
	     hasFeaturePce);
  
  getMethod(class, NAME_coreUsage, NAME_statistics, "bytes=int", 0,
	    "Total core in active use",
	    getCoreUsagePce);
  getMethod(class, NAME_deferredUnalloced, NAME_statistics, "number=int", 0,
	    "# freed referenced objects",
	    getDeferredUnallocedPce);
  getMethod(class, NAME_coreWasted, NAME_statistics, "bytes=int", 0,
	    "Core requested from system, but not in use",
	    getWastedCorePce);
  getMethod(class, NAME_cpuTime, NAME_statistics, "seconds=real", 1,
	    "kind=[{user,system}]",
	    "Total CPU time for this process",
	    getCpuTimePce);
  getMethod(class, NAME_objectsAllocated, NAME_statistics, "number=int", 0,
	    "Total number of objects created",
	    getNoCreatedPce);
  getMethod(class, NAME_objectsFreed, NAME_statistics, "number=int", 0,
	    "Total number of objects freed",
	    getNoFreedPce);
  getMethod(class, NAME_answerStackSize, NAME_statistics, "cells=int", 0,
	    "Number of cells (objects) in `answer' state",
	    getAnswerStackSizePce);
  getMethod(class, NAME_methodCalls, NAME_statistics, "calls=int", 0,
	    "Number of methods called",
	    getMethodCallsPce);

  getMethod(class, NAME_date, NAME_time, "string", 0,
	    "Unix's standard time string for now",
	    getDatePce);
  getMethod(class, NAME_fd, NAME_file, "number=int", 0,
	    "Number of free file descriptors",
	    getFdPce);
  getMethod(class, NAME_maxInteger, NAME_limit, "value=int", 0,
	    "Highest representable integer",
	    getMaxIntegerPce);
  getMethod(class, NAME_minInteger, NAME_limit, "value=int", 0,
	    "Lowest representable integer",
	    getMinIntegerPce);
  getMethod(class, NAME_pid, NAME_process, "identifier=int", 0,
	    "Process id of this process",
	    getPidPce);
  getMethod(class, NAME_user, NAME_environment, "user=name", 0,
	    "Login name of user",
	    getUserPce);
  getMethod(class, NAME_hostname, NAME_environment, "host=name", 0,
	    "Name of host on which PCE runs",
	    getHostnamePce);
  getMethod(class, NAME_postscriptHeader, NAME_postscript, "path=name", 0,
	    "Find path-name of PostScript header",
	    getPostscriptHeaderPce);
  getMethod(class, NAME_osError, NAME_report, "identifier=name", 0,
	    "Name of last operating system error",
	    getOsErrorPce);
  getMethod(class, NAME_instance, NAME_oms, "created=object|function", 2,
	    "class=class", "argument=unchecked ...",
	    "Create instance of any class",
	    getInstancePcev);
  getMethod(class, NAME_convert, NAME_conversion, "converted=unchecked", 2,
	    "object=unchecked", "type=type",
	    "Convert anything to specified type",
	    getConvertPce);
  getMethod(class, NAME_environmentVariable, NAME_environment, "value=name", 1,
	    "name=name",
	    "Unix environment variable (getenv)",
	    getEnvironmentVariablePce);
  getMethod(class, NAME_objectFromReference, NAME_oms, "object=unchecked", 1,
	    "reference=int|name",
	    "Convert object-name or integer reference into object",
	    getObjectFromReferencePce);
  getMethod(class, NAME_userInfo, NAME_environment, "value=name|int", 2,
	    "field={name,password,user_id,group_id,gecos,home,shell}",
	    "user=[name]",
	    "Get information on user (from the passwd file)",
	    getUserInfoPce);

#ifndef O_RUNTIME
  getMethod(class, NAME_cSymbolFile, NAME_debugging, "path=name", 0,
	    "Name of Unix format symbol-file",
	    getCSymbolFilePce);
  getMethod(class, NAME_cFunctionName, NAME_debugging, "function=name", 1,
	    "address=int",
	    "Translate address into C-function name",
	    getCFunctionNamePce);
  getMethod(class, NAME_unresolvedTypes, NAME_debugging, "chain", 0,
	    "New chain with unresolved types",
	    getUnresolvedTypesPce);
#endif

  getMethod(class, NAME_maxGoalDepth, NAME_debugging, "int*", 0,
	    "Maximum recursion level",
	    getMaxGoalDepthPce);
  getMethod(class, NAME_isRuntimeSystem, NAME_runtime, "bool", 0,
	    "@on if this is the runtime library",
	    getIsRuntimeSystemPce);

  PCE = globalObject(NAME_pce, ClassPce, 0);
  protectObject(PCE);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reinitialise after restoring from some kind of saved-state
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
pceReInitialise(int argc, char **argv)
{ PCEargc = argc;
  PCEargv = argv;

  if ( PCE->catch_error_signals == ON )
    catchErrorSignals(ON);
  ExecuteCalls = 0L;

  succeed;
}


status
pceInitialise(int handles, int argc, char **argv)
{ AnswerMark mark;

  if ( XPCE_initialised )
    succeed;

  XPCE_initialised = TRUE;
  inBoot = TRUE;

  PCEargc = argc;
  PCEargv = argv;

  MaxGoalDepth = PCE_MAX_INT;
  initAnswerStack();

#ifndef O_RUNTIME
  PCEdebugging = FALSE;
  if ( getenv("PCEDEBUGBOOT") != NULL )
  { PCEdebugBoot = TRUE;
    DEBUG_BOOT(Cprintf("Debugging boot cycle\n"));
  } else
    PCEdebugBoot = FALSE;
#endif

  PCE = NIL;
  pceReset();				/* reset important globals */
  markAnswerStack(mark);
  syntax.word_separator = '_';

  setProtectedObj(NIL);
  setProtectedObj(DEFAULT);
  setProtectedObj(ON);
  setProtectedObj(OFF);

  DEBUG_BOOT(Cprintf("Alloc ...\n"));
  initAlloc();
  allocRange(&ConstantNil, sizeof(struct constant));
  allocRange(&BoolOff,     sizeof(struct bool));
  initNamesPass1();
  DEBUG_BOOT(Cprintf("Types ...\n"));
  initTypes();
  DEBUG_BOOT(Cprintf("Names ...\n"));
  initCharArrays();
  initNamesPass2();
  DEBUG_BOOT(Cprintf("Name Assocs ...\n"));
  initAssoc(handles);

{ Type t;
  t = createType(CtoName("any ..."), NAME_any, NIL);
  vectorType(t, ON);
}

  DEBUG_BOOT(Cprintf("Boot classes ...\n"));

  ClassObject =
    bootClass(NAME_object,		/* Name */
	      (Name) NIL,		/* SuperClass */
	      sizeof(struct object),	/* Instance size */
	      1,			/* # PCE typed slots */
	      initialiseObject,		/* Initialisation function */
	      0);

  ClassChain = 
    bootClass(NAME_chain,
	      NAME_object,
	      sizeof(struct chain),
	      0,
	      initialiseChainv,
	      1, "any ...");

  ClassProgramObject =
    bootClass(NAME_programObject,
	      NAME_object,
	      sizeof(struct program_object),
	      1,
	      initialiseProgramObject,
	      0);

  ClassType =
    bootClass(NAME_type,
	      NAME_programObject,
	      sizeof(struct type),
	      6,
	      initialiseType,
	      4, "name", "name", "any", "any");	/* changed later!! */
  lookupBootClass(ClassType, (Func) getLookupType, 1, "name");

  ClassSourceLocation =
    bootClass(NAME_sourceLocation,
	      NAME_object,
	      sizeof(struct source_location),
	      2,
	      initialiseSourceLocation,
	      2, "name", "[int]*");

  ClassVector =
    bootClass(NAME_vector,
	      NAME_object,
	      sizeof(struct vector),
	      2,
	      initialiseVectorv,
	      1, "any ...");

  ClassHashTable =
    bootClass(NAME_hashTable,
	      NAME_object,
	      sizeof(struct hash_table),
	      1,
	      initialiseHashTable,
	      1, "[int]");

  ClassBehaviour =
    bootClass(NAME_behaviour,
	      NAME_programObject,
	      sizeof(struct behaviour),
	      2,
	      initialiseBehaviour,
	      0);

  ClassMethod =
    bootClass(NAME_method,
	      NAME_behaviour,
	      sizeof(struct method),
	      5,
	      initialiseMethod,
	      6, "name", "[vector]", "code", "[string]*",
	         "[source_location]*", "[name]*");

  ClassSendMethod =
    bootClass(NAME_sendMethod,
	      NAME_method,
	      sizeof(struct send_method),
	      0,
	      initialiseMethod,
	      6, "name", "[vector]", "code",
	         "[string]*", "[source_location]*", "[name]*");

  ClassGetMethod =
    bootClass(NAME_getMethod,
	      NAME_method,
	      sizeof(struct get_method),
	      0,
	      initialiseGetMethod,
	      7, "name", "[type]", "[vector]", "code",
	         "[string]*", "[source_location]*", "[name]*");

  ClassCharArray =
    bootClass(NAME_charArray,
	      NAME_object,
	      sizeof(struct char_array),
	      0,
	      initialiseCharArray,
	      1, "char_array");

  ClassName =
    bootClass(NAME_name,
	      NAME_charArray,
	      sizeof(struct name),
	      1,
	      initialiseName,
	      1, "char_array");

  ClassString =
    bootClass(NAME_string,
	      NAME_charArray,
	      sizeof(struct string),
	      0,
	      initialiseStringv,
	      2, "[name]", "any ...");

  ClassTuple =
    bootClass(NAME_tuple,
	      NAME_object,
	      sizeof(struct tuple),
	      2,
	      initialiseTuple,
	      2, "any", "any");

  ClassVmi =
    bootClass(NAME_vmi,
	      NAME_programObject,
	      sizeof(struct vmi),
	      1,
	      initialiseVmi,
	      1, "name");

  DEBUG_BOOT(Cprintf("Initialised boot classes\n"));
  
  initGlobals();
  classTable		= globalObject(NAME_classes,       ClassHashTable, 0);
#ifndef O_RUNTIME
  PCEdebugSubjects	= globalObject(NAME_DebugSubjects, ClassChain, 0);
#endif
  initDebugger();

  TypeTable->class = ClassHashTable;
  newAssoc(NAME_types, TypeTable);
  createdClass(ClassHashTable, TypeTable, NAME_new);

  TypeExpression = newObject(ClassType, NAME_expression, NAME_compound, 0);
  superType(TypeExpression, TypeInt);
  superType(TypeExpression, nameToType(NAME_function));
  superType(TypeExpression, nameToType(NAME_number));
  superType(TypeExpression, nameToType(NAME_real));
  superType(TypeExpression, nameToType(NAME_var));

  TypeCode     = nameToType(NAME_code);
  TypeImage    = nameToType(NAME_image);
  TypeColour   = nameToType(NAME_colour);
  TypeEquation = CtoType("=");

  ObjectConstraintTable = globalObject(NAME_objectConstraintTable,
				       ClassHashTable, 0);
  ObjectAttributeTable  = globalObject(NAME_objectAttributeTable,
				       ClassHashTable, 0);
  ObjectSendMethodTable = globalObject(NAME_objectSendMethodTable,
				       ClassHashTable, 0);
  ObjectGetMethodTable  = globalObject(NAME_objectGetMethodTable,
				       ClassHashTable, 0);
  ObjectRecogniserTable = globalObject(NAME_objectRecogniserTable,
				       ClassHashTable, 0);
  ObjectHyperTable      = globalObject(NAME_objectHyperTable,
				       ClassHashTable, 0);

  name_procent_s	= CtoName("%s");

  DEBUG_BOOT(Cprintf("Building class definitions"));
  initClassDefs();
  DEBUG_BOOT(Cprintf("Realising Boot classes ...\n"));
  realiseBootClass(ClassObject);
  realiseBootClass(ClassChain);
  realiseBootClass(ClassProgramObject);
  realiseBootClass(ClassType);
  realiseBootClass(ClassSourceLocation);
  realiseBootClass(ClassVector);
  realiseBootClass(ClassHashTable);
  realiseBootClass(ClassBehaviour);
  realiseBootClass(ClassMethod);
  realiseBootClass(ClassSendMethod);
  realiseBootClass(ClassGetMethod);
  realiseBootClass(ClassCharArray);
  realiseBootClass(ClassName);
  realiseBootClass(ClassString);
  realiseBootClass(ClassTuple);
  realiseBootClass(ClassVmi);
  DEBUG_BOOT(Cprintf("Boot classes realised.\n"));
  initTypeAliases();

  { for_hash_table(classTable, s,
		   { Class class = s->value;
		     if ( class->no_created != class->no_freed &&
			  class->realised == OFF )
		       realiseClass(class);
		   });
  }

  realiseClass(ClassPce);		/* make @pce */
  realiseClass(ClassVar);		/* @arg1, ... */
  realiseClass(ClassConstant);		/* @default, @nil */
  realiseClass(ClassBool);		/* @on, @off */

  DEBUG_BOOT(Cprintf("Defining features\n"));

#ifdef HAVE_FORK
  featurePce(PCE, NAME_process);
#endif
#ifdef HAVE_SOCKET
  featurePce(PCE, NAME_socket);
#endif

  DEBUG_BOOT(Cprintf("C++ global objects\n"));
#if O_CPLUSPLUS
  initCPlusPlusGlobals();
#endif

  rewindAnswerStack(mark, NIL);
  inBoot = FALSE;

#ifdef __WINDOWS__
  rlc_word_char('@', TRUE);
#endif

  DEBUG_BOOT(Cprintf("Pce initialisation complete.\n"));
  succeed;
}
