/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>
#include <h/interface.h>

static void initErrorDatabase(HashTable db);

static status
initialiseError(Error e, Name id, StringObj format, Name kind, Name feedback)
{ if ( isDefault(kind) )     kind     = NAME_warning;
  if ( isDefault(feedback) ) feedback = NAME_report;

  assign(e, id,       id);
  assign(e, format,   format);
  assign(e, kind,     kind);
  assign(e, feedback, feedback);

  lockObject(e, ON);
  appendHashTable(ErrorTable, e->id, e);

  succeed;
}


Error
getConvertError(Class class, Name id)
{ if ( ErrorTable )
    return getMemberHashTable(ErrorTable, id);

  if ( !inBoot )
    realiseClass(ClassError);
  if ( ErrorTable )
    return getMemberHashTable(ErrorTable, id);

  fail;
}


static status
displayError(Error e, int argc, Any *argv)
{ char buf[FORMATSIZE];

  if ( equalName(e->feedback, NAME_report) )
  { ArgVector(av, argc+2);
    int i;

    av[0] = e->kind;
    av[1] = e->format;
    for(i=0; i<argc; i++)
      av[i+2] = argv[i];

    sendv(argv[0], NAME_report, argc+2, av);
  } else
  { if ( e->kind == NAME_inform || e->kind == NAME_status )
      writef("[PCE: ");
    else
      writef("[PCE %s: ", e->kind);

    if ( swritefv(buf, (CharArray) e->format, argc, argv) )
      hostAction(HOST_WRITE, buf);

#ifndef O_RUNTIME
    if ( e->kind == NAME_fatal ||
	 (e->feedback == NAME_print &&
	  e->kind != NAME_inform &&
	  e->kind != NAME_status &&
	  e->kind != NAME_warning) )
    { writef("\n\tin: ");
      pceWriteErrorGoal();
      send(PCE, NAME_exposeConsole, 0);
      writef("%c", toInt(07));		/* ^G: ASCII bell */
      tracePce(PCE, NAME_user);
    }
#endif

    writef("]\n");
  }
  
  succeed;
}


status
makeClassError(Class class)
{ sourceClass(class, makeClassError, __FILE__, "$Revision$");
  termClass(class, "error", 2, NAME_id, NAME_format, NAME_kind, NAME_feedback);

  localClass(class, NAME_id, NAME_name, "name", NAME_get,
	     "Unique identifier");
  localClass(class, NAME_format, NAME_report, "string", NAME_get,
	     "Format used to print the message");
  localClass(class, NAME_kind, NAME_report,
	     "{status,inform,warning,error,fatal,ignored}", NAME_both,
	     "Kind of message");
  localClass(class, NAME_feedback, NAME_report, "{report,print}", NAME_get,
	     "Where (how) the report is reported");

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "name=name", "format=string",
	     "kind=[{status,inform,warning,error,fatal,ignored}]",
	     "feedback=[{report,print}]",
	     "-> id, format, [kind], [feedback]",
	     initialiseError);
  sendMethod(class, NAME_display, NAME_report, 1, "any|function ...",
	     "Display the error message using context",
	     displayError);

  getMethod(class, NAME_convert, NAME_oms, "error", 1, "name",
	    "Convert id into error",
	    getConvertError);
  getMethod(class, NAME_lookup, NAME_oms, "error", 1, "name",
	    "Convert id into error",
	    getConvertError);

  ErrorTable = globalObject(NAME_errors, ClassHashTable, 0);
  initErrorDatabase(ErrorTable);

  succeed;
}


		/********************************
		*         ERROR DATABASE	*
		********************************/

#define ET_ERROR	0x00
#define ET_WARNING	0x01
#define ET_STATUS	0x02
#define ET_INFORM	0x03
#define ET_FATAL	0x04
#define ET_IGNORED	0x05
#define ET_MASK		0x0f

#define EF_PRINT	0x00
#define EF_REPORT	0x10
#define EF_MASK		0xf0

typedef struct error_def *ErrorDef;

struct error_def
{ Name id;
  int  flags;
  char *format;
};

static struct error_def errors[] =
{					/* Files */
  { NAME_badFile,		EF_REPORT,
    "%N: Not an %s file" },
  { NAME_badFileName,		EF_REPORT,
    "%N: Bad file name: %s" },
  { NAME_cannotStat,		EF_REPORT,
    "%N: Cannot get file attributes: %s" },
  { NAME_cannotSeekNonFile,	0,
    "%N: Cannot seek non-regular file" },
  { NAME_chdir,			EF_REPORT,
    "%N: Cannot change directory to %s: %s" },
  { NAME_mkdir,			EF_REPORT,
    "%N: Cannot make directory: %s" },
  { NAME_rmdir,			EF_REPORT,
    "%N: Cannot remove directory: %s" },
  { NAME_backupFile,		EF_REPORT,
    "%N: Cannot make a backup in %s: %s" },
  { NAME_openFile,		EF_REPORT,
    "%N: Cannot open for %s: %s" },
  { NAME_readDirectory,		EF_REPORT,
    "%N: Cannot read: %s" },
  { NAME_removeFile,		EF_REPORT,
    "%N: Cannot remove: %s" },
  { NAME_renameFile,		EF_REPORT,
    "%N: Cannot rename to %s: %s" },
  { NAME_ioError,		EF_REPORT,
    "%N: IO error: %s" },
  { NAME_noLimit,		0,
    "%I%N: Failed to get system limit: %s" },
  { NAME_seekFile,		EF_REPORT,
    "%N: Cannot seek to %d from %s: %s" },
  { NAME_cannotFindFile,	EF_REPORT,
    "%N: Cannot find. Path = \"%s\"" },
  { NAME_noFile,		EF_REPORT,
    "%N: No associated file" },
  { NAME_notOpenFile,		0,
    "%N: Not open in mode %s" },
  { NAME_cannotStartProcess,	EF_REPORT,
    "%N: Cannot start: %s" },
  { NAME_noPipe,		EF_REPORT,
    "%N: Cannot create pipe: %s" },
					/* Process */
  { NAME_unknownSignal,		0,
    "%O: Unknown signal: %s" },
  { NAME_outOfPtys,		EF_REPORT,
    "%O: Out of pseudo-tty's" },
  { NAME_openTty,		EF_REPORT,
    "%O: Cannot open terminal %s: %s" },
  { NAME_ioctlGet,		EF_REPORT,
    "%O: Failed to fetch parameters of %s: %s" },
  { NAME_ioctlSet,		EF_REPORT,
    "%O: Failed to set parameters of %s: %s" },
  { NAME_setControllingTty,	EF_REPORT,
    "%O: Failed to set controlling terminal" },
  { NAME_killedOnExit,		ET_STATUS,
    "%N: Process killed on exit" },
  { NAME_processExitStatus,	EF_REPORT,
    "%N: Process exit status %d" },
  { NAME_brokenPipe,		ET_IGNORED,
    "%N: Broken pipe" },

					/* C-symbols */
  { NAME_notEnoughMemory,	ET_WARNING|EF_REPORT,
    "%N: Not enough memory" },
  { NAME_loadingCSymbols,	ET_STATUS|EF_REPORT,
    "%N: Loading C-symbols\n\t(running %s)" },
  { NAME_CSymbolOffset,		ET_STATUS|EF_REPORT,
    "%N: Symbol offset: %d" },
  { NAME_cannotLoadCSymbols,	ET_WARNING|EF_REPORT,
    "%I: Failed to load C-symbols: %s" },
  { NAME_stackOverflow,		ET_ERROR,
    "%N: Stack overflow (@pce <-max_goal_depth: %d)" },

					/* Sockets  */
  { NAME_socket,		EF_REPORT,
    "%N: Cannot %s socket: %s" },
  { NAME_noHost,		EF_REPORT,
    "%N: Cannot find host %s" },
  { NAME_noDomain,		0,
    "%N: no domain and cannot infer default" },
  { NAME_hostname,		0,
    "%N: cannot get hostname: %s" },

					/* Resources */
  { NAME_incompatibleResource,	0,
    "%N: Resource has incompatible type" },
  { NAME_noResource,		0,
    "%N: No associated resource" },
  { NAME_resourceSyntaxError,	ET_WARNING,
    "%I: %N:%d Syntax error in resource-file" },
  { NAME_oldResourceFormat,	ET_WARNING,
    "%N: old fashioned resource syntax: %s" },

					/* Display */
  { NAME_noCurrentDisplay,	0,
    "%N: No current display" },
  { NAME_notSameDisplay,	0,
    "%N: Not on the same display: %N" },
  { NAME_noMainWindow,		ET_FATAL,
    "%N: Failed to create X-application-shell" },
  { NAME_noApplicationContext,	ET_FATAL,
    "%N: Failed to create X-application-context" },
					/* Colour/Cursor/Font, etc */
  { NAME_noNamedColour,		EF_REPORT,
    "%N: No colour named %s" },
  { NAME_noNamedCursor,		EF_REPORT,
    "%N: No cursor named %s" },
  { NAME_getSelection,		EF_REPORT,
    "%N: Cannot get %s selection: %s" },
  { NAME_cannotBecomeSelectionOwner, EF_REPORT,
    "%N: Cannot become selection owner" },

					/* Fonts */
  { NAME_noDefaultFont,		ET_FATAL,
    "%N: No default font defined (Pce.Display.no_font)" },
  { NAME_replacedFont,		ET_WARNING,
    "%N: Failed to open; replaced by %N" },
  { NAME_no16BitFontsSupported,	ET_WARNING,
    "%N: 16-bit fonts are not (yet) supported" },
  { NAME_noFontsInFamily, ET_WARNING,
    "%N: No fonts in font-family %s" },

					/* COLOURS */
  { NAME_replacedColour,	ET_WARNING,
    "%O: replaced by colour(%N)" },

					/* X-errors */
  { NAME_xOpen,			ET_FATAL,
    "%N: Xopen failed on %s" },
  { NAME_xError,		0,
    "%N: X-error" },
  { NAME_noXServer,		ET_FATAL,
    "%N: Failed to connect to X-server at `%s': %s\n"
    "*********************************************************************\n"
    "* You MUST be running the X11 Windowing environment.  If you are,   *\n"
    "* check the setting of your DISPLAY environment variable as well    *\n"
    "* the access rights to your X11 server.  See xauth(1) and xhost(1). *\n"
    "*********************************************************************"
  },
  { NAME_xMovedDisplay,		ET_STATUS,
    "%N: Moved to display %s" },
  { NAME_cannotGrabPointer,	ET_WARNING,
    "%N: Failed to grab pointer: %s" },
  { NAME_noRelatedXFont,	ET_WARNING,
    "%N: No related X-font" },
  { NAME_cannotConvertResource,	ET_WARNING,
    "%N: Failed to convert %s.  Trying default" },
  { NAME_cannotConvertResourceDefault,	ET_FATAL,
    "%N: Failed to default %s" },
					/* Save/Load */
  { NAME_newSaveVersion,	ET_IGNORED,
    "%N: Saved as version %d, current version is %d" },
  { NAME_cannotSaveObject,	0,
    "%O: Cannot save object: %s" },
  { NAME_noAssoc,		0,
    "%N: No external object @%s" },
  { NAME_loadMessage,		ET_STATUS,
    "%O: %s" },
  { NAME_illegalCharacter,	ET_FATAL,
    "%O: Illegal character (%c) at index %d" },
  { NAME_referencedObjectNotLoaded, ET_FATAL,
    "%N: Referenced object %O not loaded" },
  { NAME_noSavedClassDef,	ET_FATAL,
    "%N: Cannot find class-definition from id = %d" },
  { NAME_loadNoClass,		ET_WARNING,
    "%N: Referenced class %s does not exist" },
  { NAME_loadOldSlot,		ET_WARNING,
    "%N: Slot %s<-%s is is not in current class definition" },

					/* Types */
  { NAME_argumentType,		0,
    "%N: Argument %d (%s) should be a %s" },
  { NAME_unexpectedType,	0,
    "%O: Should be a %N" },
  { NAME_elementType,		0,
    "%O: Element %d is not a %N" },
  { NAME_cannotConvert,		0,
    "%N: Cannot convert %O" },
  { NAME_argumentCount,		0,
    "%N: Behaviour has %d arguments" },
  { NAME_noNamedArgument,	0,
    "%N: No argument named %s" },
  { NAME_unboundAfterBoundArgument, 0,
    "%N: un-named arguments cannot appear after named arguments" },
  { NAME_inconsistentArguments, 0,
    "%N: Inconsistent arguments" },
  { NAME_typeLoop,		0,
    "%N: Type translation loop for %O" },
  { NAME_noTypeKind,		0,
    "%N: Unknown type-kind: %s" },
  { NAME_badTypeSyntax,		0,
    "%N: Syntax error in type-specification" },

					/* text_item */
  { NAME_cannotConvertText,	EF_REPORT|ET_WARNING,
    "%N: Cannot convert `%s' to a %N" },
  { NAME_soleCompletion,	EF_REPORT|ET_STATUS,
    "%N: Sole completion" },
  { NAME_completeNoMatch,	EF_REPORT|ET_WARNING,
    "%N: No Match" },
					/* Text (editor, text_buffer) */
  { NAME_mismatchedBracket,	EF_REPORT|ET_WARNING,
    "%IMismatched bracket" },
  { NAME_noMatchingBracket,	EF_REPORT|ET_WARNING,
    "%INo matching bracket" },

					/* Tables */
  { NAME_badParameterKeyVector,	0,
    "%O: Bad parameter- or key-vector" },
  { NAME_badVectorSize,		0,
    "%O: Vector %O should have %d elements" },
  { NAME_noTable,		0,
    "%O: Table has no hash_tables" },
					/* Graphicals */
  { NAME_rotate90,		0,
    "%O: Graphicals may only be rotated with multiples of 90 degrees" },
  { NAME_alreadyShown,		0,
    "%O: %O is already shown in %O" },
  { NAME_nodeNotInTree,		0,
    "%O: Node is not part of a tree" },
  { NAME_alreadyHasParent,	0,
    "%O: Already has a parent" },
  { NAME_wouldBeCyclic,		0,
    "%O: operation would lead to a cycle" },
  { NAME_mustBeCreatedBefore,	0,
    "%O: Must be ->create'd before `%s'" },
  { NAME_badTexture,		0,
    "%N: Unknown texture" },
  { NAME_polyTooManyPoints,	0,
    "%N: Cannot draw polygons with more than %d points" },
  { NAME_tooManyScreenLines,	0,
    "%N: More than 500 lines???" },
					/* Dialog Items */
  { NAME_noDefaultLabel,	0,
    "%N: No default label for %s" },
  { NAME_graphicalNotDisplayed,	0,
    "%N: Cannot open popup on not-displayed graphical: %s" },
					/* PostScript */
  { NAME_noPostScriptHeader,	0,
    "%O: Failed to get postscript_header" },
  { NAME_mustBeOpenBeforePostscript, 0,
    "%O: Must be opened before <-postscript" },
					/* Arithmetic */
  { NAME_noVar,			0,
    "%N: Cannot find variable %N" },
  { NAME_multipleVar,		0,
    "%N: Variable %N occurs more than once" },
  { NAME_domainError,		0,
    "%N: Domain error: %s" },
					/* Message passing */
  { NAME_badSelector,		0,
    "%N: Illegal selector: %O" },
  { NAME_freedObject,		0,
    "%N: Freed object: %O" },
#ifndef O_RUNTIME
  { NAME_noBehaviour,		ET_WARNING,
    "%N: No implementation for: %O %s%s" },
#else
  { NAME_noBehaviour,		ET_WARNING,
    "%N: Failed on not-implemented method" },
#endif /*O_RUNTIME*/
  { NAME_noTextBehaviour,	ET_WARNING,
    "%O: No implementation for interactive function: ->%s" },
  { NAME_noClass,		0,
    "%N: Unknown class: %s" },
  { NAME_noImplementation,	0,
    "%N: Not implementated" },
  { NAME_badReturnValue,	0,
    "%N: Return of incompatible value: %O; return_type is %N" },
  { NAME_convertedReturnValue,	ET_STATUS,
    "%N: Converted return value: %O to %O" },
  { NAME_mustBeToReceiver,	0,
    "%O: Is not @receiver (= %O)" },
  { NAME_redefinedAssoc,	0,
    "%N: Object @%s already exists" },
  { NAME_changedLoop,		ET_IGNORED,
    "%N: Looping while forwarding changes" },
  { NAME_badVectorUsage,	0,
    "%N: Arguments: any..., vector, [int]" },
  { NAME_cannotExecute,		0,
    "%N: Cannot execute" },
  { NAME_noFunction,		0,
    "%N: is not a function" },
  { NAME_lastIsNoFunction,	0,
    "%N: Last statement of progn is not a function" },
  { NAME_evalFailed,		ET_WARNING,
    "%N: Failed to evaluate" },
  { NAME_initVariableFailed,	ET_WARNING,
    "%N: Init failed for %O" },
  { NAME_redeclaredVar,		0,
    "%N: Variable redeclared" },
  { NAME_unlinkFailed,		ET_WARNING,
    "%O: ->unlink failed" },
  { NAME_negativeRefCountInCreate, ET_WARNING,
    "%IReference-count of %O drops below zero (while creating/freeing)" },
  { NAME_negativeRefCount, 0,
    "%IReference-count of %O drops below zero" },
  { NAME_negativeCodeReferenceCount, ET_FATAL,
    "%O: Code reference-count drops below zero" },
					/* consistency-check (object) */
  { NAME_checkedObjects, 	ET_INFORM,
    "%IChecked %d objects" },
  { NAME_noExtension, 		ET_WARNING,
    "%O: No attribute of extension %s" },
  { NAME_noProperObject,	ET_WARNING,
    "%O: Not a proper object" },
  { NAME_creating,		ET_WARNING,
    "%O: Creating flag set" },
  { NAME_badSlotValue,		ET_WARNING,
    "%O: Illegal value in slot %N: %s" },
  { NAME_failedToConvert,	ET_WARNING,
    "%O: Failed to convert %s for slot %N" },
  { NAME_badSlotValue,		ET_WARNING,
    "%O: Illegal value in slot %N: %s" },
  { NAME_freedSlotValue,	ET_WARNING,
    "%O: Freed object in slot %N: %s" },
  { NAME_freedCellValue,	ET_WARNING,
    "%O: Freed object in cell %d: %s" },
  { NAME_freedElementValue,	ET_WARNING,
    "%O: Freed object in element %d: %s" },
  { NAME_freedKeyValue,		ET_WARNING,
    "%O: Freed key in %s --> %s" },
  { NAME_freedValueValue,	ET_WARNING,
    "%O: Freed value in %s --> %s" },
  { NAME_tooFewBuckets,		ET_WARNING,
    "%O: %d elements in only %d buckets?" },

					/* Classes */
  { NAME_redecaredReference,	0,
    "%N: Redeclared object reference" },
  { NAME_cannotChangeSuperClass,0,
    "%N: Cannot change super-class" },
  { NAME_notClassType,		0,
    "%N: Is not of <-kind class" },
  { NAME_cannotRefineVariable,	0,
    "%N: Cannot refine variable %s" },
  { NAME_hasInstances,		0,
    "%N: Class already has instances" },
  { NAME_hasSubClasses,		0,
    "%N: Class already has subclasses" },
  { NAME_noVariable,		0,
    "%O: Unknown variable: %s" },
  { NAME_classHasVariable,	0,
    "%N: Class already defines variable %s" },
					/* Errors */
  { NAME_unknownError,		0,
    "%N: Unknown error: %s" },
					/* Host */
  { NAME_startOfBuffer,		ET_WARNING,
    "%N: Start of buffer" },
  { NAME_endOfBuffer,		ET_WARNING,
    "%N: End of buffer" },
					/* Host */
  { NAME_noCallBack,		0,
    "%N: Host does not support call-back" },
					/* Miscellaneous */
  { NAME_readOnly,		0,
    "%N: Read only" },
  { NAME_stackEmpty,		0,
    "%N: Stack empty: %s" },
  { NAME_notPart,		0,
    "%N: %s is not a part" },
  { NAME_unknownEscape,		0,
    "%N: Unknown escape sequence: %s%c" },
  { NAME_notImplemented,	0,
    "%N: Not implemented: %s" },
  { NAME_alreadyPartOf,		0,
    "%N: %s is already part of %s" },
  { NAME_tooManyArguments,	0,
    "%N: Too many arguments" },
  { NAME_nameAlreadyExists,	0,
    "%N: Name already exists" },
  { NAME_cannotConstraintSelf,	0,
    "%N: Cannot contraint object to itself" },
  { NAME_syntaxError,		EF_REPORT,
    "%N: Syntax error: %s" },
  { NAME_sourceError,		EF_REPORT,
    "%I%N:%d: %s" },
  { NAME_internalError,		0,
    "%N: Internal error" },
  { NAME_needImageAndHotSpot,	0,
    "%N: Style image needs <-image and <-hot_spot" },
  { NAME_noFetchFunction,	0,
    "%N: text %s does not return <-fetch_function" },
  { NAME_noChangeAfterOpen,	0,
    "%O: Cannot change after ->open" },
  { NAME_notOpen,		0,
    "%O: Not opened" },
  { NAME_noButtonEvent,		0,
    "%O: Is not a button-related event" },
  { NAME_signal,		ET_FATAL,
    "%O: Signal trapped: %s" },
  { NAME_createFailed,		0,
    "%O: Failed to ->create" },
  { NAME_noCharacter,		0,
    "%O: No character and @event is not printable" },
  { NAME_noKeyBinding,		0,
    "%O: No key_binding named %s" },
  { NAME_noArgument,		ET_WARNING|EF_REPORT,
    "%N: Cannot construct %d-th argument for %N" },
  { NAME_noRegexRegister,	ET_WARNING|EF_REPORT,
    "%N: No register \\%d" },
  { NAME_noPrintName,		0,
    "%O: Cannot generate printable name" },
  { NAME_failedToClone,		0,
    "%O: Failed to <-clone" },
  { NAME_intRange,		0,
    "%O: Integer value out of range" },
  { NAME_noMember,		0,
    "%O: No member %O" },
  { NAME_notSupportedForChar16, 0,
    "%O: operation not supported on 16-bit strings" },
  { NAME_formatBufferOverFlow,  ET_FATAL,
    "%O: format buffer overflow (size = %d)" },
  { NAME_runtimeVersion,	0,
    "%N: operation not supported in runtime system"
  },

#ifdef __WINDOWS__
					/* MS-Windows errors */
  { NAME_failedToLoadDll,	EF_REPORT,
    "%N: Failed to load DLL %s: (error %d)" },
  { NAME_moreThanOneIcon,	0,
    "%N: Contains more than 1 icon.  Using first" },
#endif /*__WINDOWS__*/
					/* List closer */
  { NULL,			0,
    NULL }
};


static void
initErrorDatabase(HashTable db)
{ ErrorDef err = errors;

  for(; err->id; err++)
  { Name feedback = NIL, kind = NIL;
    Error e;

    switch(err->flags & ET_MASK)
    { case ET_ERROR:	kind = NAME_error;	break;
      case ET_WARNING:  kind = NAME_warning;	break;
      case ET_STATUS:	kind = NAME_status;	break;
      case ET_INFORM:	kind = NAME_inform;	break;
      case ET_FATAL:	kind = NAME_fatal;	break;
      case ET_IGNORED:	kind = NAME_ignored;	break;
    }

#ifndef O_RUNTIME
    switch(err->flags & EF_MASK)
    { case EF_REPORT:	feedback = NAME_report;	break;
      case EF_PRINT:	feedback = NAME_print;	break;
    }
#else
    feedback = NAME_report;
#endif /*O_RUNTIME*/

    e = newObject(ClassError, err->id, CtoString(err->format),
		  kind, feedback, 0);
  }
}


		/********************************
		*          C-INTERFACE		*
		********************************/

static void
_errorPce(Any obj, Name id, va_list args)
{ Error e;

  if ( id == NAME_stackOverflow )
    MaxGoalDepth += 100;

  if ( (e = getConvertError(ClassError, id)) )
  { int argc, i;
    Any argv[VA_PCE_MAX_ARGS+1];

    if ( e->kind == NAME_ignored )
      return;

    argv[0] = e;
    if ( !writef_arguments(strName(e->format) + 2, /* skip '%N: ' */
			   args, &argc, &argv[1]) )
      argc = 0;
    argc++;				/* e, arg-1, arg-2, ... */

    for(i=0; i<argc; i++)
      if ( !validPceDatum(argv[i]) )
	argv[i] = CtoName("<Bad argument>");

    if ( inBoot )
    { if ( CurrentGoal )
	setGFlag(CurrentGoal, G_EXCEPTION);

      printf("[PCE BOOT ERROR: ");
      writef(strName(e->format), argc-1, argv+1)	;
      printf("\n\tin: ");
      pceWriteErrorGoal();
      printf("]\n");
      printf("Dumping C-stack ...\n");
      pcePrintStack(20);
      hostAction(HOST_RECOVER_FROM_FATAL_ERROR);
      hostAction(HOST_HALT);
      exit(1);
    } else
    { Mode(MODE_SYSTEM, sendv(obj, isFunction(obj) ? NAME_Error : NAME_error,
			      argc, argv));
      if ( e->kind == NAME_fatal )
      {
#ifndef O_RUNTIME
	if ( id != NAME_noXServer )	/* little hack ... */
	{ traceBackPce(toInt(20), NAME_always);
	  if ( PCE->print_c_stack == ON )
	    pcePrintStack(20);
	}
#endif
	hostAction(HOST_RECOVER_FROM_FATAL_ERROR);
	hostAction(HOST_HALT);
	exit(1);
      }
    }
  } else				/* undefined error */
  { if ( CurrentGoal )
      setGFlag(CurrentGoal, G_EXCEPTION);

    if ( inBoot )
      sysPce("Unknown error at boot: %s", strName(id));
    else
      errorPce(obj, NAME_unknownError, id);
  }
}


status
errorPce(Any obj, Name id, ...)
{ va_list args;

  va_start(args, id);
  _errorPce(obj, id, args);
  va_end(args);

  fail;
}

