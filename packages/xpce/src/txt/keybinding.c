/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>
#include <h/graphics.h>
#include <h/graphics.h>

NewClass(key_binding)
  ABSTRACT_RECOGNISER
  Name		name;			/* Global name of this table */
  Sheet		bindings;		/* Key-name --> function */
  Chain		defaults;		/* Default tables to be used */
  Any		default_function;	/* Function if not defined */
  Name 		prefix;			/* Parsed prefix */
  Int		argument;		/* Universal argument */
  Name		status;			/* {universal_argument}* */
  Int		saved_column;		/* {next,previous}_line column */
  Code		condition;		/* General condition for activation */
End;

static HashTable BindingTable;		/* name --> table: @key_bindings */

static status	resetKeyBinding(KeyBinding kb, Any receiver);
static status	initPredefinedKeyBinding(KeyBinding kb);

static status
initialiseKeyBinding(KeyBinding kb, Name name, int argc, KeyBinding *argv)
{ initialiseRecogniser((Recogniser) kb);

  assign(kb, bindings, newObjectv(ClassSheet, 0, NULL));
  assign(kb, defaults, newObjectv(ClassChain, 0, NULL));

  resetKeyBinding(kb, NIL);

  if ( notDefault(name) )
  { assign(kb, name, name);
    appendHashTable(BindingTable, name, kb);
    protectObject(kb);
  }

  for( ; argc > 0; argc--, argv++ )
    appendChain(kb->defaults, argv[0]);

  if ( notDefault(name) && argc == 0 )
    initPredefinedKeyBinding(kb);
    
  succeed;
}


static KeyBinding
getLookupKeyBinding(Any ctx, Name name, int argc, KeyBinding *argv)
{ KeyBinding kb;

  if ( (kb = getMemberHashTable(BindingTable, name)) )
  { TRY(initialiseKeyBinding(kb, name, argc, argv));

    answer(kb);
  }

  fail;
}


static KeyBinding
getConvertKeyBinding(Any ctx, Name name)
{ KeyBinding kb;

  if ( (kb = getMemberHashTable(BindingTable, name)) )
    answer(kb);

  answer(newObject(ClassKeyBinding, name, 0));
}


static status
receiverKeyBinding(KeyBinding kb, Any receiver)
{ if ( getHyperedObject(kb, NAME_receiver, DEFAULT) != receiver )
    freeHypersObject(kb, NAME_receiver, DEFAULT);
  
  if ( notNil(receiver) )
    newObject(ClassHyper, kb, receiver, NAME_receiver, NAME_keyBinding, 0);
  
  succeed;
}


static Any
getReceiverKeyBinding(KeyBinding kb)
{ return getHyperedObject(kb, NAME_receiver, DEFAULT);
}


static status
resetKeyBinding(KeyBinding kb, Any receiver)
{ if ( isDefault(receiver) )
    receiver = NIL;

  receiverKeyBinding(kb, receiver);
  assign(kb, prefix,   CtoName(""));
  assign(kb, argument, DEFAULT);

  succeed;
}


static Any
get_function_key_binding(KeyBinding kb, Name key)
{ Any cmd;
  Cell cell;

  if ( (cmd = getValueSheet(kb->bindings, key)) )
    answer(cmd);
  for_cell(cell, kb->defaults)
    if ( (cmd = get_function_key_binding(cell->value, key)) )
      answer(cmd);

  fail;
}


static Any
get_default_function_key_binding(KeyBinding kb, Name key)
{ Any cmd;
  Cell cell;

  if ( notNil(kb->default_function) )
    answer(kb->default_function);
  for_cell(cell, kb->defaults)
    if ( (cmd = get_default_function_key_binding(cell->value, key)) )
      answer(cmd);

  fail;
}


Any
getFunctionKeyBinding(KeyBinding kb, Name key)
{ Any cmd;

  if ( isInteger(key) )
    key = characterName(key);

  if ( (cmd = get_function_key_binding(kb, key)) )
    answer(cmd);

  answer(get_default_function_key_binding(kb, key));
}


static Name
getBindingKeyBinding(KeyBinding kb, Any function)
{ Cell cell;
  Name binding;

  for_cell(cell, kb->bindings->attributes)
  { Attribute a = cell->value;
    if ( a->value == function )
      answer(a->name);
  }
  for_cell(cell, kb->defaults)
  { if ( (binding = getBindingKeyBinding(cell->value, function)) )
      answer(binding);
  }

  fail;
}


status
functionKeyBinding(KeyBinding kb, EventId id, Any f)
{ return valueSheet(kb->bindings, id, f);
}


		/********************************
		*       EVENT PROCESSING	*
		********************************/

static status
eventKeyBinding(KeyBinding kb, EventObj ev)
{ if ( isAEvent(ev, NAME_keyboard) &&
       (isNil(kb->condition) ||
	forwardReceiverCode(kb->condition, kb, ev, 0)) )
    return send(kb, NAME_typed, getIdEvent(ev), ev->receiver, 0);

  fail;
}


#define RESET_COLUMN   1
#define RESET_ARGUMENT 2
#define RESET_STATUS   4

#define MAX_ARGS 16

status
typedKeyBinding(KeyBinding kb, EventId id, Graphical receiver)
{ Name key;
  Any cmd;
  Any crec = getReceiverKeyBinding(kb);
  Any argv[MAX_ARGS];
  int argc = 0;
  int reset = 0;
    
  if ( notDefault(receiver) )
  { if ( receiver != crec )
      resetKeyBinding(kb, receiver);
  } else if ( crec )
  { receiver = crec;
  } else
    errorPce(kb, NAME_noReceiver);

  key = getAppendName(kb->prefix, characterName(id));
  DEBUG(NAME_keyBinding, writef("Key = %s\n", key));
    
  if ( kb->status == NAME_quotedInsert )
  { cmd = NAME_insertQuoted;
    reset |= RESET_STATUS;
  } else
  { cmd = get(kb, NAME_function, key, 0);
  }

  if ( cmd )
  { status rval = FAIL;

    if ( isName(cmd) )
    { int cmdi;

      argv[argc++]        = id;
      argv[argc++]        = receiver;
      argv[(cmdi=argc++)] = cmd;

      if ( cmd == NAME_prefix )		/* Prefix (multikey)  */
      { assign(kb, prefix, key);
	rval = SUCCEED;
      } else if ( cmd == NAME_keyboardQuit )
      { resetKeyBinding(kb, receiver);
	rval = SUCCEED;
      } else if ( cmd == NAME_nextLine || cmd == NAME_previousLine )
      { Method impl = resolveSendMethodObject(receiver, NULL, cmd, NULL, NULL);
	Type argt;

	argv[argc++] = kb->argument;

	if ( impl &&
	     instanceOfObject(impl, ClassSendMethod) &&
	     (argt = getArgumentTypeMethod(impl, toInt(2))) &&
	     includesType(argt, TypeInt) )
	{ if ( isNil(kb->saved_column) )
	    assign(kb, saved_column, get(receiver, NAME_column, 0));
	  argv[argc++] = kb->saved_column;
	}

	reset |= RESET_ARGUMENT;
					/* Universal argument specification */
      } else if ( cmd == NAME_digitArgument && isInteger(id) )
      { if ( valInt(id) == Meta('-') && isDefault(kb->argument) )
	  assign(kb, argument, toInt(-1));
	else
	{ int chr = valInt(id) - Meta('0');

	  if ( chr >= 0 && chr <= 9 )
	  { if ( isDefault(kb->argument) )
	      assign(kb, argument, toInt(chr));
	    else
	      assign(kb, argument, toInt(valInt(kb->argument) * 10 + chr));
	  }
	}
	rval = SUCCEED;
      } else if ( cmd == NAME_universalArgument )
      { if ( isDefault(kb->argument) )
	{ assign(kb, argument, toInt(4));
	  assign(kb, status, NAME_universalArgument);
	} else
	  assign(kb, argument, toInt(valInt(kb->argument) * 4));
	rval = SUCCEED;
      } else if ( notDefault(kb->argument) && isdigit(valInt(id)) )
      { if ( kb->status == NAME_universalArgument )
	{ assign(kb, argument, toInt(valInt(id) - '0'));
	  assign(kb, status, NIL);
	} else
	  assign(kb, argument, toInt(valInt(kb->argument) * 10 +
				     valInt(id) - '0'));
	cmd = NAME_universalArgument;
	argv[cmdi] = cmd;
	rval = SUCCEED;
      } else if ( cmd == NAME_quotedInsert )
      { assign(kb, status, NAME_quotedInsert );
      } else 
      { reset |= (RESET_ARGUMENT|RESET_COLUMN);
      }

      rval = sendv(kb, NAME_fillArgumentsAndExecute, argc, argv);
    } else if ( instanceOfObject(cmd, ClassCode) )
    { rval = forwardReceiverCode(cmd, receiver, kb->argument, id, 0);
    }

    if ( reset & RESET_COLUMN )
      assign(kb, saved_column, NIL);
    if ( reset & RESET_ARGUMENT )
      assign(kb, argument, DEFAULT);
    if ( reset & RESET_STATUS )
      assign(kb, status, NIL);

    if ( cmd != NAME_prefix )
      assign(kb, prefix, NAME_);

    return rval;
  }

  fail;
}


static status
fillArgumentsAndExecuteKeyBinding(KeyBinding kb,
				  EventId id, Any receiver, Name cmd,
				  int ac, Any av[])
{ Any impl;

  impl = resolveSendMethodObject(receiver, NULL, cmd, NULL, NULL);
  if ( !impl && cmd == NAME_insertQuoted )
  { cmd = NAME_insertSelf;
    impl = resolveSendMethodObject(receiver, NULL, cmd, NULL, NULL);
  }

  DEBUG(NAME_keyBinding, Cprintf("%s: impl of %s is %s\n",
				 pp(kb), pp(cmd), pp(impl)));

  if ( impl )
  { Type type;
    Any val;
    int argc=0; Any theargv[MAX_ARGS];
    Any *argv = &theargv[2];

    theargv[0] = receiver;
    theargv[1] = cmd;
    for(; argc<ac; argc++)
    { argv[argc] = av[argc];
    }

    while( (type = get(impl, NAME_argumentType, toInt(argc+1), 0)) &&
	   argc < MAX_ARGS )
    { if ( includesType(type, toType(NAME_eventId)) ||
	   (includesType(type, toType(NAME_char)) && isInteger(id)) )
	argv[argc++] = id;
      else if ( includesType(type, toType(NAME_int)) &&
		notDefault(kb->argument))
	argv[argc++] = kb->argument;
      else if ( includesType(type, toType(NAME_default)) )
	argv[argc++] = DEFAULT;
      else if ( hasGetMethodObject(receiver, NAME_interactiveArgument) )
      { if ( (val = get(receiver, NAME_interactiveArgument,
			impl, toInt(argc+1), 0)) )
	{ if ( (val = checkType(val, type, receiver)) )
	    argv[argc++] = val;
	  else
	  { errorPce(kb, NAME_noArgument, toInt(argc+1), impl);
	    fail;
	  }
	} else
	  fail;
      } else
      { errorPce(kb, NAME_noArgument, toInt(argc+1), impl);
	fail;
      }
    }

    return sendv(kb, NAME_execute, argc+2, theargv);
  } else
  { if ( cmd != NAME_digitArgument &&
	 cmd != NAME_universalArgument &&
	 cmd != NAME_keyboardQuit &&
	 cmd != NAME_quotedInsert &&
	 cmd != NAME_prefix )
      errorPce(receiver, NAME_noTextBehaviour, cmd);
    else
      succeed;
  }

  fail;
}


static status
executeKeyBinding(KeyBinding kb, Any receiver, Name cmd, int argc, Any argv[])
{ return sendv(receiver, cmd, argc, argv);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_typed[] =
        { "id=event_id", "for=[object]" };
static char *T_fillArgumentsAndExecute[] =
        { "id=event_id", "receiver=object", "selector=name", "arguments=any ..." };
static char *T_function[] =
        { "key=name|event_id", "action=name|code" };
static char *T_initialise[] =
        { "name=[name]*", "super=key_binding ..." };
static char *T_lookup[] =
        { "name", "key_binding ..." };
static char *T_execute[] =
        { "receiver=object", "selector=name", "arguments=any ..." };

/* Instance Variables */

static vardecl var_keyBinding[] =
{ IV(NAME_name, "name*", IV_GET,
     NAME_name, "Name of this binding-table"),
  IV(NAME_bindings, "sheet", IV_GET,
     NAME_binding, "Sheet mappings keys to functions"),
  IV(NAME_defaults, "chain", IV_BOTH,
     NAME_default, "Chain with key_bindings to inherit from"),
  IV(NAME_defaultFunction, "name|code*", IV_BOTH,
     NAME_binding, "Default function to perform"),
  IV(NAME_prefix, "name", IV_GET,
     NAME_event, "Currently parsed prefix"),
  IV(NAME_argument, "[int]", IV_GET,
     NAME_argument, "Universal (numerical) argument"),
  IV(NAME_status, "{universal_argument,quoted_insert}*", IV_NONE,
     NAME_event, "Internal flag"),
  IV(NAME_savedColumn, "int*", IV_GET,
     NAME_caret, "Saved {next_line,previous_line} column"),
  IV(NAME_condition, "code*", IV_BOTH,
     NAME_event, "Activation condition")
};

/* Send Methods */

static senddecl send_keyBinding[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseKeyBinding,
     DEFAULT, "Create named binding-table with defaults"),
  SM(NAME_reset, 1, "[graphical]*", resetKeyBinding,
     NAME_abort, "Reset <-receiver and <-prefix"),
  SM(NAME_function, 2, T_function, functionKeyBinding,
     NAME_binding, "Append function association"),
  SM(NAME_receiver, 1, "graphical*", receiverKeyBinding,
     NAME_client, "Client of the key_binding object"),
  SM(NAME_event, 1, "event", eventKeyBinding,
     NAME_event, "Process keyboard event"),
  SM(NAME_execute, 3, T_execute, executeKeyBinding,
     NAME_event, "Invoke `selector' on `receiver' with args"),
  SM(NAME_fillArgumentsAndExecute, 4, T_fillArgumentsAndExecute, fillArgumentsAndExecuteKeyBinding,
     NAME_event, "Collect additional arguments and ->execute"),
  SM(NAME_typed, 2, T_typed, typedKeyBinding,
     NAME_event, "Process event-id (of keyboard event)")
};

/* Get Methods */

static getdecl get_keyBinding[] =
{ GM(NAME_receiver, 0, "graphical", NULL, getReceiverKeyBinding,
     NAME_client, "Client of the key_binding object"),
  GM(NAME_convert, 1, "key_binding", "name", getConvertKeyBinding,
     NAME_conversion, "Lookup existing table or create new named table"),
  GM(NAME_binding, 1, "name", "function=name|code", getBindingKeyBinding,
     NAME_meta, "Find key-binding from function"),
  GM(NAME_function, 1, "name|code", "name|event_id", getFunctionKeyBinding,
     NAME_meta, "Get function for given event-id"),
  GM(NAME_lookup, 2, "key_binding", T_lookup, getLookupKeyBinding,
     NAME_oms, "Lookup existing table")
};

/* Resources */

static resourcedecl rc_keyBinding[] =
{ RC(NAME_insert,           "chain", "[]", "Bind printable to ->insert_self"),
  RC(NAME_argument,         "chain", "[]", "C-u and M-digit binding"),
  RC(NAME_emacsSpecial,     "chain", "[]", "argument, prefix, quote and quit"),
  RC(NAME_emacsCaretBasics, "chain", "[]", "Basic caret movement"),
  RC(NAME_emacsPage,        "chain", "[]", "Moving by pages"),
  RC(NAME_emacsEditBasics,  "chain", "[]", "Basic emacs editing commands"),
  RC(NAME_emacsViewBasics,  "chain", "[]", "Basic emacs browse commands"),
  RC(NAME_emacsBasics,      "chain", "[]", "All emacs basic commands"),
  RC(NAME_editor,           "chain", "[]", "Default editor binding"),
  RC(NAME_text,             "chain", "[]", "Default text binding"),
  RC(NAME_textItem,         "chain", "[]", "Default text_item binding"),
  RC(NAME_textItemView,     "chain", "[]", "Default non-editible text_item"),
  RC(NAME_listBrowser,      "chain", "[]", "Default list_browser binding")
};

/* Class Declaration */

static Name keyBinding_termnames[] = { NAME_name };

ClassDecl(keyBinding_decls,
          var_keyBinding, send_keyBinding, get_keyBinding, rc_keyBinding,
          1, keyBinding_termnames,
          "$Rev$");



status
makeClassKeyBinding(Class class)
{ declareClass(class, &keyBinding_decls);

  BindingTable = globalObject(NAME_keyBindings, ClassHashTable, 0);

  succeed;
}


		/********************************
		*           TABLES		*
		********************************/

static status
initInsertKeyBinding(KeyBinding kb)
{ int i;

  functionKeyBinding(kb, characterName(toInt('\t')), NAME_insertSelf);
  functionKeyBinding(kb, characterName(toInt('\n')), NAME_newline);
  functionKeyBinding(kb, characterName(toInt('\r')), NAME_newline);

  for(i = ' '; i < DEL; i++ )
    functionKeyBinding(kb, characterName(toInt(i)), NAME_insertSelf);
  for(i = 128; i < 256; i++ )
    functionKeyBinding(kb, characterName(toInt(i)), NAME_insertSelf);

  succeed;
}
  

static status
initArgumentKeyBinding(KeyBinding kb)
{ int i;

  for(i = Meta('0'); i <= Meta('9'); i++ )
    functionKeyBinding(kb, characterName(toInt(i)), NAME_digitArgument);

  functionKeyBinding(kb, characterName(toInt(Meta('-'))),
		     NAME_digitArgument);
  functionKeyBinding(kb, characterName(toInt(Control('u'))),
		     NAME_universalArgument);

  succeed;
}


typedef struct
{ char *key;
  Name function;
} kbDef, *KbDef;

#define SUPER ((char *) -1)
#define DEFAULT_FUNCTION ((char *) -2)

static kbDef emacs_special[] =
{ { SUPER,		NAME_argument },

  { "\\C-c",		NAME_prefix },
  { "\\C-g",		NAME_keyboardQuit },
  { "\\C-q",		NAME_quotedInsert },
  { "\\C-x",		NAME_prefix },
  { "\\e",		NAME_prefix },

  { NULL,		NULL }
};

static kbDef emacs_caret_basics[] =
{ { "\\C-a", 		NAME_beginningOfLine },
  { "\\C-b", 		NAME_backwardChar },
  { "\\C-e", 		NAME_endOfLine },
  { "\\C-f", 		NAME_forwardChar },
  { "\\C-n", 		NAME_nextLine },
  { "\\C-p", 		NAME_previousLine },

  { "\\eb",		NAME_backwardWord },
  { "\\ef",		NAME_forwardWord },

  { "cursor_up",	NAME_previousLine },
  { "cursor_down",	NAME_nextLine },
  { "cursor_right",	NAME_forwardChar },
  { "cursor_left",	NAME_backwardChar },

  { NULL,		NULL }
};


static kbDef emacs_edit_basics[] =
{ { SUPER,		NAME_insert },

  { "\\C-d", 		NAME_deleteChar },
  { "DEL",		NAME_deleteChar },
  { "\\C-h", 		NAME_backwardDeleteChar },
  { "\\C-t", 		NAME_transposeChars },
  { "\\C-k", 		NAME_killLine },
  { "\\C-o", 		NAME_openLine },

  { "\\ed",		NAME_killWord },
  { "backspace",	NAME_backwardDeleteChar },
  { "\\e\\C-h",		NAME_backwardKillWord },

  { NULL,		NULL }
};


static kbDef emacs_basics[] =
{ { SUPER,		NAME_emacsSpecial },
  { SUPER,		NAME_emacsCaretBasics },
  { SUPER,		NAME_emacsEditBasics },

  { NULL,		NULL }
};


static kbDef emacs_view_basics[] =
{ { SUPER,		NAME_emacsSpecial },
  { SUPER,		NAME_emacsCaretBasics },

  { NULL,		NULL }
};


static kbDef text[] =
{ { SUPER,		NAME_emacsBasics },

  { "\\C-u",		NAME_clear },
  { "\\C-c",		NAME_formatCenter },
  { "\\C-l",		NAME_formatLeft },
  { "\\C-r",		NAME_formatRight },

  { NULL,		NULL }
};


static kbDef text_item[] =
{ { SUPER,		NAME_emacsBasics },

  { "\\C-u",		NAME_clear },
  { "\\C-g",		NAME_keyboardQuit },
  { "RET",		NAME_enter },
  { "LFD",		NAME_enter },
  { "TAB",		NAME_next },
  { "SPC",		NAME_complete },

  { NULL,		NULL }
};


static kbDef text_item_view[] =
{ { SUPER,		NAME_emacsViewBasics },
  { "TAB",		NAME_next },

  { NULL,		NULL }
};


static kbDef list_browser[] =
{ { SUPER,		NAME_insert },
  { SUPER,		NAME_emacsPage },

  { "\\C-g",		NAME_keyboardQuit },
  { "\\e",		NAME_keyboardQuit },
  { "RET",		NAME_enter },
  { "LFD",		NAME_enter },
  { "DEL",		NAME_backwardDeleteChar },
  { "TAB",		NAME_extendPrefixOrNext },
  { "\\C-w",		NAME_extendToCurrent },
  { "\\C-s",		NAME_repeatSearch },
  { "\\C-n", 		NAME_nextLine },
  { "cursor_down",	NAME_nextLine },
  { "\\C-p", 		NAME_previousLine },
  { "cursor_up", 	NAME_previousLine },

/*{ DEFAULT_FUNCTION,	NAME_alert },*/

  { NULL,		NULL }
};

static kbDef emacs_page[] =
{ { "\\C-v",		NAME_scrollUp },
  { "\\ev",		NAME_scrollDown },
  { "\\C-l",		NAME_recenter },
  { "page_up",		NAME_scrollDown },
  { "page_down",	NAME_scrollUp },

  { NULL,		NULL }
};

static kbDef editor[] =
{ { SUPER,		NAME_emacsBasics },
  { SUPER,		NAME_emacsPage },

  { "\\C-@",		NAME_setMark },
  { "LFD", 		NAME_newlineAndIndent },
  { "\\C-r", 		NAME_isearchBackward },
  { "\\C-s", 		NAME_isearchForward },
  { "\\C-w",		NAME_killOrGrabRegion },
  { "\\C-y",		NAME_yank },
  { "\\C-_", 		NAME_undo },

  { "\\e\\C-b",		NAME_backwardTerm },
  { "\\e\\C-f",		NAME_forwardTerm },
  { "\\e\\C-k",		NAME_killTerm },
  { "\\e\\C-t",		NAME_transposeTerms },
  { "\\eSPC",		NAME_justOneSpace },
  { "\\ea",		NAME_backwardSentence },
  { "\\ec",		NAME_capitaliseWord },
  { "\\ed",		NAME_killWord },
  { "\\ee",		NAME_forwardSentence },
  { "\\eg",		NAME_fillRegion },
  { "\\ek",		NAME_killSentence },
  { "\\el",		NAME_downcaseWord },
  { "\\eq",		NAME_fillParagraph },
  { "\\et",		NAME_transposeWord },
  { "\\eu",		NAME_upcaseWord },
  { "\\e[",		NAME_backwardParagraph },
  { "\\e]",		NAME_forwardParagraph },
  { "\\e<",		NAME_pointToTopOfFile },
  { "cursor_home",	NAME_pointToTopOfFile },
  { "\\e>",		NAME_pointToBottomOfFile },
  { "end",		NAME_pointToBottomOfFile },
  { "\\e/",		NAME_dabbrevExpand },
  { "\\eDEL",		NAME_backwardKillWord },

  { "\\C-x\\C-x",	NAME_exchangePointAndMark },
  { "\\C-x\\C-o",	NAME_deleteBlankLines },
  { "\\C-x\\C-t",	NAME_transposeLines },
  { "\\C-x\\C-l",	NAME_downcaseRegion },
  { "\\C-x\\C-u",	NAME_upcaseRegion },
  { "\\C-x\\C-s",	NAME_saveBuffer },

  { DEFAULT_FUNCTION,	NAME_undefined },

  { NULL, 	        NULL }
};


static status
bindResourcesKeyBinding(KeyBinding kb)
{ Chain ch = getResourceValueObject(kb, kb->name);

  if ( instanceOfObject(ch, ClassChain) )
  { Cell cell;

    for_cell(cell, ch)
    { Binding b = cell->value;

      if ( instanceOfObject(b, ClassBinding) &&
	   isName(b->name) && isName(b->value) )
      { functionKeyBinding(kb, b->name, b->value);
      }
    }
  }

  succeed;
}


static status
initPredefinedKeyBinding(KeyBinding kb)
{ KbDef table;

  if ( kb->name == NAME_insert )
    return initInsertKeyBinding(kb);
  if ( kb->name == NAME_argument )
    return initArgumentKeyBinding(kb);

  if ( kb->name == NAME_emacsSpecial )
    table = emacs_special;
  else if ( kb->name == NAME_emacsCaretBasics )
    table = emacs_caret_basics;
  else if ( kb->name == NAME_emacsPage )
    table = emacs_page;
  else if ( kb->name == NAME_emacsEditBasics )
    table = emacs_edit_basics;
  else if ( kb->name == NAME_emacsBasics )
    table = emacs_basics;
  else if ( kb->name == NAME_emacsViewBasics )
    table = emacs_view_basics;
  else if ( kb->name == NAME_editor )
    table = editor;
  else if ( kb->name == NAME_text )
    table = text;
  else if ( kb->name == NAME_textItem )
    table = text_item;
  else if ( kb->name == NAME_textItemView )
    table = text_item_view;
  else if ( kb->name == NAME_listBrowser )
    table = list_browser;
  else
    succeed;

  for( ; table->key; table++ )
  { if ( table->key == SUPER )
    { KeyBinding kb2 = newObject(ClassKeyBinding, table->function, 0);

      if ( kb2 )
	appendChain(kb->defaults, kb2);
      else
	errorPce(kb, NAME_noKeyBinding, table->function);
    } else if ( table->key == DEFAULT_FUNCTION )
    { assign(kb, default_function, table->function);
    } else
      functionKeyBinding(kb,
			 table->key[0] == '\\' ? CtoName(table->key)
			 		       : CtoKeyword(table->key),
			 table->function);
  }

  return bindResourcesKeyBinding(kb);
}

		/********************************
		*         C-USED TABLES		*
		********************************/


KeyBinding
KeyBindingText(void)
{ static KeyBinding kb = NULL;

  if ( !kb )
    kb = globalObject(NIL, ClassKeyBinding, NAME_text, 0);

  return kb;
}


KeyBinding
KeyBindingTextItem(void)
{ static KeyBinding kb = NULL;

  if ( !kb )
    kb = globalObject(NIL, ClassKeyBinding, NAME_textItem, 0);

  return kb;
}


KeyBinding
KeyBindingTextItemView(void)
{ static KeyBinding kb = NULL;

  if ( !kb )
    kb = globalObject(NIL, ClassKeyBinding, NAME_textItemView, 0);

  return kb;
}
