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

#define MAX_ARGS 100

status
typedKeyBinding(KeyBinding kb, EventId id, Graphical receiver)
{ Name key;
  Any cmd;
  Any crec = getReceiverKeyBinding(kb);

  if ( notDefault(receiver) )
  { if ( receiver != crec )
      resetKeyBinding(kb, receiver);
  } else if ( crec )
  { receiver = crec;
  } else
    errorPce(kb, NAME_noReceiver);

  key = getAppendName(kb->prefix, characterName(id));
  DEBUG(NAME_keyBinding, writef("Key = %s\n", key));

  if ( (cmd = getFunctionKeyBinding(kb, key)) )
  { Any argv[MAX_ARGS];
    int argc = 0;
    status rval = FAIL;
    int reset = 0;

    if ( isName(cmd) )
    { if ( cmd == NAME_prefix )		/* Prefix (multikey)  */
	assign(kb, prefix, key);
					/* Keyboard quit */
      else if ( cmd == NAME_keyboardQuit )
	resetKeyBinding(kb, receiver);
					/* Next/Previous line column saving */
      else if ( cmd == NAME_nextLine || cmd == NAME_previousLine )
      { if ( isNil(kb->saved_column) &&
	     hasGetMethodObject(receiver, NAME_column) )
	  assign(kb, saved_column, get(receiver, NAME_column, 0));

	if ( argc == 0 )
	  argv[argc++] = kb->argument;

	argv[argc++] = kb->saved_column;
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
      } else if ( cmd == NAME_universalArgument )
      { if ( isDefault(kb->argument) )
	{ assign(kb, argument, toInt(4));
	  assign(kb, status, NAME_universalArgument);
	} else
	  assign(kb, argument, toInt(valInt(kb->argument) * 4));
      } else if ( notDefault(kb->argument) && isdigit(valInt(id)) )
      { if ( kb->status == NAME_universalArgument )
	{ assign(kb, argument, toInt(valInt(id) - '0'));
	  assign(kb, status, NIL);
	} else
	  assign(kb, argument, toInt(valInt(kb->argument) * 10 +
				     valInt(id) - '0'));
	cmd = NAME_universalArgument;
      } else
	reset |= (RESET_ARGUMENT|RESET_COLUMN);

      { Any impl;

	if ( (impl=resolveSendMethodObject(receiver, NULL, cmd, NULL, NULL)) )
	{ Type type;
	  Any val;

	  while( (type = get(impl, NAME_argumentType, toInt(argc+1), 0)) &&
		 argc < MAX_ARGS )
	  { if ( includesType(type, toType(NAME_eventId)) )
	      argv[argc++] = id;
	    else if ( includesType(type, toType(NAME_char)) && isInteger(id))
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
		  goto out;
		}
	      } else
		goto out;
	    } else
	    { errorPce(kb, NAME_noArgument, toInt(argc+1), impl);
	      goto out;
	    }
	  }

	  rval = sendv(receiver, cmd, argc, argv);
	} else
	{ if ( cmd != NAME_digitArgument &&
	       cmd != NAME_universalArgument &&
	       cmd != NAME_keyboardQuit &&
	       cmd != NAME_prefix )
	    errorPce(receiver, NAME_noTextBehaviour, cmd);
	}
      }
    } else if ( instanceOfObject(cmd, ClassCode) )
    { rval = forwardReceiverCode(cmd, receiver, kb->argument, id, 0);
    }

out:
    if ( reset & RESET_COLUMN )
      assign(kb, saved_column, NIL);
    if ( reset & RESET_ARGUMENT )
      assign(kb, argument, DEFAULT);

    if ( cmd != NAME_prefix )
      assign(kb, prefix, CtoName(""));

    return rval;
  }

  fail;
}


status
makeClassKeyBinding(Class class)
{ sourceClass(class, makeClassKeyBinding, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name*", NAME_get,
	     "Name of this binding-table");
  localClass(class, NAME_bindings, NAME_binding, "sheet", NAME_get,
	     "Sheet mappings keys to functions");
  localClass(class, NAME_defaults, NAME_default, "chain", NAME_both,
	     "Chain with key_bindings to inherit from");
  localClass(class, NAME_defaultFunction, NAME_binding, "name|code*", NAME_both,
	     "Default function to perform");
  localClass(class, NAME_prefix, NAME_event, "name", NAME_get,
	     "Currently parsed prefix");
  localClass(class, NAME_argument, NAME_argument, "[int]", NAME_get,
	     "Universal (numerical) argument");
  localClass(class, NAME_status, NAME_event, "{universal_argument}*", NAME_none,
	     "Internal flag");
  localClass(class, NAME_savedColumn, NAME_caret, "int*", NAME_get,
	     "Saved {next_line,previous_line} column");
  localClass(class, NAME_condition, NAME_event, "code*", NAME_both,
	     "Activation condition");

  termClass(class, "key_binding", 1, NAME_name);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "name=[name]*", "super=key_binding ...",
	     "Create named binding-table with defaults",
	     initialiseKeyBinding);
  sendMethod(class, NAME_reset, NAME_abort, 1, "[graphical]*",
	     "Reset <-receiver and <-prefix",
	     resetKeyBinding);
  sendMethod(class, NAME_function, NAME_binding, 2,
	     "key=name|event_id", "action=name|code",
	     "Append function association",
	     functionKeyBinding);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Process keyboard event",
	     eventKeyBinding);
  sendMethod(class, NAME_typed, NAME_event, 2,
	     "id=event_id", "for=[object]",
	     "Process event-id (of keyboard event)",
	     typedKeyBinding);
  sendMethod(class, NAME_receiver, NAME_client, 1, "graphical*",
	     "Client of the key_binding object",
	     receiverKeyBinding);

  getMethod(class, NAME_convert, NAME_conversion, "key_binding", 1, "name",
	    "Lookup existing table or create new named table",
	    getConvertKeyBinding);
  getMethod(class, NAME_lookup, NAME_oms, "key_binding", 2,
	    "name", "key_binding ...",
	    "Lookup existing table",
	    getLookupKeyBinding);
  getMethod(class, NAME_function, NAME_meta, "name|code", 1, "name|event_id",
	    "Get function for given event-id",
	    getFunctionKeyBinding);
  getMethod(class, NAME_binding, NAME_meta, "name", 1, "function=name|code",
	    "Find key-binding from function",
	    getBindingKeyBinding);
  getMethod(class, NAME_receiver, NAME_client, "graphical", 0,
	    "Client of the key_binding object",
	    getReceiverKeyBinding);

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
  { "DEL",		NAME_backwardDeleteChar },

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
  { "\\C-h", 		NAME_backwardDeleteChar },
  { "\\C-t", 		NAME_transposeChars },
  { "\\C-k", 		NAME_killLine },
  { "\\C-o", 		NAME_openLine },

  { "\\ed",		NAME_killWord },
  { "\\eDEL",		NAME_backwardKillWord },

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

  { "\\C-g",		NAME_keyboardQuit },
  { "RET",		NAME_enter },
  { "LFD",		NAME_enter },
  { "DEL",		NAME_backwardDeleteChar },
  { "TAB",		NAME_extendPrefixOrNext },
  { "\\C-w",		NAME_extendToCurrent },
  { "\\C-s",		NAME_repeatSearch },

  { DEFAULT_FUNCTION,	NAME_alert },

  { NULL,		NULL }
};

static kbDef emacs_page[] =
{ { "\\C-v",		NAME_scrollUp },
  { "\\ev",		NAME_scrollDown },
  { "\\C-l",		NAME_recenter },

  { NULL,		NULL }
};

static kbDef editor[] =
{ { SUPER,		NAME_emacsBasics },
  { SUPER,		NAME_emacsPage },

  { "\\C-@",		NAME_setMark },
  { "LFD", 		NAME_newlineAndIndent },
  { "\\C-q", 		NAME_quotedInsert },
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
  { "\\e>",		NAME_pointToBottomOfFile },
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

  succeed;
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
