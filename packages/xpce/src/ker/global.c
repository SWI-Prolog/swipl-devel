/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

struct global
{ Name		reference;		/* Name of the object */
  Name		classname;		/* Class of the object */
} globals[] =
{ { NAME_display,		NAME_display },
  { NAME_displayManager,	NAME_displayManager },
  { NAME_ModifierShift,		NAME_modifier },
  { NAME_ModifierAllUp,		NAME_modifier },
  { NAME_modifiers, 		NAME_modifier },
  { NAME_PopupGesture, 		NAME_popupGesture },
  { NAME_defaultLink, 		NAME_link },
  { NAME_changedWindows, 	NAME_window },
  { NAME_DocumentFonts,		NAME_font },
  { NAME_scrollBarRepeatTimer,  NAME_scrollBar},
  { NAME_c, 			NAME_c},
  { NAME_host,			NAME_host },
  { NAME_ButtonGesture,		NAME_clickGesture },
  { NAME_PopupWindows,		NAME_popup },
  { NAME_completer,		NAME_textItem },
  { NAME_variables,		NAME_var },
  { NAME_errors,		NAME_error },
  { NAME_traceConditions,	NAME_programObject },
  { NAME_breakConditions,	NAME_programObject },
  { NAME_DebugSubjects,		NAME_pce },
  { NAME_objectConstraintTable, NAME_object },
  { NAME_objectAttributeTable,  NAME_object },
  { NAME_objectSendMethodTable, NAME_object },
  { NAME_objectGetMethodTable,  NAME_object },
  { NAME_objectRecogniserTable, NAME_object },
  { NAME_objectHyperTable,	NAME_object },
  { NAME_vmiSend,		NAME_vmi },
  { NAME_vmiGet,		NAME_vmi },
  { NAME_vmiNew,		NAME_vmi },
  { NAME_vmiFree,		NAME_vmi },
  { NAME_electricTimer,		NAME_editor },
  { NAME_textKillRing,		NAME_editor },
  { NAME_keyBindings,		NAME_keyBinding },
  { NAME_syntaxTables,		NAME_syntaxTable },
  { NAME_directoryStack,	NAME_directory },
  { NAME_compressionFilters,	NAME_file },
  { NAME_runningProcesses,	NAME_process },
  { NAME_openSockets,		NAME_socket },
  { NAME_colours,		NAME_colour },
  { NAME_colourMaps,		NAME_colourMap },
  { NAME_cursors,		NAME_cursor },
  { NAME_cursorNames,		NAME_cursor },
  { NAME_eventTree,		NAME_event },
  { NAME_fonts,			NAME_font },
  { NAME_images,		NAME_image },
  { NAME_GrabbedWindows,	NAME_window },
  { NAME_whiteImage,		NAME_image },
  { NAME_grey12Image,		NAME_image },
  { NAME_grey25Image,		NAME_image },
  { NAME_grey50Image,		NAME_image },
  { NAME_grey75Image,		NAME_image },
  { NAME_blackImage,		NAME_image },
  { NAME_cycleImage,		NAME_image },
  { NAME_markImage,		NAME_image },
  { NAME_nomarkImage,		NAME_image },
  { NAME_pullRightImage,	NAME_image },
  { NAME_markHandleImage,	NAME_image },
  { NAME_defaultSyntaxTable,	NAME_syntaxTable },
  { NAME_resourceParser,	NAME_resource },
  { NAME_notObtained,		NAME_resource },

  { NULL,	  		NULL}
};

static HashTable GlobalTable;		/* name --> class-name */

void
initGlobals()
{ struct global *g = globals;

  GlobalTable = createHashTable(toInt(32), OFF);
  for(; g->reference; g++)
    appendHashTable(GlobalTable, g->reference, g->classname);
}


static status
isFontReference(Name name)
{ int i1, i2;
  char sep = syntax.word_separator;
  String s = &name->data;

  if ( (i1=str_index(s, sep)) >= 0 &&
       i1 != (i2=str_rindex(s, sep)) &&
       isdigit(str_fetch(s, i2+1)) )
    succeed;

  fail;
}


Any
findGlobal(Name name)
{ Any obj;
  Name classname;
  Class class;

  if ( (obj = getObjectAssoc(name)) )
    answer(obj);

  if ( (classname = getMemberHashTable(GlobalTable, name)) &&
       (class = getConvertClass(ClassClass, classname)) &&
       realiseClass(class) &&
       (obj = getObjectAssoc(name)) )
    answer(obj);

  if ( isFontReference(name) )
  { makeBuiltinFonts();
    if ( (obj = getObjectAssoc(name)) )
      answer(obj);
  }

  if ( exceptionPce(PCE, NAME_undefinedAssoc, name, 0) &&
       (obj = getObjectAssoc(name)) )
    answer(obj);

  fail;
}
