/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static status
initialiseView(View v, Name name, Size size, DisplayObj display, Editor editor)
{ Editor e;

  if ( isDefault(editor) )
  { Size s2;

    if ( isDefault(size) && (s2 = getResourceValueObject(v, NAME_size)) )
      size = newObject(ClassSize, s2->w, s2->h, 0);

    TRY(e = get(v, NAME_createEditor, size, 0));
  } else
    e = editor;

  initialiseWindow((PceWindow) v, name,
		   getSizeGraphical((Graphical) e), display);

  return send(v, NAME_editor, e, 0);
}


static status
unlinkView(View v)
{ Editor e = v->editor;

  assign(v, editor, NIL);
  freeObject(e);

  return unlinkWindow((PceWindow) v);
}


static status
editorView(View v, Editor editor)
{ if ( notNil(v->editor) )
  { assign(v, editor, NIL);
    send(v->editor, NAME_destroy, 0);
  }
  assign(v, editor, editor);
  send(editor, NAME_set, ZERO, ZERO, 0);
  send(v, NAME_display, editor, 0);
  send(v, NAME_resizeMessage,
       newObject(ClassMessage, editor, NAME_Size, Arg(2), 0), 0);
  assign(v, keyboard_focus, editor);

  succeed;
}


static Editor
getCreateEditorView(View v, Size size)
{ Editor e;
  Int w, h;

  if ( notDefault(size) )
  { w = size->w;
    h = size->h;
  } else
    w = h = DEFAULT;

  if ( (e = newObject(ClassEditor, DEFAULT, w, h, 0)) )
  { FontObj f;

    if ( (f = getResourceValueObject(v, NAME_font)) )
      send(e, NAME_font, f, 0);

    answer(e);
  }

  fail;
}


static Point
getSelectionView(View v)
{ answer(getSelectionEditor(v->editor));
}


status
requestGeometryView(View v, Int x, Int y, Int w, Int h)
{ Editor e = v->editor;

  if ( notDefault(w) )
    w = mul(w, getExFont(e->font));
  if ( notDefault(h) )
    h = mul(h, getHeightFont(e->font));

  return requestGeometryWindow((PceWindow) v, x, y, w, h);
}


		/********************************
		*           OVERRULE		*
		********************************/

static status
clearView(View v)
{ return clearEditor(v->editor);
}


static status
selectionView(View v, Int from, Int to)
{ return selectionEditor(v->editor, from, to);
}


static StringObj
getSelectedView(View v)
{ return getSelectedEditor(v->editor);
}


static status
normaliseView(View v, Int from, Int to)
{ return normaliseEditor(v->editor, from, to);
}


static status
scrollToView(View v, Int index)
{ return scrollToEditor(v->editor, index);
}


static status
formatView(View v, CharArray fmt, int argc, Any *argv)
{ return formatEditor(v->editor, fmt, argc, argv);
}


status
makeClassView(Class class)
{ sourceClass(class, makeClassView, __FILE__, "$Revision$");

  localClass(class, NAME_editor, NAME_delegate, "editor", NAME_get,
	     "Editor displayed");

  termClass(class, "view",
	    1, NAME_label, NAME_displaySize, NAME_display, NAME_editor);
  prependDelegateClass(class, NAME_editor);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "label=[name]", "size=[size]",
	     "display=[display]", "editor=[editor]",
	     "Create from label, size, display and editor",
	     initialiseView);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Unlink the editor",
	     unlinkView);
  sendMethod(class, NAME_clear, NAME_delete, 0,
	     "Overrule window behaviour",
	     clearView);
  sendMethod(class, NAME_scrollTo, NAME_scroll, 1, "int",
	     "Overrule window behaviour",
	     scrollToView);
  sendMethod(class, NAME_format, NAME_format, 2, "char_array", "any ...",
	     "Formatted insert (see `string->format')",
	     formatView);
  sendMethod(class, NAME_selection, NAME_selection, 2, "from=int", "to=int",
	     "Overrule window behaviour",
	     selectionView);
  sendMethod(class, NAME_normalise, NAME_scroll, 2, "from=int", "to=int",
	     "Overrule window behaviour",
	     normaliseView);
  sendMethod(class, NAME_editor, DEFAULT, 1, "editor",
	     "Associate editor with view",
	     editorView);
  sendMethod(class, NAME_requestGeometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Map size to character units",
	     requestGeometryView);

  getMethod(class, NAME_selection, NAME_selection, "point", 0,
	    "New point with start and end of selection",
	    getSelectionView);
  getMethod(class, NAME_selected, NAME_selection, "string", 0,
	    "New string with contents of selection",
	    getSelectedView);
  getMethod(class, NAME_createEditor, NAME_create, "editor", 1, "size=[size]",
	    "Create the editor of the view",
	    getCreateEditorView);

  attach_resource(class, "pen",    "int",   "0",
		  "Pen (done by <-editor)");
  attach_resource(class, "size",   "size",  "size(80,20)",
		  "Default size in `characters x lines'");

  succeed;
}

