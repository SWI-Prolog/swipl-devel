/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static status
initialiseView(View v, Name name, Size size, DisplayObj display, Editor editor)
{ Editor e;

  if ( isDefault(editor) )
  { Size s2;

    if ( isDefault(size) && (s2 = getClassVariableValueObject(v, NAME_size)) )
      size = newObject(ClassSize, s2->w, s2->h, EAV);

    TRY(e = get(v, NAME_createEditor, size, EAV));
  } else
    e = editor;

  initialiseWindow((PceWindow) v, name,
		   getSizeGraphical((Graphical) e), display);

  return send(v, NAME_editor, e, EAV);
}


static status
unlinkView(View v)
{ Editor e = v->editor;

  unlinkWindow((PceWindow) v);
  assign(v, editor, NIL);
  freeObject(e);

  succeed;
}


static status
editorView(View v, Editor editor)
{ if ( notNil(v->editor) )
  { assign(v, editor, NIL);
    send(v->editor, NAME_destroy, EAV);
  }
  assign(v, editor, editor);
  send(editor, NAME_set, ZERO, ZERO, EAV);
  send(v, NAME_display, editor, EAV);
  send(v, NAME_resizeMessage,
       newObject(ClassMessage, editor, NAME_Size, Arg(2), EAV), EAV);
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

  if ( (e = newObject(ClassEditor, DEFAULT, w, h, EAV)) )
  { FontObj f;

    if ( (f = getClassVariableValueObject(v, NAME_font)) )
      send(e, NAME_font, f, EAV);

    answer(e);
  }

  fail;
}


static Point
getSelectionView(View v)
{ answer(getSelectionEditor(v->editor));
}


static status
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
selectionView(View v, Int from, Int to, Name status)
{ return selectionEditor(v->editor, from, to, status);
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
scrollToView(View v, Int index, Int screenline)
{ return scrollToEditor(v->editor, index, screenline);
}


static status
formatView(View v, CharArray fmt, int argc, Any *argv)
{ return formatEditor(v->editor, fmt, argc, argv);
}


static status
backgroundView(View v, Any bg)
{ return backgroundEditor(v->editor, bg);
}


static status
colourView(View v, Any c)
{ return colourEditor(v->editor, c);
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_format[] =
        { "char_array", "any ..." };
static char *T_fromAint_toAint[] =
        { "from=int", "to=int" };
static char *T_initialise[] =
        { "label=[name]", "size=[size]", "display=[display]", "editor=[editor]" };
static char *T_requestGeometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_selection[] =
        { "mark=[int]", "caret=[int]",
	  "status=[{active,inactive,highlight}]"
	};
static char *T_scrollTo[] = 
	{ "index=[int]", "screenline=[int]" };

/* Instance Variables */

static vardecl var_view[] =
{ IV(NAME_editor, "editor", IV_GET,
     NAME_delegate, "Editor displayed")
};

/* Send Methods */

static senddecl send_view[] =
{ SM(NAME_editor, 1, "editor", editorView,
     DEFAULT, "Associate editor with view"),
  SM(NAME_initialise, 4, T_initialise, initialiseView,
     DEFAULT, "Create from label, size, display and editor"),
  SM(NAME_requestGeometry, 4, T_requestGeometry, requestGeometryView,
     DEFAULT, "Map size to character units"),
  SM(NAME_unlink, 0, NULL, unlinkView,
     DEFAULT, "Unlink the editor"),
  SM(NAME_clear, 0, NULL, clearView,
     NAME_delete, "Overrule window behaviour"),
  SM(NAME_format, 2, T_format, formatView,
     NAME_format, "Formatted insert (see `string->format')"),
  SM(NAME_normalise, 2, T_fromAint_toAint, normaliseView,
     NAME_scroll, "Overrule window behaviour"),
  SM(NAME_scrollTo, 2, T_scrollTo, scrollToView,
     NAME_scroll, "Overrule window behaviour"),
  SM(NAME_selection, 3, T_selection, selectionView,
     NAME_selection, "Overrule window behaviour"),
  SM(NAME_background, 1, "colour|pixmap", backgroundView,
     NAME_appearance, "Backround for the window"),
  SM(NAME_colour, 1, "colour|pixmap", colourView,
     NAME_appearance, "Foreground colour for the text")
};

/* Get Methods */

static getdecl get_view[] =
{ GM(NAME_createEditor, 1, "editor", "size=[size]", getCreateEditorView,
     NAME_create, "Create the editor of the view"),
  GM(NAME_selected, 0, "string", NULL, getSelectedView,
     NAME_selection, "New string with contents of selection"),
  GM(NAME_selection, 0, "point", NULL, getSelectionView,
     NAME_selection, "New point with start and end of selection")
};

/* Resources */

static classvardecl rc_view[] =
{ RC(NAME_pen, "int", "0",
     "Pen (done by <-editor)"),
  RC(NAME_size, "size", "size(80,20)",
     "Default size in `characters x lines'"),
  RC(NAME_background, RC_REFINE, "@_dialog_bg", NULL)
};

/* Class Declaration */

static Name view_termnames[] = { NAME_name };

ClassDecl(view_decls,
          var_view, send_view, get_view, rc_view,
          1, view_termnames,
          "$Rev$");

status
makeClassView(Class class)
{ declareClass(class, &view_decls);
  prependDelegateClass(class, NAME_editor);

  succeed;
}

