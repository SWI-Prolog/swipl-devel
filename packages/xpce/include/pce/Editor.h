/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_EDITOR_H
#define _PCE_EDITOR_H

PceExternalClass(ClassEditor);
class PceEditor :public PceObject
{
public:
  PceEditor() :
    PceObject(ClassEditor)
  {
  }
  PceEditor(PceArg text) :
    PceObject(ClassEditor, text)
  {
  }
  PceEditor(PceArg text, PceArg width) :
    PceObject(ClassEditor, text, width)
  {
  }
  PceEditor(PceArg text, PceArg width, PceArg height) :
    PceObject(ClassEditor, text, width, height)
  {
  }
  PceEditor(PceArg text, PceArg width, PceArg height, PceArg margin) :
    PceObject(ClassEditor, text, width, height, margin)
  {
  }
};

inline PceEditor
AsEditor(PceArg a)
{ return *((PceEditor*) &a);
}

#endif /*!_PCE_EDITOR_H*/
