/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseSheet(Sheet sh, int argc, Attribute *argv)
{ assign(sh, attributes, newObjectv(ClassChain, argc, (Any *)argv));

  succeed;
}


Sheet
getCopySheet(Sheet sh)
{ if ( notNil(sh) )
  { Sheet copy = answerObjectv(classOfObject(sh), 0, NULL);

    assign(copy, attributes, getCopyChain(sh->attributes));

    answer(copy);
  } else
    answer(sh);
}


status
isAttributeSheet(Sheet sh, Any name)
{ Cell cell;

  for_cell(cell, sh->attributes)
  { Attribute att = cell->value;

    if ( EQ(att->name, name) )
      succeed;
  }

  fail;
}


Attribute
getMemberSheet(Sheet sh, register Any name)
{ register Cell cell;

  for_cell(cell, sh->attributes)
  { Attribute att = cell->value;

    if ( att->name == name )
      answer(att);
  }

  fail;
}


status
deleteSheet(Sheet sh, Any name)
{ Cell cell;
  Attribute att;

  for_cell(cell, sh->attributes)
  { att = cell->value;

    if ( EQ(att->name, name) )
    { deleteCellChain(sh->attributes, cell);
      succeed;
    }
  }
  fail;
}


static Chain
getAttributeNamesSheet(Sheet sh)
{ Chain chain;
  Cell cell;

  chain = answerObject(ClassChain, 0);
  for_cell(cell, sh->attributes)
    appendChain(chain, ((Attribute) cell->value)->name);

  answer(chain);
}


Any
getValueSheet(Sheet sh, Any name)
{ Cell cell;

  for_cell(cell, sh->attributes)
  { Attribute att = cell->value;

    if ( EQ(att->name, name) )
      answer(att->value);
  }

  fail;
}


status
valueSheet(Sheet sh, Any name, Any value)
{ Cell cell;

  for_cell(cell, sh->attributes)
  { Attribute a = cell->value;

    if ( EQ(a->name, name) )
    { assign(a, value, value);
      succeed;
    }
  }

  return appendChain(sh->attributes,
		     newObject(ClassAttribute, name, value, 0));
}


static Any
getCatchAllSheet(Sheet sh, Name name)
{ Any rval = getValueSheet(sh, (Any) name);

  if ( !rval )
    errorPce(sh, NAME_noBehaviour,  CtoName("<-"), name);

  answer(rval);
}


static status
catchAllSheet(Sheet sh, Name name, Any value)
{ return valueSheet(sh, (Any) name, value);
}


static status
appendSheet(Sheet sh, Attribute att)
{ Cell cell;

  for_cell(cell, sh->attributes)
  { Attribute a = cell->value;
    
    if ( EQ(a->name, att->name) )
    { assign(a, value, att->value);
      succeed;
    }
  }

  appendChain(sh->attributes, att);
  succeed;
}


static Int
getAritySheet(Sheet sh)
{ answer(getSizeChain(sh->attributes));
}


static Any
getArgSheet(Sheet sh, Int arg)
{ answer(getNth1Chain(sh->attributes, arg));
}


static status
forAllSheet(Sheet sh, Code msg)
{ Cell cell, c2;

  for_cell_save(cell, c2, sh->attributes)
    TRY( forwardCode(msg, cell->value, 0) );

  succeed;
}


static status
forSomeSheet(Sheet sh, Code msg)
{ Cell cell, c2;

  for_cell_save(cell, c2, sh->attributes)
    forwardCode(msg, cell->value, 0);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_value[] =
        { "key=any", "value=any" };
static char *T_catchAll[] =
        { "key=name", "value=any" };

/* Instance Variables */

static vardecl var_sheet[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_storage, "Attributes of the sheet")
};

/* Send Methods */

static senddecl send_sheet[] =
{ SM(NAME_initialise, 1, "member=attribute ...", initialiseSheet,
     DEFAULT, "Create sheet from attributes"),
  SM(NAME_append, 1, "attribute", appendSheet,
     NAME_attributes, "Append attribute"),
  SM(NAME_delete, 1, "name=any", deleteSheet,
     NAME_attributes, "Delete named attribute"),
  SM(NAME_forAll, 1, "action=code", forAllSheet,
     NAME_iterate, "Run code on all attributes (demand acceptance)"),
  SM(NAME_forSome, 1, "action=code", forSomeSheet,
     NAME_iterate, "Run code on all attributes"),
  SM(NAME_isAttribute, 1, "name=any", isAttributeSheet,
     NAME_meta, "Test if object is name of an attribute"),
  SM(NAME_catchAll, 2, T_catchAll, catchAllSheet,
     NAME_value, "Set attribute named selector"),
  SM(NAME_value, 2, T_value, valueSheet,
     NAME_value, "Set named attribute to value")
};

/* Get Methods */

static getdecl get_sheet[] =
{ GM(NAME_Arg, 1, "attribute", "int", getArgSheet,
     DEFAULT, "Nth-1 argument of term"),
  GM(NAME_Arity, 0, "int", NULL, getAritySheet,
     DEFAULT, "Arity of term"),
  GM(NAME_attributeNames, 0, "chain", NULL, getAttributeNamesSheet,
     NAME_meta, "New chain with attribute names"),
  GM(NAME_member, 1, "attribute", "key=any", getMemberSheet,
     NAME_meta, "Attribute object with name"),
  GM(NAME_catchAll, 1, "value=any", "key=name", getCatchAllSheet,
     NAME_value, "Get value associated with selector"),
  GM(NAME_value, 1, "any", "any", getValueSheet,
     NAME_value, "Get value associated with name")
};

/* Resources */

#define rc_sheet NULL
/*
static classvardecl rc_sheet[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(sheet_decls,
          var_sheet, send_sheet, get_sheet, rc_sheet,
          ARGC_UNKNOWN, NULL,
          "$Rev$");


status
makeClassSheet(Class class)
{ return declareClass(class, &sheet_decls);
}

