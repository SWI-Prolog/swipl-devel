/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseSheet(Sheet sh, int argc, Attribute *argv)
{ assign(sh, attributes, newObjectv(ClassChain, argc, (Any *)argv));

  succeed;
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
{ return getValueSheet(sh, (Any) name);
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


status
makeClassSheet(Class class)
{ sourceClass(class, makeClassSheet, __FILE__, "$Revision$");

  localClass(class, NAME_members, NAME_storage, "chain", NAME_get,
	     "Attributes of the sheet");

  termClass(class, "sheet", ARGC_UNKNOWN);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "member=attribute ...",
	     "Create sheet from attributes",
	     initialiseSheet);
  sendMethod(class, NAME_append, NAME_attributes, 1, "attribute",
	     "Append attribute",
	     appendSheet);
  sendMethod(class, NAME_value, NAME_value, 2, "key=any", "value=any",
	     "Set named attribute to value",
	     valueSheet);
  sendMethod(class, NAME_delete, NAME_attributes, 1, "name=any",
	     "Delete named attribute",
	     deleteSheet);
  sendMethod(class, NAME_isAttribute, NAME_meta, 1, "name=any",
	     "Test if object is name of an attribute",
	     isAttributeSheet);
  sendMethod(class, NAME_forAll, NAME_iterate, 1, "action=code",
	     "Run code on all attributes (demand acceptance)",
	     forAllSheet);
  sendMethod(class, NAME_forSome, NAME_iterate, 1, "action=code",
	     "Run code on all attributes",
	     forSomeSheet);
  sendMethod(class, NAME_catchAll, NAME_value, 2, "key=name", "value=any",
	     "Set attribute named selector",
	     catchAllSheet);

  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity of term",
	    getAritySheet);
  getMethod(class, NAME_Arg, DEFAULT, "attribute", 1, "int",
	    "Nth-1 argument of term",
	    getArgSheet);
  getMethod(class, NAME_value, NAME_value, "any", 1, "any",
	    "Get value associated with name",
	    getValueSheet);
  getMethod(class, NAME_member, NAME_meta, "attribute", 1, "key=any",
	    "Attribute object with name",
	    getMemberSheet);
  getMethod(class, NAME_attributeNames, NAME_meta, "chain", 0,
	    "New chain with attribute names",
	    getAttributeNamesSheet);
  getMethod(class, NAME_catchAll, NAME_value, "value=any", 1, "key=name",
	    "Get value associated with selector",
	    getCatchAllSheet);

  succeed;
}

