/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

		 /*******************************
		 *	      CREATE		*
		 *******************************/

status
initialiseParagraph(Paragraph par, Int width, Int indent, Bool justify)
{ initialiseLayoutManager(par);

  assign(c, width,   width);
  assign(c, indent,  indent);
  assign(c, justify, justify);
  assign(c, words,   newObject(ClassChain, 0));

  return obtainResourcesObject(par);
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static status
computeParagraph(Paragraph par)
{ if ( notNil(par->request_compute) )
  { 

    assign(par, request_compute, NIL);
  }

  succeed;
}


		 /*******************************
		 *	    ATTRIBUTES		*
		 *******************************/

static status
assignParagraph(Paragraph par, Name slot, Any value)
{ Class class = classOfObject(par);
  Variable var;

  if ( (var = getInstanceVariableClass(class, (Any) slot)) )
  { if ( getGetVariable(var, par, 0, NULL) != value )
    { setSlotInstance(tab, var, value);
      requestComputeLayoutManager((LayoutManager)par, DEFAULT);
    }

    succeed;
  }

  fail;
}


static status
widthParagraph(Paragraph par, Int w)
{ return assignParagraph(par, NAME_width, w);
}


static status
justifyParagraph(Paragraph par, Bool justify)
{ return assignParagraph(par, NAME_justify, justify);
}


static status
indentParagraph(Paragraph par, Int i)
{ return assignParagraph(par, NAME_indent, i);
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
	{ "width=0..", "indent=[int]", "justify=[bool]" };

/* Instance Variables */

static vardecl var_paragraph[] =
{ SV(NAME_width, "0..", IV_GET|IV_STORE, widthParagraph,
     NAME_layout, "Width (in pixels) of the paragraph"),
  SV(NAME_justify, "bool", IV_GET|IV_STORE, justifyParagraph,
     NAME_layout, "Align all lines at the right"),
  SV(NAME_indent, "int", IV_GET|IV_STORE, indentParagraph,
     NAME_layout, "Indentation of the first line"),
  IV(NAME_words,  "chain", IV_GET, 
     NAME_contents, "`Words' represented")
};
  
/* Send Methods */

static senddecl send_paragraph[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseParagraph,
     DEFAULT, "Initialise paragraph layout manager"),
  SM(NAME_append, 1, "graphical", appendParagraph,
     DEFAULT, "Append graphical to the paragraph layout"),
  SM(NAME_compute, 0, NULL, computeParagraph,
     DEFAULT, "Compute the layout of the member graphicals")
};

/* Get Methods */

#define get_paragraph NULL
/*
static getdecl get_paragraph[] =
{ 
};
*/

/* Resources */

static resourcedecl rc_paragraph[] =
{ RC(NAME_width, "0..", "200",
     "Total width of the paragraph"),
  RC(NAME_indent, "int", "0",
     "Indentation of the first line"),
  RC(NAME_justify, "bool", "@off",
     "Justify the right-margin")
};

/* Class Declaration */

static Name paragraph_termnames[] = { NAME_image };

ClassDecl(paragraph_decls,
          var_paragraph,
	  send_paragraph,
	  get_paragraph,
	  rc_paragraph,
          1, paragraph_termnames,
          "$Rev$");

status
makeClassParagraph(Class class)
{ return declareClass(class, &paragraph_decls);
}

