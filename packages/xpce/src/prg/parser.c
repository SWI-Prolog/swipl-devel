/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/lang.h>

NewClass(parser)
  Tokeniser	tokeniser;		/* The tokeniser */
  ChainTable	operators;		/* Operator table */
  HashTable	active;			/* Active symbols */
End;


Name openbracket;
Name closebracket;
Name comma;


static status
initialiseParser(Parser p, Tokeniser t)
{ assign(p, tokeniser, t);
  assign(p, operators, newObject(ClassChainTable, 0));
  assign(p, active,    newObject(ClassHashTable, 0));

  succeed;
}

		 /*******************************
		 *	   DECLARACTIONS	*
		 *******************************/

static status
operatorParser(Parser p, Operator op)
{ appendChainTable(p->operators, op, ON);
  symbolTokeniser(p->tokeniser, op->name);

  succeed;
}

		 /*******************************
		 *	OUTPUT GENERATION	*
		 *******************************/

static Any
getBuildTermParser(Parser p, Class class, int argc, Any *argv)
{ answer(answerObjectv(class, argc, argv));
}


		 /*******************************
		 *	      SPECIALS		*
		 *******************************/

#define MAX_ARGV 256

static Any
getListParser(Parser p, Name end, Name delimiter, Name functor)
{ Any argv[MAX_ARGV];
  int argc = 0;
  Any arg;
  Any token;
  Chain endterm;

  if ( isDefault(end) )
    end = closebracket;
  if ( isDefault(delimiter) )
    delimiter = comma;
  if ( notDefault(argv) )
    argv[argc++] = functor;

  TRY(token = get(p, NAME_token, 0));
  if ( token == end )
    answer(getv(p, NAME_buildTerm, argc, argv));
  else
    send(p, NAME_token, token, 0);		/* push back */

  endterm = answerObject(ClassChain, end, delimiter, 0); /* TBD: reuse! */

  for(;;)
  { Any dl;

    TRY(arg = get(p, NAME_term, endterm, 0));
    argv[argc++] = arg;

    TRY(dl = get(p, NAME_token, 0));
    if ( dl == end )
    { doneObject(endterm);
      answer(getv(p, NAME_buildTerm, argc, argv));
    }
    if ( isNil(delimiter) )
      send(p, NAME_token, token, 0);
  }
}


static Operator
prefix_op(Chain ch)
{ Cell cell;

  for_cell(cell, ch)
  { Operator o = cell->value;
    if ( o->left_priority == ZERO )
      return o;
  }

  fail;
}


static Operator
postfix_op(Chain ch)
{ Cell cell;

  for_cell(cell, ch)
  { Operator o = cell->value;
    if ( o->right_priority == ZERO )
      return o;
  }

  fail;
}


static Operator
infix_op(Chain ch)
{ Cell cell;

  for_cell(cell, ch)
  { Operator o = cell->value;
    if ( o->left_priority != ZERO && o->right_priority != ZERO )
      return o;
  }

  fail;
}


static status
reduce(Parser p, Chain out, Chain side, int pri)
{ Operator o2;

  while( (o2=getHeadChain(side)) && valInt(o2->priority) <= pri )
  { deleteHeadChain(side);

    DEBUG(NAME_term, printf("Reduce %s\n", pp(o2->name)));
    if ( o2->left_priority != ZERO && o2->right_priority != ZERO ) /* infix */
    { Any a1 = getDeleteHeadChain(out);
      Any a2 = getDeleteHeadChain(out);
      Any t;

      TRY(t = get(p, NAME_buildTerm, o2->name, a2, a1, 0));
      prependChain(out, t);
    } else				/* pre- or postfix */
    { Any a1 = getDeleteHeadChain(out);
      Any t;

      TRY(t = get(p, NAME_buildTerm, o2->name, a1, 0));
      prependChain(out, t);
    }
  }

  succeed;
}


static int
modify(Parser p, int rmo, Chain out, Chain side, int pri)
{ Operator s, o2;
  Chain ops;

  if ( (s = getHeadChain(side)) && valInt(s->priority) < pri )
  { if ( s->left_priority == ZERO && rmo == 0 )	/* prefix */
    { rmo++;
      prependChain(out, s->name);
      deleteHeadChain(side);
      DEBUG(NAME_term, printf("Modify prefix %s --> name\n", pp(s->name)));
    } else if ( s->left_priority != ZERO && s->right_priority != ZERO &&
		rmo == 0 &&
		!emptyChain(out) &&
		(ops = getMemberHashTable((HashTable)p->operators, s->name)) &&
		(o2 = postfix_op(ops)) )
    { rmo++;
      prependChain(out, newObject(ClassChain,
				  o2->name, getDeleteHeadChain(out), 0));
      deleteHeadChain(side);
      DEBUG(NAME_term, printf("Modify infix %s --> postfix\n", pp(s->name)));
    }
  }

  return rmo;
}



static Any
getTermParser(Parser p, Chain end)
{ Any token;
  Any active, rval;
  Function f;
  Chain out  = answerObject(ClassChain, 0);
  Chain side = answerObject(ClassChain, 0);
  int rmo = 0;
  
  for(;;)
  { Chain ops;

    TRY(token = get(p, NAME_token, 0));

					/* Active tokens */
    if ( (active = getMemberHashTable(p->active, token)) )
    { if ( (f = checkType(active, TypeFunction, NIL)) &&
	   (rval = getForwardFunction(f, p, token, 0)) )
	token = rval;
      else if ( instanceOfObject(active, ClassCode) )
      { forwardCode(active, p, token, NIL, 0);
	continue;
      }
    }

    if ( isName(token) && getPeekTokeniser(p->tokeniser) == toInt('(') )
    { Any t2;

      if ( (t2 = get(p, NAME_token, 0)) != openbracket )
	send(p, NAME_token, t2, 0);
      else
	TRY(token = get(p, NAME_list, closebracket, comma, token, 0));
    }
					/* end detection */
    if ( notDefault(end) && memberChain(end, token) )
    { send(p, NAME_token, token, 0);
      goto exit;
    }

					/* operators */
    if ( isName(token) &&
	 (ops = getMemberHashTable((HashTable)p->operators, token)) )
    { Operator op;

      if ( rmo == 0 && (op = prefix_op(ops)) )
      { DEBUG(NAME_term, printf("Prefix op %s\n", pp(token)));
	TRY(reduce(p, out, side, valInt(op->left_priority)));
	prependChain(side, op);
	continue;
      } else if ( rmo == 1 )
      { if ( (op = infix_op(ops)) )
	{ DEBUG(NAME_term, printf("Infix op %s\n", pp(token)));
	  rmo = modify(p, rmo, out, side, valInt(op->left_priority));
	  TRY(reduce(p, out, side, valInt(op->left_priority)));
	  prependChain(side, op);
	  rmo--;
	  continue;
	} else if ( (op = postfix_op(ops)) )
	{ DEBUG(NAME_term, printf("Postfix op %s\n", pp(token)));
	  rmo = modify(p, rmo, out, side, valInt(op->left_priority));
	  TRY(reduce(p, out, side, valInt(op->left_priority)));
	  prependChain(side, op);
	  continue;
	}
      }
    }

    if ( rmo == 0 )
    { rmo++;
      DEBUG(NAME_term, printf("Pushing %s\n", pp(token)));
      prependChain(out, token);
    } else
    { send(p, NAME_syntaxError, CtoName("Operator expected"), 0);
      fail;
    }
  }

exit:
  rmo = modify(p, rmo, out, side, 1201);
  TRY(reduce(p, out, side, 1201));
  
  if ( out->size == ONE && side->size == ZERO )
  { Any rval = getDeleteHeadChain(out);
    doneObject(out);
    doneObject(side);

    answer(rval);
  }

  if ( out->size == ZERO && side->size == ONE )
  { Operator op = getHeadChain(side);

    doneObject(out);
    doneObject(side);

    answer(op->name);
  }

  doneObject(out);
  doneObject(side);

  send(p, NAME_syntaxError, CtoName("Unbalanced operators"), 0);
  fail;
}


status
makeClassParser(Class class)
{ sourceClass(class, makeClassParser, __FILE__, "$Revision$");

  localClass(class, NAME_tokeniser, NAME_syntax, "tokeniser", NAME_both,
	     "Tokeniser used for this parser");
  localClass(class, NAME_operators, NAME_syntax, "chain_table", NAME_both,
	     "Operator table for this parser");
  localClass(class, NAME_active, NAME_syntax, "hash_table", NAME_both,
	     "Active tokens");

  termClass(class, "parser", 1, NAME_tokeniser);
  delegateClass(class, NAME_tokeniser);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "tokeniser=tokeniser",
	     "Initialise",
	     initialiseParser);
  sendMethod(class, NAME_operator, NAME_syntax, 1, "operator=operator",
	     "Declare operator for parser",
	     operatorParser);

  getMethod(class, NAME_term, NAME_parse, "term=unchecked", 1, "end=[chain]",
	    "Read next term",
	    getTermParser);
  getMethod(class, NAME_list, NAME_parse, "object=unchecked", 3,
	    "end=[name]", "delimiter=[name]*", "functor=[name]",
	    "Read terms upto end",
	    getListParser);
  getMethod(class, NAME_buildTerm, NAME_build, "object=unchecked", 2,
	    "class=class", "argument=unchecked ...",
	    "Create object from data read",
	    getBuildTermParser);

  openbracket = CtoName("(");
  closebracket = CtoName(")");
  comma = CtoName(",");

  succeed;
}
