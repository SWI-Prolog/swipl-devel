/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseClassStub(ClassStub cstub, Name name, Class super, StringObj summary)
{ if ( getMemberHashTable(classTable, name) )
    fail;				/* error in getLookupClassStub() */

  if ( isDefault(summary) )
    summary = NIL;

  initialiseProgramObject(cstub);

  assign(cstub, name, name);
  assign(cstub, super_class, super);
  assign(cstub, summary, summary);
  assign(cstub, creator, inBoot ? NAME_builtIn : NAME_host);

  if ( notNil(super) )
  { if ( isNil(super->sub_classes) )
      assign(super, sub_classes, newObject(ClassChain, cstub, 0));
    else
      addChain(super->sub_classes, cstub);
  }

  { char tmp[LINESIZE];

    appendHashTable(classTable, cstub->name, cstub);
    sprintf(tmp, "%s_class", strName(cstub->name));
    newAssoc(CtoKeyword(tmp), cstub);
  }
  appendHashTable(classTable, name, cstub);

  succeed;
}


static ClassStub
getLookupClassStub(Class class, Name name, Class super, StringObj summary)
{ ClassStub cstub = getMemberHashTable(classTable, name);

  if ( cstub )
  { if ( notDefault(super) && cstub->super_class != super )
    { errorPce(cstub, NAME_cannotChangeSuperClass);
      fail;
    }

    if ( notDefault(summary) )
      assign(cstub, summary, summary);

    answer(cstub);
  }

  fail;
}


static ClassStub
getConvertClassStub(Class class, Name name)
{ ClassStub cstub;

  if ( (cstub = getMemberHashTable(classTable, name)) )
    answer(cstub);

  fail;
}


static Chain
getSubClassesClassStub(ClassStub cstub)
{ if ( notNil(cstub->sub_classes) )
    answer(cstub->sub_classes);

  fail;
}


static Class
getRealiseClassStub(ClassStub cstub)
{ fail;
}

		 /*******************************
		 *	     DELEGATION		*
		 *******************************/

static status
catchAllClassStubv(ClassStub cstub, int argc, Any *argv)
{ Class class;

  if ( classOfObject(cstub) == ClassClassStub &&
       (class = getConvertClass(ClassClass, cstub)) )
    return sendv(class, argv[0], argc-1, argv+1);

  fail;
}


static Any
getCatchAllClassStubv(ClassStub cstub, int argc, Any *argv)
{ Class class;

  if ( classOfObject(cstub) == ClassClassStub &&
       (class = getConvertClass(ClassClass, cstub)) )
    return getv(class, argv[0], argc-1, argv+1);

  fail;
}


#ifndef O_RUNTIME

		/********************************
		*        MANUAL SUPPORT		*
		********************************/


static Name
getManIdClass(ClassStub cstub)
{ char buf[LINESIZE];

  sprintf(buf, "C.%s", strName(cstub->name));

  answer(CtoName(buf));
}


static Name
getManIndicatorClass(ClassStub cstub)
{ answer(CtoName("C"));
}

#endif /*O_RUNTIME*/

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_nameAname_superADclass_stubDN_summaryADstringDN[] =
        { "name=name", "super=[class_stub]*", "summary=[string]*" };

/* Instance Variables */

static vardecl var_classStub[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Name of the class"),
  IV(NAME_summary, "string*", IV_BOTH,
     NAME_manual, "Summary documentation for class"),
  IV(NAME_creator, "{built_in,host,C++}", IV_GET,
     NAME_manual, "Who created the class"),
  IV(NAME_superClass, "class_stub*", IV_GET,
     NAME_type, "Immediate super class"),
  IV(NAME_subClasses, "chain*", IV_NONE,
     NAME_type, "Sub classes")
};

/* Send Methods */

static senddecl send_classStub[] =
{ SM(NAME_initialise, 3, T_nameAname_superADclass_stubDN_summaryADstringDN, initialiseClassStub,
     DEFAULT, "Create Class Stub object"),
  SM(NAME_catchAll, 1, "unchecked ...", catchAllClassStubv,
     NAME_delegate, "Convert to class and delegate message to the class")
};

/* Get Methods */

static getdecl get_classStub[] =
{ GM(NAME_convert, 1, "class_stub", "name=name", getConvertClassStub,
     DEFAULT, "Reuse predefined stub"),
  GM(NAME_lookup, 3, "class_stub", T_nameAname_superADclass_stubDN_summaryADstringDN, getLookupClassStub,
     DEFAULT, "Reuse predefined stub"),
  GM(NAME_realise, 0, "class", NULL, getRealiseClassStub,
     NAME_autoload, "Create the actual class"),
  GM(NAME_catchAll, 1, "unchecked", "unchecked ...", getCatchAllClassStubv,
     NAME_delegate, "Convert to class and delegate message to the class"),
#ifndef O_RUNTIME
  GM(NAME_manId, 0, "name", NULL, getManIdClass,
     NAME_manual, "Card Id for method"),
  GM(NAME_manIndicator, 0, "name", NULL, getManIndicatorClass,
     NAME_manual, "Manual type indicator (`C')"),
#endif /*O_RUNTIME*/
  GM(NAME_subClasses, 0, "chain", NULL, getSubClassesClassStub,
     NAME_type, "Sub classes (fails if none available)")
};

/* Resources */

#define rc_classStub NULL
/*
static resourcedecl rc_classStub[] =
{ 
};
*/

/* Class Declaration */

static Name classStub_termnames[] = { NAME_name, NAME_super, NAME_summary };

ClassDecl(classStub_decls,
          var_classStub, send_classStub, get_classStub, rc_classStub,
          3, classStub_termnames,
          "$Rev$");

status
makeClassClassStub(Class class)
{ return declareClass(class, &classStub_decls);
}

