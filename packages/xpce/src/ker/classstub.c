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


status
makeClassClassStub(Class class)
{ localClass(class, NAME_name, NAME_name, "name", NAME_get,
	     "Name of the class");
  localClass(class, NAME_summary, NAME_manual, "string*", NAME_both,
	     "Summary documentation for class");
  localClass(class, NAME_creator, NAME_manual, "{built_in,host}", NAME_both,
	     "Who created the class: {built_in,host}");
  localClass(class, NAME_superClass, NAME_type, "class_stub*", NAME_get,
	     "Imediate super class");
  localClass(class, NAME_subClasses, NAME_type, "chain*", NAME_none,
	     "Sub classes");

  sourceClass(class, makeClassClassStub, __FILE__, "$Revision$");
  termClass(class, "class_stub", 3, NAME_name, NAME_super, NAME_summary);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "name=name", "super=[class_stub]*", "summary=[string]*",
	     "Create Class Stub object",
	     initialiseClassStub);

  getMethod(class, NAME_lookup, DEFAULT, "class_stub", 3,
	    "name=name", "super=[class_stub]*", "summary=[string]*",
	    "Reuse predefined stub",
	    getLookupClassStub);
  getMethod(class, NAME_convert, DEFAULT, "class_stub", 1,
	    "name=name",
	    "Reuse predefined stub",
	    getConvertClassStub);
  sendMethod(class, NAME_catchAll, NAME_delegate, 1, "unchecked ...",
	     "Convert to class and delegate message to the class",
	     catchAllClassStubv);

#ifndef O_RUNTIME
  getMethod(class, NAME_manId, NAME_manual, "name", 0,
	    "Card Id for method",
	    getManIdClass);
  getMethod(class, NAME_manIndicator, NAME_manual, "name", 0,
	    "Manual type indicator (`C')",
	    getManIndicatorClass);
#endif /*O_RUNTIME*/
  getMethod(class, NAME_subClasses, NAME_type, "chain", 0,
	    "Sub classes (fails if none available)",
	    getSubClassesClassStub);

  getMethod(class, NAME_realise, NAME_autoload, "class", 0,
	    "Create the actual class",
	    getRealiseClassStub);
  getMethod(class, NAME_catchAll, NAME_delegate, "unchecked", 1,
	    "unchecked ...",
	    "Convert to class and delegate message to the class",
	    getCatchAllClassStubv);

  succeed;
}

