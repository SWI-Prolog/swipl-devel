/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>		/* resource handling */
#include <h/unix.h>

static Any	loadNameObject(FILE *);
static int	pceSlotsClass(Class);
static status	checkConvertedObject(Any, ClassDef);
static Int	storeClass(Class, FileObj);
static status	storeExtensionsObject(Any obj, FileObj file);
static status	storeIdObject(Any obj, Int id, FileObj file);
static status	storeSlotsClass(Class class, FileObj file);
static status	restoreClass(FILE *fd);
static int	offsetVariable(Class class, Name name);

static int objects_saved;
static int classes_saved;
static int save_nesting;		/* depth of saved object */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Binary saved state of PCE object (collection). File format:

<file>		::= <magic>
		    <version>
		    <object>
		    {'s' <object>}		(= support objects)
		    {'n' <from> <slot> <to>}	(= nil-references)
		    'x' 

<magic>		::= <string>			(= SAVEMAGIC)
<version>	::= <word>			(= SAVEVERSION)

<object>	::= ['C' <class_def>]		(a Class slot definition)
		    'O'
		    <class_id>
		    <object_name>
		    {<extension>} 'x'
		    {<slot>}			(times according to class def)
		  | 'd'				(@default)
		  | 'n'				(@nil)
		  | 'a'				(@on)
		  | 'u'				(@off)
		  | 'r'				(@receiver)
		  | 'b'				(@block)
		  | '1'				(@arg1)
		  | '2'				(@arg2)
		  | '3'				(@arg3)
		  | '4'				(@arg4)
		  | '5'				(@arg5)
		  | '6'				(@arg6)
		  | '7'				(@arg7)
		  | '8'				(@arg8)
		  | '9'				(@arg9)
		  | '0'				(@arg10)
		  | 'N' <string>		(a name)
		  | 'S' <string> <string>	(HACK: a lisp_symbol)
		  | 'I' <integer>		(an integer)
		  | 'R' <object_name>		(reference to saved object)
		  | 'A' <string>		(reference to exernal object)

<extension>	::= 'a' <Object>		(Attribute sheet)
		  | 'c' <Object>		(Constraint-list)
		  | 's' <Object>		(SendMethod-list)
		  | 'g' <Object>		(GetMethod-list)
		  | 'r' <Object>		(Recogniser-list)
		  | 'h' <Object>		(Hyper-list)
		    
<object_name>	::= 'N' <string>		(name as reference)
		  | 'I' <word>			(integer as reference)
<abtract>	::= <slot>
<slot>		::= <object>

<class_def>	::= <class_name> <class_id>
		    <slots>			(number of pce typed slots)
		    {<slot_name>}		(<slots> times)
<class_name>	::= <string>
<class_id>	::= <word>
<slots>		::= <word>
<slot_name>	::= <string>
<slot_offset>	::= <byte>			(offset of slot above struct)
						(`object')

<string>	::= <size>{<char>}		(<size> times <char>)
<char>		::= <byte>

<byte>		::= (8 bits)
<word>		::= (32 bits)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static HashTable saveTable;
static HashTable saveClassTable;
static HashTable saveNilRefTable;
static Chain     candidateSaveRelations;

static inline Int
setSavedObj(Any obj)
{ objects_saved++;

  appendHashTable(saveTable, obj, toInt(objects_saved));

  return toInt(objects_saved);
}

static inline Int
setSavedClass(Class class)
{ classes_saved++;

  appendHashTable(saveClassTable, class, toInt(classes_saved));

  return toInt(classes_saved);
}

Int
isSavedObject(Any obj)
{ return getMemberHashTable(saveTable, obj);
}

#define isSavedClass(class)	getMemberHashTable(saveClassTable, class)

static status
candidateSaveRelation(Any r)
{ if ( !isSavedObject(r) )
  { if ( !candidateSaveRelations )
      candidateSaveRelations = newObject(ClassChain, r, 0);
    else
      appendChain(candidateSaveRelations, r);
  }

  succeed;
}


static status
saveRelations(FileObj f)
{ Any r;

  while( candidateSaveRelations &&
	 (r = getDeleteHeadChain(candidateSaveRelations)) )
  { if ( !isSavedObject(r) )
      TRY(send(r, NAME_SaveRelation, f, 0));
  }

  succeed;
}


static status
saveNilRefs(FileObj f)
{ if ( saveNilRefTable )
  { for_hash_table(saveNilRefTable, s,
		   { Instance inst = s->name;
		     Variable var  = s->value;
		     Any to = inst->slots[valInt(var->offset)];
		     Int ref;

		     if ( (ref = isSavedObject(to)) )
		     { DEBUG(NAME_save,
			     printf("storing nil-ref %s-%s->%s\n",
				    pp(inst), pp(var->name), pp(to)));
		       storeCharFile(f, 'n');
		       storeIntFile(f, storeClass(classOfObject(inst), f));
		       storeIdObject(inst, isSavedObject(inst), f);
		       storeIntFile(f, var->offset);
		       storeIdObject(to, ref, f);
		     }
		   });

    freeHashTable(saveNilRefTable);
    saveNilRefTable = NULL;
  }

  succeed;
}


status
saveInFileObject(Any obj, FileObj file)
{ status result;

  TRY(send(file, NAME_kind, NAME_binary, 0) &&
      send(file, NAME_open, NAME_write, 0));

  if ( SaveMagic == NULL )
    SaveMagic = SAVEMAGIC;

  objects_saved = classes_saved = save_nesting = 0;
  storeCharpFile(file, SaveMagic);
  storeWordFile(file, (Any) SAVEVERSION);
  saveTable = createHashTable(toInt(256), OFF);
  saveClassTable = createHashTable(toInt(256), OFF);
  if ( candidateSaveRelations )
    clearChain(candidateSaveRelations);
  result = (storeObject(obj, file) &&
	    saveRelations(file) &&
	    saveNilRefs(file) &&
	    storeCharFile(file, 'x'));
  closeFile(file);
  if ( !result )
    removeFile(file);
  DEBUG(NAME_statistics, printf("Saved %d objects of %d classes\n",
				objects_saved, classes_saved));
  freeHashTable(saveTable);
  freeHashTable(saveClassTable);
    
  
  return result ? SUCCEED : FAIL;
}


status
storeObject(Any obj, FileObj file)
{ /*DEBUG(NAME_save, printf("Storing %s from %ld\n",
	  pp(obj), ftell(file->fd)));*/

  if ( isInteger(obj) )
  { storeCharFile(file, 'I');
    storeIntFile(file, obj); 
    succeed;
  } 

  assert(isObject(obj));

  if ( instanceOfObject(obj, ClassVar) )
  { int a = (ulong)obj - (ulong)Arg(0);

    if ( a >= 1 && a <= 9 )
      return storeCharFile(file, '0' + (int) a);
    else if ( a == 10 )
      return storeCharFile(file, '0');
    else if ( obj == RECEIVER )
      return storeCharFile(file, 'r');
  } else if ( instanceOfObject(obj, ClassConstant) )
  { if ( isNil(obj) )
      return storeCharFile(file, 'n');
    else if ( isDefault(obj) )
      return storeCharFile(file, 'd');
    else if ( isOn(obj) )		/* booleans are constants! */
      return storeCharFile(file, 'a');
    else if ( isOff(obj) )
      return storeCharFile(file, 'u');
  }

  { Class class = classOfObject(obj);
    Name name;

    if ( isAClass(class, ClassName) )
    { if ( class == ClassName )
      { storeCharFile(file, 'N');
	storeNameFile(file, obj);
	succeed;
      } else if ( class->name == NAME_lispSymbol ) /* HACK */
      { storeCharFile(file, 'S');
	storeNameFile(file, obj);
	storeNameFile(file, get(obj, NAME_package, 0));
	succeed;
      }
    }

    DEBUG(NAME_save, printf(" [%3d] Storing %s from %ld\n",
			    save_nesting, pp(obj), ftell(file->fd)));

    if ( class->saveStyle == NAME_nil )
    { return storeCharFile(file, 'n');
    } else if ( class->saveStyle == NAME_external &&
	        (name = getNameAssoc(obj)) )
    { storeCharFile(file, 'A');
      storeNameFile(file, name);
      succeed;
    } else /*if ( equalName(class->saveStyle, NAME_normal) )*/
    { Int ref, classref;
      status rval;

      if ( (ref = isSavedObject(obj)) )
      { DEBUG(NAME_save, printf("Storing reference\n"));
	storeCharFile(file, 'R');
	return storeIdObject(obj, ref, file);
      }
      ref = setSavedObj(obj);
      TRY( classref = storeClass(class, file) );
      storeCharFile(file, 'O');
      storeIntFile(file, classref);
      storeIdObject(obj, ref, file);
      storeExtensionsObject(obj, file);
      save_nesting++;
      if ( class->saveFunction )
      { DEBUG(NAME_save, printf("Using private function\n"));
	rval = (*class->saveFunction)(obj, file);
      } else
      { if ( allPceSlotsClass(class) )
	  rval = storeSlotsObject(obj, file);
	else
	{ errorPce(obj, NAME_cannotSaveObject, NAME_alienData);
	  rval = storeObject(NIL, file);
	}
      }
      save_nesting--;

      return rval;
    }
  }
}


static status
storeExtensionsObject(Any obj, FileObj file)
{ if ( onFlag(obj, F_CONSTRAINT|F_ATTRIBUTE|F_SENDMETHOD|F_GETMETHOD|
	           F_HYPER|F_RECOGNISER) )
  { if ( onFlag(obj, F_CONSTRAINT) )
    { storeCharFile(file, 'c');
      storeObject(getAllConstraintsObject(obj, ON), file);
    }
    if ( onFlag(obj, F_ATTRIBUTE) )
    { storeCharFile(file, 'a');
      storeObject(getAllAttributesObject(obj, ON), file);
    }
    if ( onFlag(obj, F_SENDMETHOD) )
    { storeCharFile(file, 's');
      storeObject(getAllSendMethodsObject(obj, ON), file);
    }
    if ( onFlag(obj, F_GETMETHOD) )
    { storeCharFile(file, 'g');
      storeObject(getAllGetMethodsObject(obj, ON), file);
    }
    if ( onFlag(obj, F_HYPER) )
    { Chain hypers = getAllHypersObject(obj, ON);
      Cell cell;

      for_cell(cell, hypers)
	candidateSaveRelation(cell->value);
    }
    if ( onFlag(obj, F_RECOGNISER) )
    { storeCharFile(file, 'r');
      storeObject(getAllRecognisersGraphical(obj, ON), file);
    }
  }

  return storeCharFile(file, 'x');
}


static status
storeSlotObject(Instance inst, Variable var, FileObj file)
{ int i = valInt(var->offset);
  Any val = inst->slots[i];

  if ( onDFlag(var, D_SAVE_NORMAL) )
    return storeObject(val, file);

  if ( onDFlag(var, D_SAVE_NIL) )
  { if ( isSavedObject(val) )
      return storeObject(val, file);
    if ( !saveNilRefTable )
      saveNilRefTable = createHashTable(toInt(32), OFF);
    appendHashTable(saveNilRefTable, inst, var);
    storeObject(NIL, file);
  } 

  succeed;
}


status
storeSlotsObject(Any obj, FileObj file)
{ Class class = classOfObject(obj);
  
  for_vector(class->instance_variables, Variable var,
	     storeSlotObject(obj, var, file));

  succeed;
}


static status
storeIdObject(Any obj, Int id, FileObj file)
{ Name name;

  if ( (name = getNameAssoc(obj)) )
  { storeCharFile(file, 'N');
    storeNameFile(file, name);
    succeed;
  }
  storeCharFile(file, 'I');
  storeIntFile(file, id);
  succeed;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StoreClass stores the instance layout, as  far as PCE  typed slots are
concerned.  Alien slots  are taken care  of by  specialised load/store
functions defined on the class itself.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Int
storeClass(Class class, FileObj file)
{ Int ref;

  if ( (ref = isSavedClass(class)) )
    return ref;

  ref = setSavedClass(class);
  storeCharFile(file, 'C');
  storeNameFile(file, class->name);
  storeIntFile(file, ref);
  storeIntFile(file, toInt(pceSlotsClass(class)));
  storeSlotsClass(class, file);

  return ref;
}


static int
pceSlotsClass(Class class)
{ int pce_slots = 0;
  int slots = valInt(class->slots);
  int n;

  for(n = 0; n<slots; n++)
    if ( isPceSlot(class, n) )
      pce_slots++;

  return pce_slots;
}


static status
storeSlotsClass(Class class, FileObj file)
{ for_vector(class->instance_variables, Variable var,
	     if ( var->type->kind != NAME_alien )
	       storeNameFile(file, var->name));

  succeed;
}

		/********************************
		*            LOADING            *
		*********************************/

struct classdef
{ Class	class;			/* current class structure */
  Name	class_name;		/* name of this class */
  int	slots;			/* number of saved slots */
  int	*offset;		/* array of slot offsets */
  Name  *name;			/* array of slot-names */
};

static HashTable savedClassTable;	/* table with saved classes */
static HashTable restoreTable;		/* restored objects table */
static Chain	 restoreMessages;	/* messages for restoration */

long
loadWord(FILE *fd)
{
#if defined(__i386__) && !defined(__WATCOMC__) /* correct for byte-order */
  union
  { ulong         l;
    unsigned char c[4];
  } cvrt;
  long rval;

  cvrt.l = getw(fd);
  rval = (cvrt.c[0] << 24) |
         (cvrt.c[1] << 16) |
	 (cvrt.c[2] << 8) |
	  cvrt.c[3];
  DEBUG(NAME_byteOrder, printf("loadWord(0x%lx) --> %ld\n", cvrt.l, rval));
  return rval;
#else
  return getw(fd);
#endif
}


char *
loadCharp(FILE *fd)
{ int size = loadWord(fd);
  char *s = alloc(size+1);

  fread(s, sizeof(char), size, fd);
  s[size] = '\0';

  return s;
}


static Name
loadName(FILE *fd)
{ char *s = loadCharp(fd);
  Name name = CtoName(s);

  free_string(s);

  return name;
}


void
restoreMessage(Any msg)
{ if ( !restoreMessages )
    restoreMessages = newObject(ClassChain, 0);

  appendChain(restoreMessages, msg);
}


static status
loadNilRef(FILE * fd)
{ Int classid  = toInt(loadWord(fd));
  Any r1       = loadNameObject(fd);
  int offset   = loadWord(fd);
  Any r2       = loadNameObject(fd);
  ClassDef def = getMemberHashTable(savedClassTable, classid);
  Instance f   = getMemberHashTable(restoreTable, r1);
  Any t        = getMemberHashTable(restoreTable, r2);

  if ( !def )
    return errorPce(LoadFile, NAME_noSavedClassDef, classid);
  if ( !f )
    return errorPce(LoadFile, NAME_referencedObjectNotLoaded, r1);
  if ( !t )
    return errorPce(LoadFile, NAME_referencedObjectNotLoaded, r2);
    
  if ( def->offset[offset] >= 0 )
  { DEBUG(NAME_save, printf("Restoring (nil)ref %s-%s --> %s\n",
			    pp(f), pp(def->name[offset]), pp(t)));
    assignField(f, &(f->slots[def->offset[offset]]), t);
  }
  /* else slot is gone; no problem I think */

  succeed;
}


Any
getObjectFile(FileObj f)
{ FILE *fd;
  Any result;

  TRY(send(f, NAME_kind, NAME_binary, 0) &&
      send(f, NAME_open, NAME_read, 0));

  LoadFile = f;				/* TBD: pass as argument */
  fd = f->fd;

  if ( !checkObjectFile(f) )
  { closeFile(f);
    errorPce(f, NAME_badFile, NAME_object);
    fail;
  }

  restoreVersion = loadWord(fd);
  if ( restoreVersion != SAVEVERSION )
    errorPce(f, NAME_newSaveVersion,
	     toInt(restoreVersion), toInt(SAVEVERSION));

  savedClassTable = createHashTable(toInt(128), OFF);
  restoreTable = createHashTable(toInt(256), ON);
  if ( restoreMessages )
    clearChain(restoreMessages);
  if ( (result = loadObject(fd)) )
    addCodeReference(result);
  if ( restoreVersion >= 13 )
  { char c;

    do
    { switch((c=getc(fd)))
      { case 's':			/* support (relation) objects */
	  if ( !loadObject(fd) )
	    fail;			/* TBD */
	  break;
	case 'n':
	  if ( !loadNilRef(fd) )
	    fail;
	  break;
	case 'x':
	  break;
	default:
	  errorPce(f, NAME_illegalCharacter, toInt(c), toInt(ftell(fd)));
	  fail;
      }
    } while( c != 'x' );
  }

  freeHashTable(restoreTable);
  freeHashTable(savedClassTable);
  closeFile(f);

  if ( result )
  { if ( restoreMessages )
    { Any msg;

      while((msg= getDeleteHeadChain(restoreMessages)))
	forwardCodev(msg, 0, NULL);
    }

    delCodeReference(result);
    pushAnswerObject(result);
  }

  LoadFile = NULL;

  answer(result);
}


static void
updateFlagsObject(Any obj)
{ if ( instanceOfObject(obj, ClassFunction) )
    setFlag(obj, F_ACTIVE);
}


static status
loadExtensionsObject(Instance obj, FILE *fd)
{ if ( restoreVersion <= 7 )
    succeed;				/* extensions in interceptor */

  for(;;)
  { char c;
    Any ext;

    if ( restoreVersion == 8 )
    { if ( (c=getc(fd)) != 'e' )
      { ungetc(c, fd);
	succeed;
      }
    }

    switch(c=getc(fd))
    { case 'x':
	succeed;
      case 'a':
	setFlag(obj, F_ATTRIBUTE);
	appendHashTable(ObjectAttributeTable, obj, ext = loadObject(fd));
	addRefObj(ext);
	break;
      case 'c':
	setFlag(obj, F_CONSTRAINT);
	appendHashTable(ObjectConstraintTable, obj, ext = loadObject(fd));
	addRefObj(ext);
	break;
      case 's':
	setFlag(obj, F_SENDMETHOD);
	appendHashTable(ObjectSendMethodTable, obj, ext = loadObject(fd));
	addRefObj(ext);
	break;
      case 'g':
	setFlag(obj, F_GETMETHOD);
	appendHashTable(ObjectGetMethodTable, obj, ext = loadObject(fd));
	addRefObj(ext);
	break;
      case 'r':
	setFlag(obj, F_RECOGNISER);
	appendHashTable(ObjectRecogniserTable, obj, ext = loadObject(fd));
	addRefObj(ext);
	break;
      case 'h':
	setFlag(obj, F_HYPER);
	appendHashTable(ObjectHyperTable, obj, ext = loadObject(fd));
	addRefObj(ext);
	break;
      default:
	errorPce(LoadFile, NAME_illegalCharacter, toInt(c), toInt(ftell(fd)));
	fail;
    }
  }
}


Any
loadObject(FILE *fd)
{ char c;
#ifndef O_RUNTIME
  long start = 0;
#endif

  DEBUG(NAME_save, start = ftell(fd));

  switch( c = getc(fd) )
  { case 'd':	return DEFAULT;
    case 'n':	return NIL;
    case 'a':	return ON;
    case 'u':	return OFF;
    case 'r':	return RECEIVER;
    case '1':	return Arg(1);
    case '2':	return Arg(2);
    case '3':	return Arg(3);
    case '4':	return Arg(4);
    case '5':	return Arg(5);
    case '6':	return Arg(6);
    case '7':	return Arg(7);
    case '8':	return Arg(8);
    case '9':	return Arg(9);
    case '0':	return Arg(10);
    case 'N':   return loadName(fd);
    case 'I':   return toInt(loadWord(fd));
    case 'R': { Any r;
		Any ref = loadNameObject(fd);

		if ( !(r = getMemberHashTable(restoreTable, ref)) )
		{ errorPce(LoadFile, NAME_referencedObjectNotLoaded, ref);
		  fail;;
		}
		return r;
	      }
    case 'A': { Any r;
		Name name = loadName(fd);

		if ( !(r = getObjectFromReferencePce(PCE, name)) )
		{ errorPce(NIL, NAME_noAssoc, name);
		  fail;
		}
		return r;
	      }		
    case 'C':	restoreClass(fd);
		if ( (c=getc(fd)) != 'O' )
		{ errorPce(LoadFile, NAME_illegalCharacter,
			   toInt(c), toInt(ftell(fd)));
		  fail;
		}
    case 'O': { ClassDef def;
		Int classid = toInt(loadWord(fd));
		Any name;

		if ( !(def = getMemberHashTable(savedClassTable, classid)) )
		{ errorPce(LoadFile, NAME_noSavedClassDef, classid);
		  fail;
		}

		name = loadNameObject(fd);
		if ( def->class )
		{ Instance obj = allocObject(def->class, FALSE);

		  if ( isName(name) )
		    newAssoc(name, obj);
		  addCodeReference(obj);

		  DEBUG(NAME_save, printf("Loading %s from %ld\n",
					  pp(obj), start));

		  appendHashTable(restoreTable, name, obj);
		  loadExtensionsObject(obj, fd);

		  if ( def->class->loadFunction != NULL )
		    (*def->class->loadFunction)(obj, fd, def);
		  else
		    loadSlotsObject(obj, fd, def);
		  updateFlagsObject(obj);
		
		  if ( SAVEVERSION != restoreVersion || PCEdebugging )
		    TRY(checkConvertedObject(obj, def));

		  createdClass(def->class, obj, NAME_loaded);

		  DEBUG(NAME_save, CheckObject(obj, OFF));
		  delCodeReference(obj);

		  return obj;
		} else			/* no class; load into sheet */
		{ int i;
		  Any slotValue;
		  Sheet sh = createObjectv(isName(name) ? name : (Name) NIL,
					   ClassSheet, 0, NULL);

		  valueSheet(sh, NAME_className, def->class_name);
		  DEBUG(NAME_save, printf("Loading %s from %ld\n",
					  pp(sh), start));
		  appendHashTable(restoreTable, name, sh);
		  loadExtensionsObject((Any) sh, fd);

		  for( i=0; i<def->slots; i++ )
		  { if ( (slotValue = loadObject(fd)) == FAIL )
		      fail;
		    valueSheet(sh, def->name[i], slotValue);
		  }

		  DEBUG(NAME_save, CheckObject(sh, OFF));
		  return sh;
		}
	      }
    case 'S':				/* lisp-symbol hack */
	{ char *name_string = loadCharp(fd);
	  char *package_string = loadCharp(fd);
	  Name name = CtoName(name_string);
	  Name package = CtoName(package_string);
	  Class symbol_class = getConvertClass(ClassClass, NAME_lispSymbol);
	  Any  symbol = newObject(symbol_class, name, package, 0);

	  free_string(name_string);
	  free_string(package_string);

	  return symbol;
	}
    
    default:  { long index;

		index = ftell(fd) - 1;
		errorPce(LoadFile, NAME_illegalCharacter,
			 toInt(c), toInt(index));
		fail;
	      }
  }
}


static Any
loadNameObject(FILE *fd)
{ char c;

  switch( c = getc(fd) )
  { case 'I':	return (Any) toInt(loadWord(fd));
    case 'N':	return (Any) loadName(fd);
    default:	ungetc(c, fd);
		errorPce(LoadFile, NAME_illegalCharacter,
			 toInt(getc(fd)), toInt(ftell(fd)));
		fail;
  }
}


static status
restoreClass(FILE *fd)
{ Name name = loadName(fd);
  Int classid = toInt(loadWord(fd));
  int slots = loadWord(fd);
  int i;
  ClassDef def;

  if ( restoreVersion == 1 )
    slots++;

  def = alloc(sizeof(struct classdef));
  def->class_name = name;
  def->offset = alloc(slots * sizeof(int));
  def->name = alloc(slots * sizeof(Name));

  if ( (def->class = checkType(name, TypeClass, NIL)) )
    realiseClass(def->class);
  else
    printf("\n\tUnknown class: %s ...", pp(name));
  def->slots = slots;
  appendHashTable(savedClassTable, classid, def);
  
  for( i = 0; i<slots; i++ )
  { Name name = loadName(fd);

    def->name[i] = name;
    if ( def->class )
    { def->offset[i] = offsetVariable(def->class, name);
      if ( def->offset[i] < 0 )
      { printf("\n\tSlot %s of class %s out of use ...",
	       pp(name), pp(def->class));
	fflush(stdout);
      }
    }
  }

  succeed;
}


static status
definedSlotClassDef(ClassDef def, Name slot)
{ int i;

  for(i=0; i<def->slots; i++)
    if ( def->name[i] == slot )
      succeed;

  fail;
}


static int
offsetVariable(Class class, Name name)
{ Variable var;
  
  if ( (var = getInstanceVariableClass(class, name)) &&
       var->type->kind != NAME_alien )
    return valInt(var->offset);

  return -1;
}


status
loadSlotsObject(Any obj, FILE *fd, ClassDef def)
{ int i;
  Any slotValue;
  Instance inst = obj;

  for( i=0; i<def->slots; i++ )
  { int slot;

    if ( (slotValue = loadObject(fd)) == FAIL )
      fail;
    if ( (slot = def->offset[i]) < 0 )	/* slot out of use */
    { if ( hasSendMethodObject(inst, NAME_convertOldSlot) )
	send(inst, NAME_convertOldSlot, def->name[i], slotValue, 0);
      continue;
    }
    if ( restoreVersion != SAVEVERSION || PCEdebugging )
    { Any converted;
      Variable var = def->class->instance_variables->elements[slot];

      if ( !(converted = checkType(slotValue, var->type, inst)) &&
	   !(isNil(slotValue) && onDFlag(var, D_SAVE_NIL)) )
	errorPce(obj, NAME_failedToConvert, slotValue, var->name);
      else
	slotValue = converted;
    }
    assignField(inst, &(inst->slots[slot]), slotValue);
  }

  succeed;
}


static status
checkConvertedObject(Any obj, ClassDef def)
{ Class class = def->class;
  int slots = valInt(class->slots);
  Instance inst = obj;
  int i;

  if ( hasSendMethodObject(inst, NAME_convertLoadedObject) )
    send(inst, NAME_convertLoadedObject,
	 toInt(restoreVersion),
	 toInt(SAVEVERSION), 0);

  for(i=0; i<slots; i++)
  { if ( isPceSlot(class, i) )
    { Variable var = getInstanceVariableClass(class, toInt(i));
      Any value = inst->slots[i];

      if ( var == FAIL )
      { printf("Can't find variable %d of %s\n", i, pp(class));
	continue;
      }

      if ( isDefault(value) && getResourceClass(class, var->name) )
	continue;

      if ( hasSendMethodObject(inst, NAME_initialiseNewSlot) &&
	   !definedSlotClassDef(def, var->name) )
	send(inst, NAME_initialiseNewSlot, var, 0);
      value = inst->slots[i];

      if ( !checkType(value, var->type, inst) &&
	   !(isNil(value) && onDFlag(var, D_SAVE_NIL)) )
	errorPce(inst, NAME_badSlotValue, var->name, value);
    }
  }

  succeed;
}
