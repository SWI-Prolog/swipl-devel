/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: load and save intermediate code files
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"

forwards char *	getString(FILE *);
forwards long	getNum(FILE *);
forwards real	getReal(FILE *);
forwards bool	loadWicFd(char *, FILE *, bool, bool);
forwards bool	loadPredicate(FILE *);
forwards bool	loadExport(FILE *);
forwards bool	loadImport(FILE *);
forwards void	putString(char *, FILE *);
forwards void	putAtom(Atom, FILE *);
forwards void	putNum(long, FILE *);
forwards void	putReal(real, FILE *);
forwards void	saveWicClause(Clause, FILE *);
forwards void	closeProcedureWic(void);
forwards void	checkSource(Atom);
forwards bool	openWic(char *);
forwards bool	closeWic(void);
forwards bool	addClauseWic(Word, Atom);
forwards bool	addDirectiveWic(word);
forwards bool	startModuleWic(Atom, SourceFile);
forwards bool	exportWic(Atom, int);
forwards bool	importWic(Atom, Atom, int);
forwards word	directiveClause(word, char *);
forwards bool	compileFile(char *);
forwards bool	putStates(FILE *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog can compile Prolog source files into intermediate code files, 
which can be loaded very  fast.   They  can  be  saved  as  stand  alone
executables using Unix #! magic number.

A wic file consists of the magic code and a version check code.  This is
followed by the command line option defaults.  Then an  optional  series
of  `include'  statements follow.  Finally the predicates and directives
are  described.   Predicates  are  described  close  to   the   internal
representation.  Directives are described as Prolog source.

The default options and include statements are written incrementally  in
each  wic  file.   In  the  normal  boot  cycle  first  the boot file is
determined.  Then the option structure is filled with the default option
found in this boot file.  Next the command line arguments are scanned to
obtain all options.  Then stacks, built  in's,  etc.   are  initialised.
The  the  boot  file is read again, but now only scanning for directives
and predicates.

IF YOU CHANGE ANYTHING TO THIS FILE, SO THAT OLD WIC-FILES CAN NO LONGER
BE READ, PLEASE DO NOT FORGET TO INCREMENT THE VERSION NUMBER!

Below is an informal description of the format of a `wic' file:

<wic-file>	::=	#!<path>
			<magic code>
			<version number>
			<localSize>			% a <word>
			<globalSize>			% a <word>
			<trailSize>			% a <word>
			<argumentSize>			% a <word>
			<lockSize>			% a <word>
			<goal>				% a <string>
			<topLevel>			% a <string>
			<initFile>			% a <string>
			{<statement>}
<magic code>	::=	<string>			% normally #!<path>
<version number>::=	<word>
<statement>	::=	'W' <string>			% include wic file
		      | 'P' <num> <string>
			    {<clause>} <pattern>	% predicate
		      | 'D' <string>			% directive
		      | 'F' <string> <system> <time>	% source file
		      | 'M' <string> <string>		% start module in file
		      | 'E' <num> <string>		% export predicate
		      | 'I' <string> <num> <string>	% import predicate
<clause>	::=	'C' <line_no> <n var> <n clause> <externals> <codes>	% clause
		      | 'X' 				% end of list
<externals>	::=	<num> {<external>}
<external>	::=	'a' <string>			% atom
			'f' <num> <string>		% functor
			'p' <num> <string>		% predicate
			'e' <string> <num> <string>	% extern predicate
			'n' <word>			% number
			'r' <word>			% real (float)
			's' <string>			% string
<system>	::=	's'				% system source file
		      | 'u'				% user source file
<time>		::=	<word>				% time file was loaded
<pattern>	::=	<word>				% indexing pattern
<codes>		::=	<num> {<code>}
<string>	::=	{<non-zero byte>} <0>
<word>		::=	<4 byte entity>

Numbers are stored in  a  packed  format  to  reduce  the  size  of  the
intermediate  code  file  as  99%  of  them  is  normally  small, but in
principle not limited (virtual  machine  codes,  arities,  table  sizes,
etc).   The  upper  two  bits  of  the  first byte contain the number of
additional bytes.  the bytes represent the number `most-significant part
first'.  See the functions putNum() and getNum()  for  details.   Before
you  don't  agree  to  this  schema,  you  should  remember it makes the
intermediate code files about 30% smaller  and  avoids  the  differences
between  16  and  32  bits  machines (arities on 16 bits machines are 16
bits) as well as machines with different byte order.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define VERSION 12			/* save version number */

static char saveMagic[] = "SWI-Prolog (c) 1990 Jan Wielemaker\n";
static char *wicFile;			/* name of output file */
static FILE *wicFd;			/* file descriptor of wic file */
static Procedure currentProc;		/* current procedure */
static SourceFile currentSource;	/* current source file */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On tos, loading takes long; give the user  something  to  look  at.   On
workstations, it normally is so fast it is hardy noticable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if tos
forwards void	notifyLoad(char *file);
forwards void	notifyLoaded(void);
forwards void	notifyPredicate(char *name, int arity);

static void
notifyLoad(file)
char *file;
{ printf("Loading %s ", file);
  fflush(stdout);
}

static void
notifyLoaded()
{ printf("\r\033K");
}

static void
notifyPredicate(name, arity)
char *name;
int arity;
{ static char cur[] = "|/-\\";
  static int  n = 0;

  printf("%c\b", cur[n++ & 0x3]);
}

#else /*!tos*/

#define notifyLoad(file)
#define notifyLoaded()
#define notifyPredicate(name, arity)

#endif /* tos */

static char *
getString(FILE *fd)
{ static char *tmp;
  static char *tmpend;
  static int  tmpsize = 512;
  char *s;
  Char c;

  if ( tmp == NULL )
  { tmp    = malloc(tmpsize);
    tmpend = &tmp[tmpsize-1];
  }

  for( s = tmp; (c = Getc(fd)) != EOS, *s = c; s++ )
  { if ( s == tmpend )
    { tmp = realloc(tmp, tmpsize+512);
      s = &tmp[tmpsize-1];
      tmpsize += 512;
      tmpend = &tmp[tmpsize-1];
    }
    if ( c == EOF )
      fatalError("Unexpected EOF on intermediate code file at offset %d",
		 ftell(fd));
  }

  return tmp;
}

static long
getNum(FILE *fd)
{ long first = Getc(fd);
  int bytes, shift, b;

  if ( (bytes = (int) ((first >> 6) & 0x3)) == 0 )
    return (first << 26) >> 26;		/* 99% of them: speed up a bit */

  first &= 0x3f;
  for( b = 0; b < bytes; b++ )
  { first <<= 8;
    first |= Getc(fd) & 0xff;
  }

  shift = (3-bytes)*8 + 2;

  return (first << shift) >> shift;
}

static real
getReal(FILE *fd)
{ real f;
  char *s = (char *)&f;
  int n;

  for(n=0; n<sizeof(f); n++)
    *s++ = Getc(fd);

  return f;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load a complete `wic' file.  `toplevel' tells  us  whether  we  are  the
toplevel  file  opened,  and thus should include other `wic' files or we
should ignore the include statements.  `load_options' tells us  to  only
load the options of the toplevel file.

All wic files loaded are appended in the  right  order  to  a  chain  of
`states'.  They are written to a new toplevel wic file by openWic().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
loadWicFile(char *file, bool toplevel, bool load_options)
{ FILE *fd;
  bool rval = TRUE;

  if ((fd = Fopen(file, STREAM_OPEN_BIN_READ)) == (FILE *) NULL)
  { fatalError("Can't open %s: %s", file, OsError());
    rval = FALSE;
    goto out;
  }

  notifyLoad(file);

  if (loadWicFd(file, fd, toplevel, load_options) == FALSE)
  { rval = FALSE;
    goto out;
  }
  if (toplevel == TRUE && load_options == FALSE)
  { if (appendState(file) == FALSE)
    { rval = FALSE;
      goto out;
    }
  }

out:
  if (fd != (FILE *) NULL)
    fclose(fd);

  notifyLoaded();

  return rval;
}


static bool
loadWicFd(char *file, FILE *fd, bool toplevel, bool load_options)
{ char *s;
  Char c;
  int n;

#if OS2
  for(n=0; n<5; n++)                    /* skip first five lines */
#else
  for(n=0; n<2; n++)			/* skip first two lines */
#endif
  { while( (c=(Char)Getc(fd)) != '\n' && c != EOF ) ;
    if ( c == EOF )
      return fatalError("%s is not a SWI-Prolog intermediate code file", file);
  }

  s = getString(fd);
  if (!streq(s, saveMagic) )
    return fatalError("%s is not a SWI-Prolog intermediate code file", file);

  if (getNum(fd) != VERSION)
  { fatalError("Intermediate code file %s has incompatible save version",
	       file);
    fail;
  }

  if (load_options && toplevel)
  { options.localSize    = getNum(fd);
    options.globalSize   = getNum(fd);
    options.trailSize    = getNum(fd);
    options.argumentSize = getNum(fd);
    options.lockSize	 = getNum(fd);
    DEBUG(2, printf("local=%ld, global=%ld, trail=%ld, argument=%ld\n",
		options.localSize, options.globalSize,
		options.trailSize, options.argumentSize));
    options.goal         = store_string(getString(fd) );
    options.topLevel     = store_string(getString(fd) );
    options.initFile     = store_string(getString(fd) );

    succeed;
  } else
  { int n;
    for(n=0; n<5; n++)   getNum(fd);
    for(n=0; n<3; n++)   getString(fd);
  }

  for(;;)
  { switch(c=Getc(fd) )
    { case EOF:
	succeed;
      case 'W':
	{ char *name;

	  name = store_string(getString(fd) );
	  if (toplevel == TRUE)
	  { appendState(name);
	    loadWicFile(name, FALSE, FALSE);
	  }
	  continue;
	}
      case 'P':
	{ loadPredicate(fd);
	  continue;
	}
      case 'D':
	{ Word directive;
	  word rval;
	  mark m;

	  Mark(m);
	  directive = newTerm();
	  s = getString(fd);
	  seeString(s);
	  rval = pl_read(directive);
	  seenString();
	  if (rval == TRUE)
	  { environment_frame = (LocalFrame) NULL;
	    if (callGoal(MODULE_user, *directive, FALSE) == FALSE)
	    { printf("[WARNING: directive failed: ");
	      pl_write(directive);
	      printf("]\n");
	    }
	  }
	  Undo(m);

	  continue;
	}	  
      case 'F':
	{ currentSource = lookupSourceFile(lookupAtom(getString(fd)));
	  currentSource->system = (Getc(fd) == 's' ? TRUE : FALSE);
	  currentSource->time = Getw(fd);
	  continue;
	}
      case 'M':
	{ char *file;

	  modules.source = lookupModule(lookupAtom(getString(fd)));
	  file = getString(fd);
	  if (!streq(file, "-") )
	    modules.source->file = lookupSourceFile(lookupAtom(file));
	  continue;
	}
      case 'E':
	{ loadExport(fd);
	  continue;
	}
      case 'I':
	{ loadImport(fd);
	  continue;
	}
      default:
	sysError("Illegal statement in wic file: %d", c);
    }
  }
}

static bool
loadPredicate(FILE *fd)
{ int arity, n;
  char *name;
  Procedure proc;
  Definition def;
  Clause clause;
  Word xp;
  Code bp;

  arity = (int) getNum(fd);
  if ((name = getString(fd)) == (char *) NULL)
    sysError("bad string in wic file");
  notifyPredicate(name, arity);
  proc = lookupProcedure(lookupFunctorDef(lookupAtom(name), arity), 
			  modules.source);
  def = proc->definition;
  if ( SYSTEM_MODE )
  { set(def, SYSTEM|HIDE_CHILDS|LOCKED);
  }

  for(;;)
  { switch(Getc(fd) )
    { case 'X':
      { unsigned long pattern = Getw(fd);

	if ( def->indexPattern != pattern )
	{ def->indexPattern = pattern;
	  def->indexCardinality = cardinalityPattern(def->indexPattern);
	  if ( pattern != 0x1 )
	  reindexProcedure(proc);
	}

	succeed;
      }
      case 'C':
	clause = (Clause) allocHeap(sizeof(struct clause));
	clause->line_no = getNum(fd);
	clause->next = (Clause) NULL;
	clause->references = 0;
	clearFlags(clause);
	clause->variables = getNum(fd);
	clause->subclauses = getNum(fd);
	clause->procedure = proc;
	clause->source_no = currentSource->index;

	clause->XR_size = getNum(fd);
	statistics.externals += clause->XR_size;
	if ( clause->XR_size > 0 )
	{ clause->externals = (Word) allocHeap(clause->XR_size * sizeof(word));
	  xp = clause->externals;
	  for(n=0; n<clause->XR_size; n++, xp++)
	  { char c;

	    switch( c=Getc(fd) )
	    { case 'a':
		*xp = (word)lookupAtom(getString(fd) );
		continue;
#if O_STRING
	      case 's':
		*xp = heapString(getString(fd));
		continue;
#endif /* O_STRING */
	      case 'f':
		arity = (int) getNum(fd);
		name = getString(fd);
		*xp = (word)lookupFunctorDef(lookupAtom(name), arity);
		continue;
	      case 'p':
		arity = (int) getNum(fd);
		name = getString(fd);
		*xp = (word)lookupProcedure(lookupFunctorDef(lookupAtom(name), arity), 
					    modules.source);
		continue;
	      case 'e':
		{ Module module = lookupModule(lookupAtom(getString(fd) ));

		  arity = (int) getNum(fd);
		  name = getString(fd);
		  *xp = (word)lookupProcedure(lookupFunctorDef(lookupAtom(name), arity), 
					      module);
		  continue;
		}
	      case 'n':
		*xp = Getw(fd);
		continue;
	      case 'r':
		*xp = heapReal(getReal(fd));
		continue;
	      default:
		sysError("%s (char. index 0%lo): illegal XR entry: %c",
					procedureName(proc), ftell(fd), c);
	    }
	  }
	} else	      
	  clause->externals = NULL;
	clause->code_size = getNum(fd);
	statistics.codes += clause->code_size;
	clause->codes = (Code) allocHeap(clause->code_size * sizeof(code));
	bp = clause->codes;
#if VMCODE_IS_ADDRESS
	while( bp < &clause->codes[clause->code_size] )
	{ code op = getNum(fd);
	  int n;
	  
	  *bp++ = encode(op);
	  for(n = codeTable[op].arguments; n > 0; n--)
	    *bp++ = getNum(fd);
	}
#else
	for(n=0; n<clause->code_size; n++)
	  *bp++ = getNum(fd);
#endif

	assertProcedure(proc, clause, 'z');
	reindexClause(clause);
    }
  }
}

static bool
loadExport(FILE *fd)
{ int arity =  (int) getNum(fd);
  char *name = getString(fd);
  FunctorDef functor = lookupFunctorDef(lookupAtom(name), arity);
  Procedure proc = lookupProcedure(functor, modules.source);

  addHTable(modules.source->public, functor, proc);

  succeed;
}
  
static bool
loadImport(FILE *fd)
{ Module source = lookupModule(lookupAtom(getString(fd) ));
  int arity = (int) getNum(fd);
  char *name = getString(fd);
  FunctorDef functor = lookupFunctorDef(lookupAtom(name), arity);
  Procedure proc = lookupProcedure(functor, source);
  Procedure old;

  DEBUG(2, printf("loadImport(): %s/%d into %s\n",
		  name, arity, stringAtom(modules.source->name)));

  if ((old = isCurrentProcedure(functor, modules.source)) != (Procedure) NULL)
  { if ( old->definition == proc->definition )
      succeed;			/* already done this! */

    if (!isDefinedProcedure(old) )
    { old->definition = proc->definition;
      succeed;
    }

    return warning("Failed to import %s into %s", 
		   procedureName(proc), 
		   stringAtom(modules.source->name) );
  }
  addHTable(modules.source->procedures, functor, proc);

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code below handles the creation of `wic' files.  It offers a  number
of  predicates  which  enables  us  to write the compilation toplevel in
Prolog.

Note that we keep track of the `current procedure' to keep  all  clauses
of a predicate together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
putString(register char *s, FILE *fd)
{ while(*s)
  { Putc(*s, fd);
    s++;
  }

  Putc(EOS, fd);
}

static void
putAtom(Atom a, FILE *fd)
{ putString(a->name, fd);
}

static void
putNum(long int n, FILE *fd)
{ long m = n > 0 ? n : n - 1;

  if ( m < (1L << 5) )
  { Putc((char) (n & 0x3f), fd);
    return;
  }
  if ( m < (1L << 13) )
  { Putc((char) (((n >> 8) & 0x3f) | (1 << 6)), fd);
    Putc((char) (n & 0xff), fd);
    return;
  }
  if ( m < (1L << 21) )
  { Putc((char) (((n >> 16) & 0x3f) | (2 << 6)), fd);
    Putc((char) ((n >> 8) & 0xff), fd);
    Putc((char) (n & 0xff), fd);
    return;
  }
  if ( m < (1L << 29) )
  { Putc((char) (((n >> 24) & 0x3f) | (3 << 6)), fd);
    Putc((char) ((n >> 16) & 0xff), fd);
    Putc((char) ((n >> 8) & 0xff), fd);
    Putc((char) (n & 0xff), fd);
    return;
  }
  sysError("Argument to putNum() out of range: %ld", n);
}

static void
putReal(real f, FILE *fd)
{ char *s = (char *)&f;
  int n;

  for(n=0; n < sizeof(f); n++)
    Putc(*s++, fd);
}


static void
saveWicClause(Clause clause, FILE *fd)
{ Word xp;
  word xr;
  Code bp;
  int n;

  Putc('C', fd);
  putNum(clause->line_no, fd);
  putNum(clause->variables, fd);
  putNum(clause->subclauses, fd);
  putNum(clause->XR_size, fd);
  xp = clause->externals;
  for(n=0; n<clause->XR_size; n++, xp++)
  { xr = *xp;
    if (isAtom(xr) )
    { Putc('a', fd);
      putAtom((Atom)xr, fd);
    } else if (isInteger(xr) )
    { Putc('n', fd);
      Putw(xr, fd);
    } else if (isReal(xr) )
    { Putc('r', fd);
      putReal(valReal(xr), fd);
#if O_STRING
    } else if ( isString(xr) )
    { Putc('s', fd);
      putString(valString(xr), fd);
#endif /* O_STRING */
    } else if (((FunctorDef)xr)->type == FUNCTOR_TYPE)
    { Putc('f', fd);
      putNum(((FunctorDef)xr)->arity, fd);
      putAtom(((FunctorDef)xr)->name, fd);
    } else
    { if (((Procedure)xr)->definition->module != modules.source)
      { Putc('e', fd);
	putAtom(((Procedure)xr)->definition->module->name, fd);
      } else
      { Putc('p', fd);
      }
      putNum(((Procedure)xr)->functor->arity, fd);
      putAtom(((Procedure)xr)->functor->name, fd);
    }
  }

  putNum(clause->code_size, fd);
  bp = clause->codes;
#if VMCODE_IS_ADDRESS
  while( bp < &clause->codes[clause->code_size] )
  { code op = decode(*bp++);
    int n;

    putNum(op, fd);
    for(n = codeTable[op].arguments; n > 0; n--)
      putNum(*bp++, fd);
  }
#else
  for(n=0; n<clause->code_size; n++, bp++)
    putNum(*bp, fd);
#endif
}


		/********************************
		*         COMPILATION           *
		*********************************/

static void
closeProcedureWic()
{ if (currentProc != (Procedure) NULL)
  { Putc('X', wicFd);
    Putw(currentProc->definition->indexPattern, wicFd);
    currentProc = (Procedure) NULL;
  }
}

static void
checkSource(Atom file)
{ SourceFile sf = lookupSourceFile(file);

  if (sf != currentSource)
  { currentSource = sf;
    Putc('F', wicFd);
    putAtom(file, wicFd);
    Putc(sf->system ? 's' : 'u', wicFd);
    Putw(sf->time, wicFd);
  }
}

static bool
openWic(char *file)
{ char *exec;

  wicFile = file;

  DEBUG(1, printf("Open compiler output file %s\n", file));
  if ( (wicFd = Fopen(file, STREAM_OPEN_BIN_WRITE)) == (FILE *)NULL )
    return warning("Can't open %s: %s", file, OsError());
  DEBUG(1, printf("Searching for executable\n"));
  if ( loaderstatus.restored_state )
  { exec = stringAtom(loaderstatus.restored_state);
  } else
  { TRY( getSymbols() );
    exec = stringAtom(loaderstatus.orgsymbolfile);
  }
  DEBUG(1, printf("Executable = %s\n", exec));
  if ( !(exec = OsPath(AbsoluteFile(exec))) )
    fail;
  DEBUG(1, printf("Expanded executable = %s\n", exec));
/*fprintf(wicFd, "#!%s -x\n", exec);*/
#if OS2
  fprintf(wicFd, "/* Compiled SWI-Prolog Program */\r\n'@ECHO OFF'\r\nparse source . . name\r\n\"%s -x \" name arg(1)\r\nexit\r\n", exec);
#else
  fprintf(wicFd, "#!/bin/sh\nexec %s -x $0 $*\n", exec);
#endif /* OS2 */
  DEBUG(2, printf("Magic  ...\n"));
  putString( saveMagic,            wicFd);
  DEBUG(2, printf("Numeric options ...\n"));
  putNum(    VERSION,              wicFd);
  putNum(    options.localSize,    wicFd);
  putNum(    options.globalSize,   wicFd);
  putNum(    options.trailSize,    wicFd);
  putNum(    options.argumentSize, wicFd);
  putNum(    options.lockSize,     wicFd);
  DEBUG(2, printf("String options ...\n"));
  putString(options.goal,          wicFd);
  putString(options.topLevel,      wicFd);
  putString(options.initFile, 	   wicFd);

  DEBUG(2, printf("States ...\n"));
  putStates(wicFd);
  currentProc = (Procedure) NULL;
  currentSource = (SourceFile) NULL;

  DEBUG(2, printf("Header complete ...\n"));
  succeed;
}  

static bool
closeWic()
{ if (wicFd == (FILE *) NULL)
    fail;
  closeProcedureWic();
  fclose(wicFd);
  return MarkExecutable(wicFile);
}

static bool
addClauseWic(Word term, Atom file)
{ Clause clause;

  if ((clause = assert_term(term, 'z', file)) != (Clause)NULL)
  { if (clause->procedure != currentProc)
    { closeProcedureWic();
      checkSource(file);
      currentProc = clause->procedure;
      Putc('P', wicFd);
      putNum(currentProc->functor->arity, wicFd);
      putAtom(currentProc->functor->name, wicFd);
    }
    saveWicClause(clause, wicFd);
    succeed;
  }

  fail;
}

static bool
addDirectiveWic(word term)
{ char *s = (char *)lTop;
#if !O_DYNAMIC_STACKS
  long n = (char *)lMax - (char *)lTop;

  tellString(s, n-2);
#else
  tellString(s, 1000000);
#endif
  TRY(pl_writeq(&term) );
  toldString();
  strcat(s, ". ");

  closeProcedureWic();
  Putc('D', wicFd);
  putString(s, wicFd);

  succeed;
}  

static bool
startModuleWic(Atom name, SourceFile file)
{ closeProcedureWic();

  Putc('M', wicFd);
  putAtom(name, wicFd);
  if (file != (SourceFile) NULL)
    putAtom(file->name, wicFd);
  else
    putString("-", wicFd);

  succeed;
}

static bool
exportWic(Atom name, int arity)
{ closeProcedureWic();

  Putc('E', wicFd);
  putNum(arity, wicFd);
  putAtom(name, wicFd);

  succeed;
}

static bool
importWic(Atom module, Atom name, int arity)
{ closeProcedureWic();

  Putc('I', wicFd);
  putAtom(module, wicFd);
  putNum(arity, wicFd);
  putAtom(name, wicFd);

  succeed;
}

		/********************************
		*        PROLOG SUPPORT         *
		*********************************/

word
pl_open_wic(Word name)
{ if (!isAtom(*name) )
    fail;

  return openWic(stringAtom(*name));
}

word
pl_close_wic(void)
{ return closeWic();
}

word
pl_add_clause_wic(Word term, Word file)
{ if (isVar(*term) || !isAtom(*file) )
    return warning("$add_clause_wic/2: instantiation fault");

  return addClauseWic(term, (Atom)*file);
}

word
pl_add_directive_wic(Word term)
{ if (isVar(*term) )
    return warning("$add_directive_wic/1: directive is a variable");

  return addDirectiveWic(*term);
}

word
pl_start_module_wic(Word term, Word file)
{ if (!isAtom(*term) || (!isAtom(*file) && !isInteger(*file)))
    return warning("$start_module_wic/1: instantiation fault");

  return startModuleWic((Atom)*term,
			isInteger(*file) ? (SourceFile) NULL
					 : lookupSourceFile((Atom)*file));
}

word
pl_export_wic(Word name, Word arity)
{ if (!isAtom(*name) || !isInteger(*arity) )
    return warning("$export_wic/2: instantiation fault");

  return exportWic((Atom)*name, (int)valNum(*arity));
}

word
pl_import_wic(Word module, Word name, Word arity)
{ if (!isAtom(*module) || !isAtom(*name) || !isInteger(*arity) )
    return warning("$import_wic/3: instantiation fault");

  return importWic((Atom)*module, (Atom)*name, (int) valNum(*arity));
}


		/********************************
		*     BOOTSTRAP COMPILATION     *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code below offers a restricted compilation  toplevel  used  for  the
bootstrap  compilation  (-b  option).  It handles most things the Prolog
defined compiler handles as well, except:

  - Be carefull to define  a  predicate  first  before  using  it  as  a
    directive
  - It does not offer `consult', `ensure_loaded' or the  list  notation.
    (there is no way to include other files).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*  Check if a clause is of the for ":- directive". If not return NULL, 
    otherwise return the argument.

 ** Wed Jun  8 16:12:39 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static word
directiveClause(word clause, char *functor)
{ if (!isTerm(clause) )
    return (word) NULL;
  if (functorTerm(clause)->arity == 1 &&
       streq(functorTerm(clause)->name->name, functor) )
  { word d;

    d = argTerm(clause, 0);
    if (isVar(d) )
      return (word) NULL;

    if ( !isTerm(d) || functorTerm(d) != FUNCTOR_module2 )
    { word directive;
      
      directive = globalFunctor(FUNCTOR_module2);
      argTerm(directive, 0) = (word) modules.source->name;
      argTerm(directive, 1) = d;

      return directive;
    }

    return d;
  } else
    return (word) NULL;
}

/*  Compile an entire file into intermediate code.

 ** Thu Apr 28 13:44:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
compileFile(char *file)
{ char *path;
  word f;
  Word term = newTerm();

  DEBUG(1, printf("Boot compilation of %s\n", file));
  if ((path = AbsoluteFile(file)) == (char *) NULL)
    fail;
  DEBUG(2, printf("Expanded to %s\n", path));

  f = (word) lookupAtom(path);
  DEBUG(2, printf("Opening\n"));
  if (pl_see(&f) == FALSE)
    fail;
  DEBUG(2, printf("pl_start_consult()\n"));
  pl_start_consult(&f);
  
  for(;;)
  { word directive;
    mark m;
    
    Mark(m);
    DEBUG(2, printf("pl_read_clause() -> "));
    if (pl_read_clause(term) == FALSE)
      continue;
    DEBUG(2, pl_write(term); pl_nl());
    if (*term == (word) ATOM_end_of_file)
      break;
    if ((directive = directiveClause(*term, ":-")) != (word) NULL)
    { environment_frame = (LocalFrame) NULL;
      DEBUG(1, Putf(":- "); pl_write(&directive); Putf(".\n") );
      callGoal(MODULE_user, directive, FALSE);
      addDirectiveWic(directive);
    } else if ((directive = directiveClause(*term, "$:-")) != (word) NULL)
    { environment_frame = (LocalFrame) NULL;
      DEBUG(1, Putf("$:- "); pl_write(&directive); Putf(".\n") );
      callGoal(MODULE_user, directive, FALSE);
    } else
      addClauseWic(term, (Atom)f);
    Undo(m);
  }
  pl_seen();

  succeed;
}

bool
compileFileList(char *out, int argc, char **argv)
{ newOp("$:-", OP_FX, 1200);
  TRY(openWic(out) );
  
  for(;argc > 0; argc--, argv++)
  { if (streq(argv[0], "-c") )
      break;
    compileFile(argv[0]);
  }

  return closeWic();
}


		/********************************
		*         STATE LISTS           *
		*********************************/

/*  Add a new state to the chain of states this Prolog session is build
    from. The file name is made absolute to avoid directory problems
    with incremental loading.
*/

bool
appendState(char *name)
{ State state, st;
  char *absolute;

  if ((absolute = AbsoluteFile(name)) == (char *) NULL)
    return warning("invalid file specification: %s", name);

  state = (State) allocHeap(sizeof(struct state) );
  state->next = (State) NULL;
  state->name = store_string(absolute);

  if (stateList == (State) NULL)
  { stateList = state;
    succeed;
  }
  for(st = stateList; st->next; st = st->next) ;
  st->next = state;

  succeed;
}

/*  Add 'W' statements to the WIC file for each file in the state list.
*/

static bool
putStates(FILE *fd)
{ State st;

  for(st = stateList; st; st = st->next)
  { Putc('W', fd);
    putString(st->name, fd);
  }

  succeed;
}
