/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: load foreign files
*/

/*  Modified (M) 1993 Dave Sherratt  */
/*  Implementing foreign functions for HP-PA RISC architecture  */

#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Make sure the symbolfile and  orgsymbolfile  attributes  of  the  global
structure status are filled properly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
getSymbols(void)
{ char *symbols, *abs_symbols;

  if ( loaderstatus.symbolfile != (Atom) NULL )
    succeed;
  
  if ( (symbols = Symbols()) == (char *)NULL )
  { Putf("[WARNING: Failed to find symbol table. Trying %s]\n", mainArgv[0]);
    symbols = mainArgv[0];
  }
  DEBUG(2, Sdprintf("Symbol file = %s\n", symbols));
  if ( (abs_symbols = AbsoluteFile(symbols)) == NULL )
    fail;

  loaderstatus.symbolfile = loaderstatus.orgsymbolfile
			  = lookupAtom(abs_symbols);
  setFeature(lookupAtom("symbol_file"), (word) loaderstatus.symbolfile);

  succeed;
}

#if O_FOREIGN

forwards bool create_a_out(char *files, char *entry,
			   char *options, char *libraries,
			   long int base, char *outfile);
forwards int  openExec(char *execFile);
forwards int  sizeExec(void);
forwards Func loadExec(int fd, unsigned long base, char *sentry);
#if O_NOENTRY
forwards bool scanSymbols();
forwards char *symbolString();
#endif

#include <sys/file.h>
#include <a.out.h>
#include <unistd.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>			/* valloc() prototype */
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load an object file and link it to the system.  The intented  schema  is
to  call  the  standard  system  loader `ld' to proceduce an incremental
executable starting at some specified address.  As we only need 1  entry
point  (the foreign module's initialisation function) we call the loader
with -e <function> which will make the loader put the  address  of  that
function in the header of the executable, thus avoiding the need to scan
the  symbol table.  With the new dynamic linking facilities of SunOs 4.0
this appears not to work any more.  Therefore a NOENTRY  flag  has  been
introduced  to  indicate that `-e' does not work properly and the symbol
table is to be scanned for the entry point.

If the size of the executable is not provided by the user, we first make
an executable for an arbitrary base address (0) to deterimine the  size.
Next  we  allocate  memory  and  produce  an  executable to start at the
allocated memory base.  Finally, we read the text and  initialised  data
segment  from  the  executable,  clear  the  bss area and call the entry
point.

Normally, the entry point will install foreign language  functions,  but
the user is allowed to do anything (s)he likes (even take over control).

This module is a bit of a mess due to all the #ifdef.  We should  define
a better common basis to get rid of most of these things.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if hpux
#  ifdef TEXT_OFFSET  /* a.out_300 */
#    define N_DATOFF(x)       DATA_OFFSET(x)
#    define N_TXTOFF(x)       TEXT_OFFSET(x)
#  else                       /* a.out_800 */
#    define aout_800 1
#    define N_TXTOFF(x) ((x).exec_tfile)
#    define N_DATOFF(x) ((x).exec_dfile)
#    define aouthdr som_exec_auxhdr
#    define filehdr header
#    define tsize exec_tsize
#    define dsize exec_dsize
#    define bsize exec_bsize
#    define LD_O_OPTIONS	 "-N -a archive"
#    define LD_O_LIBS    	 "-lc /lib/dyncall.o"
#  endif
#  ifdef EXEC_PAGESIZE
#    define PAGSIZ    EXEC_PAGESIZE
#  else
#    define PAGSIZ    0x1000
#  endif
#endif

#if vax
#define PAGSIZ		0x400
#endif

#ifndef N_DATOFF			/* SunOs 3.4 does not define this */
#define N_DATOFF(x) ( N_TXTOFF(x) + (x).a_text )
#endif

#define ROUND_UP(cp,POWER_OF_TWO) \
  (((unsigned long)(cp)+POWER_OF_TWO-1) & ~(POWER_OF_TWO-1))

#define PAGE_ROUND_UP(cp) \
  ROUND_UP(cp,PAGSIZ)

#define ADDRESS_ALIGN(cp) \
  ((char *)(PAGE_ROUND_UP(cp)))

#if O_NOENTRY
#define MAXSYMBOL 256			/* maximum length of a function name */

typedef struct
{ char *string;				/* name of function (withouth _) */
  Func function;			/* functions address */
} textSymbol;

char *symbolString();			/* forwards */
#endif /* O_NOENTRY */

#ifndef aout_800
static struct exec header;            /* a.out header */
#else
struct aouthdr sysHeader;
struct filehdr fileHeader;
#endif

void
resetLoader(void)
{ loaderstatus.symbolfile = loaderstatus.orgsymbolfile = NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate room for text and data segment of executable.  The  SUN  has  a
special  function  for  this  called valloc(). On some systems you might
need to start the text and data segment on a page  boundary,  on  others
not.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(hpux) || defined(vax)
#  ifndef aout_800
#define valloc malloc
#  else
#define valloc( size )        ADDRESS_ALIGN( malloc( ( size ) + PAGSIZ - 1 ) )
#  endif
#endif

long
allocText(long int size)
{ long base;

  if ( size < sizeof(word) )
    return 0;				/* test run */

  size = ROUND(size, sizeof(long));

  if ( !(base = (long) valloc(size)) )
    fatalError("%s", OsError());

  statistics.heap += size;

  return base;
}


word
pl_load_foreign(term_t file, term_t entry, term_t options,
		term_t libraries, term_t size)
{ char *sfile, *sentry, *soptions, *slibraries;
  long sz, nsz, n;
  Atom execName;
  char *execFile;
  long base;
  int fd;

  if ( !PL_get_atom_chars(file, &sfile) ||
       !PL_get_atom_chars(entry, &sentry) ||
       !PL_get_atom_chars(options, &soptions) ||
       !PL_get_atom_chars(libraries, &slibraries) ||
       !PL_get_long(size, &sz) )
    return warning("pl_load_foreign/5: instantiation fault");

  if ( sz < 0 )
    sz = 0;
  
  TRY( getSymbols() );
  execName = TemporaryFile("ld");
  execFile = stringAtom(execName);

  for( n=0; n<2; n++)
  { base = (long) allocText(sz);
    TRY( create_a_out(sfile, sentry, soptions, slibraries, base, execFile) );
    if ( (fd = openExec(execFile)) < 0 )
      fail;

    if ( sizeExec() <= sz )
    { Func entry;
      if ( (entry = loadExec(fd, base, sentry)) == NULL )
    	fail;
      loaderstatus.symbolfile = execName;
      DEBUG(1, Sdprintf("Calling entry point at 0x%x\n", entry));
      (*entry)();
      DEBUG(1, Sdprintf("Entry point returned successfully\n"));

      succeed;
    }

    if ( base > 0 )			/* used for test runs */
      freeHeap(base, sz);
    nsz = sizeExec();
    if ( sz > 0 )
    { Putf("! Executable %s does not fit in %d bytes\n", sfile, sz);
#ifndef aout_800
      Putf("Size: %d bytes (%d text %d data, %d bss) (reloading ...)\n",
		nsz, header.a_text, header.a_data, header.a_bss);
#endif
    }
    sz = nsz;
  }

  return sysError("Can't fit executable %s", execFile);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create an a.out file from a .o file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef LD_COMMAND
#define LD_COMMAND	"ld"		/* Unix loader command name */
#endif
#ifndef LD_O_OPTIONS
#define LD_O_OPTIONS	"-N"		/* General options */
#endif
#ifndef LD_O_SFILE
#define LD_O_SFILE	"-A %s"		/* symbol file of process */
#endif
#ifndef LD_O_ADDR
#define LD_O_ADDR	"-T %lx"	/* Base address */
#endif
#ifndef LD_O_ENTRY
#define LD_O_ENTRY	"-e _%s"	/* Entry-point */
#endif
#ifndef LD_O_OUT
#define LD_O_OUT	"-o %s"		/* output file */
#endif
#ifndef LD_O_LIBS
#define LD_O_LIBS	"-lc"		/* standard libraries */
#endif

static bool
create_a_out(char *files, char *entry, char *options, char *libraries, long int base, char *outfile)
{ char command[10240];
  char *s = command;

#define next(str) { (str) += strlen(str); *(str)++ = ' '; };

  Ssprintf(s, "%s", LD_COMMAND);					 next(s);
  Ssprintf(s, "%s", LD_O_OPTIONS);				 next(s);
  Ssprintf(s, LD_O_SFILE, stringAtom(loaderstatus.symbolfile));   next(s);
  Ssprintf(s, LD_O_ADDR, base);				 	 next(s);
#if !O_NOENTRY
  Ssprintf(s, LD_O_ENTRY, entry);				 next(s);
#endif
  Ssprintf(s, LD_O_OUT, outfile);				 next(s);
  Ssprintf(s, "%s", options);					 next(s);
  Ssprintf(s, "%s", files);					 next(s);
  Ssprintf(s, "%s", libraries);					 next(s);
  Ssprintf(s, LD_O_LIBS);

#undef next
  
  DEBUG(1, Sdprintf("Calling loader: %s\n", command) );
  if (system(command) == 0)
    succeed;

  unlink(outfile);
  return warning("load_foreign/5: Failed to create an executable from %s\ncommand was %s",
		 files,
		 command);
}

#ifndef O_BINARY
#define O_BINARY 0
#endif

static
int
openExec(char *execFile)
{ int fd;

					/* O_BINARY needed on OS2 && EMX  */
  if ((fd=open(execFile, O_RDONLY|O_BINARY)) < 0)
  { warning("load_foreign/5: Cannot open %s", execFile);
    return -1;
  }

#ifndef aout_800
  if (read(fd, &header, sizeof(struct exec)) != sizeof(struct exec) ||
      N_BADMAG(header) != 0)
  { warning("load_foreign/5: Bad magic number in %s", execFile);
    close(fd);
    return -1;
  }
#else
  if ( read(fd, &fileHeader, sizeof(fileHeader)) != sizeof(fileHeader) )
  { warning("load_foreign/5: Unable to read file header of %s\n", execFile);
    close(fd);
    return -1;
  }
  if ( fileHeader.aux_header_size == 0 )
  { warning("load_foreign/5: No read aux header in %s\n", execFile);
    close(fd);
    return -1;
  }
  lseek(fd, fileHeader.aux_header_location, 0 );
  if ( read(fd, &sysHeader, sizeof(sysHeader)) != sizeof(sysHeader) )
  { warning("load_foreign/5: Unable to read som header of %s\n", execFile);
    close(fd);
    return -1;
  }
#endif

  return fd;
}


static
int
sizeExec(void)
{ return
#ifndef aout_800
    ROUND(header.a_text, 4) +
    ROUND(header.a_data, 4) +
    ROUND(header.a_bss, 4);
#else
    PAGE_ROUND_UP(sysHeader.tsize) +
    PAGE_ROUND_UP(sysHeader.dsize) +
    PAGE_ROUND_UP(sysHeader.bsize);
#endif
}


static Func
loadExec(int fd, unsigned long base, char *sentry)
{ Func entry;
  long *text, text_off, text_size;
  long *data, data_off, data_size;
  long *bss, bss_size;

#ifndef aout_800
  text = (long *)base;			/* address of text in memory */
  text_size = header.a_text;		/* size of text area */
  data = (long *)(base+text_size);	/* address of data in memory */
  data_size = header.a_data;		/* size of data area */
  text_off = N_TXTOFF(header);		/* offset of text in file */
  data_off = N_DATOFF(header);		/* offset of data in file */
  bss = (long *)(base + text_size + data_size);
  bss_size = header.a_bss;
#else
  text = (long *)sysHeader.exec_tmem; /* address of text in memory */
  text_size = sysHeader.tsize;                /* size of text area */
  data = (long *)sysHeader.exec_dmem; /* address of data in memory */
  data_size = sysHeader.dsize;                /* size of data area */
  text_off = N_TXTOFF(sysHeader);     /* offset of text in file */
  data_off = N_DATOFF(sysHeader);     /* offset of data in file */
  bss = (long *)(data + data_size);
  bss_size = sysHeader.bsize;
#endif

  DEBUG(1, Sdprintf("Text offset = %d, Data offset = %d\n", text_off, data_off));
  DEBUG(1, Sdprintf("Base = 0x%x (= %d), text at 0x%x, %d bytes, data at 0x%x, %d bytes\n",
		    base, base, text, text_size, data, data_size) );

  if ( lseek(fd, text_off, 0) < 0 ||
       text_size != read(fd, text, text_size) ||
       lseek(fd, data_off, 0) < 0 ||
       data_size != read(fd, data, data_size) )
  { warning("load_foreign/5: Failed to read text segment");
    close(fd);
    return NULL;
  }

#if O_NOENTRY
  { textSymbol ts[1];
    ts[0].string = sentry;
    ts[0].function = (Func) NULL;

    TRY( scanSymbols(fd, 1, ts) );
    entry = ts[0].function;
  }
#else
#  if hpux
#    ifndef aout_800
  entry = (Func)(header.a_entry + (long)text);
  DEBUG(2, Sdprintf("a_entry = 0x%x; text = 0x%x, entry = 0x%x\n",
				header.a_entry, text, entry));
#    else
  entry = (Func)(sysHeader.exec_entry);
  DEBUG(2, Sdprintf("exec_entry = 0x%x; text = 0x%x, entry = 0x%x\n",
                              sysHeader.exec_entry, text, entry));
#    endif
#  else
  entry = (Func)(header.a_entry);
#  endif
#endif

  close(fd);

  DEBUG(1, Sdprintf("Cleaning BSS %d bytes from 0x%x (=%d)\n", 
	      bss_size, bss, bss));
  memset(bss, 0, bss_size);

  return entry;
}

#if O_NOENTRY
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the symbol table and try to resolve all textSymbols given  in  `tv'
(target  vector).   The  first `tc' (target count) members of this array
are valid.  TRUE is returned if  all  functions  are  found.   Otherwise
FALSE is returned.

Searching starts at the end of the symbol table, as this  is  the  place
were the incrementally loaded symbols normally lives.

It assumes a global struct exec `header'  to  hold  the  header  of  the
symbol  file and the argument `fd' to be an open file descriptor on that
file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
scanSymbols(fd, tc, tv)
int fd;
int tc;
textSymbol * tv;
{ long symbols, strings;
  long next_symbol;
  struct nlist name;
  char *s;
  int n, left = tc;

  symbols = N_SYMOFF(header);
  strings = N_STROFF(header);

  n = (strings - symbols)/sizeof(struct nlist);
  next_symbol = symbols+(n-1)*sizeof(struct nlist);

  for(; next_symbol >= symbols; next_symbol -= sizeof(struct nlist) )
  { if (lseek(fd, next_symbol, 0) < 0)
      return warning("seek on executables' symbol table failed");
    if (read(fd, &name, sizeof(struct nlist) ) != sizeof(struct nlist) )
      return warning("failed to read symbol in executable");

    if (name.n_type == (unsigned char)(N_TEXT|N_EXT))
    { s = symbolString(fd, name.n_un.n_strx+strings);

      for(n = 0; n < tc; n++)
      { if ( streq(tv[n].string, s+1) )
	{ tv[n].function = (Func) name.n_value;
	  if ( --left <= 0 )
	    succeed;
	}
      }
    }
  }

  if ( left > 0 )
  { for(n = 0; n < tc; n++)
    { if ( tv[n].function == (Func) NULL )
        warning("Dynamic loader: undefined: %s", tv[n].string);
    }
    fail;
  }
  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return the char string at offset `n' in the string table.   The  strings
are supposed not to be longer than MAXSYMBOL characters.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
symbolString(fd, n)
int fd;
long n;
{ static char temp[MAXSYMBOL+1];
  int l;

  if (n == 0)
    return "";
  if (lseek(fd, n, 0) < 0)
  { warning("Failed to seek to string in executable");
    return (char *) NULL;
  }
  l = read(fd, temp, MAXSYMBOL);
  temp[l] = EOS;

  return temp;
}

#endif /* O_NOENTRY */

#else

#if O_AIX_FOREIGN
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The AIX foreign interface  is completely different to the SUN/VAX/HPUX
version.  The  latter cannot  be used  because ld is  lacking   the -A
option and AIX uses  XCOFF  format a.out files.  Instead, AIX supplies
the  load()  and loadbind() functions  to   load executable  code in a
running  image.   This makes   the implementation a   lot  easier (and
supported by official functions).

There is  still a problem in  the cooperation with save_program/[1,2].
Normally, it appears the foreign code is loaded in  the program's data
area and save nicely  by save_program.  If the loaded   code  is small
however it will be put below &_data, in  which case save_program won't
see it.   Currently,  there is  only detection  of  this  problem.  We
should try  to figure out  the starting adres  of the loaded code  and
communicate this to save_program.  How to do this?

Note  than  the  Prolog   part    is  also different    for AIX.   See
boot/aixforeign.pl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <sys/ldr.h>

static Func main_entry;		/* my entry-point */

void
resetLoader()
{ loaderstatus.symbolfile = loaderstatus.orgsymbolfile = NULL;
  main_entry = NULL;
}

word
pl_load_foreign1(term_t file)
{ char *sfile;
  Atom name;
  long rval;
  Func entry;
  char *libpath = (getenv("LIBPATH") == NULL ? "/lib:/usr/lib" : NULL);
  extern int _data;

  if ( !PL_get_atom(file, &name) )
    return warning("pl_load_foreign/5: instantiation fault");
  sfile = stringAtom(name);

  if ( main_entry == NULL )
  { char *me;

    TRY(getSymbols());
    me = stringAtom(loaderstatus.symbolfile);

    DEBUG(1, Sdprintf("Loading %s ... ", me));
    if ( (main_entry = (Func) load(me, L_NOAUTODEFER, libpath)) == NULL )
      return warning("load_foreign/5: %s: %s", me, OsError());
    DEBUG(1, Sdprintf("ok\n"));
  }

  DEBUG(1, Sdprintf("Loading %s ... ", sfile));
  if ((entry = (Func) load(sfile, L_NOAUTODEFER, libpath)) == NULL)
  { char *buf[1024];
    warning("load_foreign/5: %s: %s", sfile, OsError());

    buf[0] = "execerror";
    buf[1] = sfile;
    if ( loadquery(L_GETMESSAGES, &buf[2], sizeof(buf) - 8) < 0 )
      warning("load_foreign/5: loadquery: %s", OsError());
    else
    { switch ( fork() )
      { case 0:
	  execvp("/etc/execerror", buf);
	case -1:
	  warning("Couldn't exec /etc/execerror: %s", OsError());
      }
    }
    fail;
  }
  DEBUG(1, Sdprintf("ok\n"));

  if ( entry < (Func) &_data )
    cannot_save_program = "Foreign code loaded outside data area";

  DEBUG(1, Sdprintf("Loadbind() ... "));
  if ( loadbind(0, main_entry, entry) != 0 )
    return warning("load_foreign/5: loadbind: %s", OsError());
  DEBUG(1, Sdprintf("ok\n"));

  DEBUG(1, Sdprintf("Calling entry-point at 0x%x\n", entry));
  rval = (*entry)();
  DEBUG(1, Sdprintf("rval = %d (0x%x)\n", rval, rval));

  succeed;
}

#else

#if O_MACH_FOREIGN
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The NeXT foreign interface is completely   different to the SUN/VAX/HPUX
version.  The latter cannot be used because   the  NeXT uses MACH format
a.out files.  Instead, MACH  supplies   the  rld_load() and rld_lookup()
functions to load executable code in a   running  image.  This makes the
implementation a lot easier (and supported by official functions).

The prolog part is identical to  the   SUN  versions.  However, the only
arguments of load_foreign/5 that are used   are 'File', 'Libraries', and
'Entry'.  The other arguments are ignored.   'Libraries' is not expanded
by the C code; filenames should be  either full pathnames or 'library()'
names that expand to a full pathname.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_MACH_O_RLD_H
#include <mach-o/rld.h>
#else
#include <rld.h>
#endif
#include <strings.h>
#include <streams/streams.h>

extern int unlink(const char *), mkstemp (char *template), close(int);
extern char *mktemp(char *template);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
the rld_...  routines  spew  their  complaints   on  a  stream  of  type
NXStream.  We do not want to print   these to Serror or Soutput, because
the 'current stream' mechanism of prolog   is  circumvented in this way.
We open a temp file instead, informing the user this file exists only if
an error occurred and errno == 0.

Be aware of the fact rld_load() may fail   and not set errno to !0.  For
example,  the  call  rld_load(rld_err_stream,_,"i_do_not_exist",_)  will
result in the string "rld(): Can't open: i_do_not_exist (No such file or
directory, errno = 2)" being sent to  the appropriate stream, with errno
==  2,  while  the  call  rld_load(rld_err_stream,_,"/dev/null",_)  will
result in "rld(): file: /dev/null is empty (not an object or archive)"
being printed, with errno == 0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_load_foreign(term_t file, term_t entry, term_t options,
		term_t libraries, term_t size)
{ char *sfile, *sentry, *soptions, *slibraries;

  struct mach_header *m_header;
  long rld_result, rval;
  unsigned long rld_adress;
  Func entry_func;
  char **object_filenames;
  char *tmp;
  int stringno, maxstrings, i;

  /* errorhandling */
  char      *errorBuffer;
  int        streamLength, maxLength;
  NXStream  *rld_err_stream;
  
  char underscore = '_';

  status.debugLevel = 1;
  rld_err_stream = NXOpenMemory(NULL,0,NX_WRITEONLY);

  if ( !PL_get_atom_chars(file, &sfile) ||
       !PL_get_atom_chars(entry, &sentry) ||
       !PL_get_atom_chars(libraries, &slibraries) ) 
    return warning("pl_load_foreign/5: instantiation fault");

  /* append object-files and libraries */
  if (strlen(slibraries) > 0)
    sfile = strcat(strcat(sfile," "),slibraries);
  
  /* as *file as well as *libraries may point to a string containing >1
   * filename, we have to break *sfile up in pieces, in order to get 
   * the type of argument rld_load() expects: char **
   */
   
  	/* estimate max number of sub-strings in string */
   maxstrings = (strlen(sfile)/ 2) +1;
   if ((object_filenames = 
      (char **)calloc((size_t)maxstrings,sizeof(char *))) == (void *)NULL)
     fatalError("%s", OsError());

   stringno = 0;
   if (*sfile != '\0') 
      do {
        object_filenames[stringno] = sfile; /* sub-string */
        tmp = strchr(sfile,' '); /* try to find a space */
        if (tmp != (char *)0) /* space found */
        {  *tmp = '\0'; /* terminate previous string (replace ' ' by '\0') */
           stringno++;
   	   tmp++; sfile = tmp;            
        } else { /* no space left in string pointed to by tmp */
           object_filenames[stringno + 1] = NULL; /* signals end of char** to rld_load */
        }
      } while (tmp != (char *)0); /* end of sfile reached */
   else /* sfile == "" */
     object_filenames[0] = NULL;
   
  DEBUG(1, 
    Sdprintf("Calling rld_load(), file(s):\n");
    for (i = 0; i <= stringno; i++)
      Sdprintf("\t \"%s\"\n",object_filenames[i]));
  
  rld_result = rld_load(rld_err_stream,&m_header,object_filenames,NULL);
  /* get rid of these as soon as we can */
  free((void *)object_filenames);

  if (rld_result == 0) 
  { 	
    NXFlush(rld_err_stream);
	NXGetMemoryBuffer(rld_err_stream, &errorBuffer, &streamLength, &maxLength);
	warning("load_foreign/5: rld_load() failed (%s)",errorBuffer);
    NXCloseMemory (rld_err_stream, NX_FREEBUFFER);
	fail;
  } 
  DEBUG(1, Sdprintf("\nrld_load returned ok (adress of mach-header: %ld)\n",m_header));

  DEBUG(1, Sdprintf("Calling rld_lookup()\n"));
  /* Add an underscore to sentry (as in symbol-table looked at by 
   * rld_lookup())
   *
   * 	Problems:
   *
   * Rld_error_stream not used here; rld_lookup() seems to alter
   * the stream; even if the stream * is NOT passed to it !!
   * Functions using the stream dump core on us;
   * unfortunately I can't replicate the error in a small program.
   */ 
  if ( rld_lookup(NULL,strcat(&underscore,sentry), &rld_adress) == 0 )
  {
	warning("load_foreign/5: rld_lookup() of \"%s()\" failed",sentry);
	fail;
  }
  DEBUG(1, Sdprintf("rld_lookup returned ok\n"));

  entry_func = (Func)rld_adress;
  DEBUG(1, Sdprintf("Calling entry-point at 0x%x\n", entry_func));
  rval = (*entry_func)();
  if (!rval > 0) {
  	warning("load_foreign/5: entry-function failed (%s())",sentry);
	fail;
  }
  DEBUG(1, Sdprintf("Entry point returned successfully\n"));
  DEBUG(1, Sdprintf("rval = %d (0x%x)\n", rval, rval));
  
  succeed;
}

void
resetLoader()
{ loaderstatus.symbolfile = loaderstatus.orgsymbolfile = NULL;
}

#else					/* No foreign language interface */

void
resetLoader()
{ loaderstatus.symbolfile = loaderstatus.orgsymbolfile = NULL;
}

word
pl_load_foreign(term_t file, term_t entry, term_t options,
		term_t libraries, term_t size)
{
#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD) || defined(O_DLL)
  warning("load_foreign/[2,5] are not available for this machine\n"
	  "\thowever, the predicates from `library(shlib)' are available");
#else
  warning("load_foreign/[2,5] are not available for this machine");
#endif

  fail;
}

#endif /* O_MACH_FOREIGN */
#endif /* O_AIX_FOREIGN */
#endif /* O_FOREIGN */

		 /*******************************
		 *     DLOPEN() AND FRIENDS	*
		 *******************************/

#ifdef HAVE_DLOPEN			/* sysvr4, elf binaries */

#include <dlfcn.h>

#ifndef RTLD_GLOBAL			/* solaris defines this */
#define RTLD_GLOBAL 0
#endif
#ifndef RTLD_NOW			/* implicit on some versions */
#define RTLD_NOW 0
#endif

#endif HAVE_DLOPEN

#ifdef HAVE_SHL_LOAD			/* HPUX */

#include <dl.h>
#define dlopen(path, flags) shl_load((path), (flags), 0L)
#define dlclose(handle)	    shl_unload((handle))
#define dlerror() OsError()

void *
dlsym(shl_t handle, const char *name)
{ void *value;

  if ( shl_findsym(&handle, name, TYPE_PROCEDURE, &value) < 0 )
    return NULL;

  return value;
}

#define RTLD_LAZY	BIND_DEFERRED
#define RTLD_GLOBAL	0

#endif

#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD)

typedef int (*dl_funcptr)();

typedef struct dl_entry *DlEntry;
struct dl_entry
{ int	  id;				/* Prolog's identifier */
  void   *dlhandle;			/* DL libraries identifier */
  Atom	  file;				/* Loaded filed */
  DlEntry next;				/* Next in table */
};

int	dl_plid;			/* next id to give */
DlEntry dl_head;			/* loaded DL's */
DlEntry dl_tail;			/* end of this chain */

#define DL_NOW	  0x1
#define DL_GLOBAL 0x2

word
pl_open_shared_object(term_t file, term_t plhandle,
		      term_t flags)
{ void *dlhandle;
  Atom afile;
  DlEntry e;
  int dlflags;
  int n;

  if ( PL_get_integer(flags, &n) )
  { dlflags = (n & DL_NOW) ? RTLD_NOW : RTLD_LAZY;
    if ( n & DL_GLOBAL )
      dlflags |= RTLD_GLOBAL;
  } else
    dlflags = RTLD_LAZY | RTLD_GLOBAL;

  if ( !PL_get_atom(file, &afile) )
    return warning("open_shared_object/2: instantiation fault");
  if ( !(dlhandle = dlopen(stringAtom(afile), dlflags)) )
    return warning("load_shared_object/2: %s", dlerror());
  e = allocHeap(sizeof(struct dl_entry));
  e->id       = ++dl_plid;
  e->dlhandle = dlhandle;
  e->file     = afile;
  e->next     = NULL;
  if ( !dl_tail )
    dl_head = dl_tail = e;
  else
    dl_tail->next = e;

  return PL_unify_integer(plhandle, e->id);
}


static DlEntry
find_dl_entry(term_t h)
{ DlEntry e;
  int id;

  if ( PL_get_integer(h, &id) )
  { for(e = dl_head; e; e = e->next)
      if ( e->id == id )
	return e;
  }

  return NULL;
}


word
pl_close_shared_object(term_t plhandle)
{ DlEntry e = find_dl_entry(plhandle);

  if ( e && e->dlhandle) 
  { dlclose(e->dlhandle);
    e->dlhandle = NULL;

    succeed;
  }

  fail;
}


word
pl_call_shared_object_function(term_t plhandle, term_t name)
{ DlEntry e = find_dl_entry(plhandle);
  char *fname;
  dl_funcptr ef;

  if ( !e || !e->dlhandle )
    return warning("call_shared_object_function/2: bad handle");
  if ( !PL_get_chars(name, &fname, CVT_ALL) )
    return warning("call_shared_object_function/2: instantiation fault");
  
  if ( !(ef = (dl_funcptr) dlsym(e->dlhandle, fname)) )
    fail;

  (*ef)();

  succeed;
}

#else /*HAVE_DLOPEN*/

word
pl_open_shared_object(Word file, Word plhandle, Word flags)
{ return warning("open_shared_object/3: not ported to this machine");
}

#endif /*HAVE_DLOPEN*/
