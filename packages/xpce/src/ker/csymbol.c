/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

#ifndef O_RUNTIME
#include <h/interface.h>

typedef struct
{ char *name;				/* name of the symbol */
  long value;				/* its value; */
} c_symbol, *CSymbol;


CSymbol symbols = NULL;			/* symbol-table */
long	nsymbols = 0;			/* # symbols */
long	allocated = 0;			/* allocated size (in symbols) */


Name
getCSymbolFilePce(Pce pce)
{ char *name = getHostSymbolTable();

  if ( name )
    answer(CtoName(name));
  else
  { Name machine = get(pce, NAME_machine, 0);
    Name home    = get(pce, NAME_home, 0);
    char buf[LINESIZE];

    if ( machine && home )
    { sprintf(buf, "%s/%s/XPCE.o", strName(home), strName(machine));
      answer(CtoName(buf));
    }
  }

  fail;
}


static status
addCSymbol(Pce pce, char *name, long int value)
{ if ( nsymbols == allocated )
  { CSymbol new;

    if ( !(new = pceRealloc(symbols, (allocated+500)*sizeof(c_symbol))) )
    { return errorPce(pce, NAME_notEnoughMemory);
    } else
    { symbols = new;
      allocated += 500;
    }
  }

  symbols[nsymbols].name = save_string(name);
  symbols[nsymbols].value = value;

  nsymbols++;
  succeed;
}


static status
strsuffix(char *str, char *suff)
{ int   ln = strlen(str);
  int   ls = strlen(suff);

  if ( ls <= ln && streq(&str[ln-ls], suff) )
    succeed;

  fail;
}


static status
initCSymbolsPce(Pce pce)
{ if ( !symbols )
  {
#ifndef HAVE_POPEN
    return errorPce(pce, NAME_cannotLoadCSymbols, CtoName("No popen()"));
#else
    long offset = 0;
    Name symbolfile;
    char line[LINESIZE];
    FILE *fd;

    symbols = pceMalloc(500*sizeof(c_symbol));
    allocated = 500;

    if ( !(symbolfile = get(pce, NAME_cSymbolFile, 0)) )
      fail;
    
    sprintf(line, "nm -n %s", strName(symbolfile));
    DEBUG(NAME_cSymbols, Cprintf("Loading C-symbols\n\t(running %s)\n", line));
    if ( (fd = popen(line, "r")) )
    { while ( fgets(line, LINESIZE, fd) )
      { unsigned long value;
	char type[100];
	char name[200];

	if ( sscanf(line, "%lx%s%s", &value, type, name) == 3 &&
	     type[1] == EOS &&
	     (type[0] == 't' || type[0] == 'T') &&
	     !strsuffix(name, ".o") &&
	     !strsuffix(name, ".") )	/* trap gcc_compiled., etc */
	{ if ( !addCSymbol(pce, &name[1], value) )
	    break;
	  if ( streq(&name[1], "initCSymbolsPce") )
	  { offset = (long) initCSymbolsPce - value;
	    if ( offset )
	      DEBUG(NAME_cSymbols, Cprintf("\toffset = %ld\n", offset));
	  }
	}
      }
      pclose(fd);
      DEBUG(NAME_cSymbols, Cprintf("found %ld text symbols", nsymbols));
    } else
      errorPce(pce, NAME_cannotLoadCSymbols, getOsErrorPce(pce));
#endif /*HAVE_POPEN*/
  }

  succeed;
}

char *
c_function_name_from_address(long int addr, int *perc)
{ long low, high;

  TRY(initCSymbolsPce(PCE));

  if ( addr < symbols[0].value ||
       addr > symbols[nsymbols-1].value )
    fail;

  for(low = 0, high = nsymbols; ;)
  { long here = (low + high) / 2;

    DEBUG(NAME_cFunctionName,
	  Cprintf("low = %ld; here = %ld; high = %ld (%ld)\n",
		  low, here, high, symbols[here].value));
    if ( symbols[here].value <= addr &&
	 (here > nsymbols - 2 || symbols[here+1].value > addr) )
    { if ( perc && here <= nsymbols - 2 )
      { *perc = ((addr - symbols[here].value) * 100) /
 	         (symbols[here+1].value - symbols[here].value);
      }

      return symbols[here].name;
    }
  
    if ( here == low )
    { if ( low == high )
      { Cprintf("Can't find symbol from 0x%lx, l=0x%lx, h=0x%lx\n",
		addr, symbols[low].value, symbols[high].value);
	fail;
      } else
	here = low = high;
    }

    if ( addr > symbols[here].value )
      low = here;
    else
      high = here;
  }
}


Name
getCFunctionNamePce(Pce pce, Int address)
{ char *name = c_function_name_from_address(valInt(address), NULL);

  if ( name )
    answer(CtoName(name));

  fail;
}

#endif /*O_RUNTIME*/
