/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <errno.h>
#include <signal.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <string.h>

static int debuglevel = 0;
static int emit_space = 0;

#define DEBUG(n, g)	if ( n <= debuglevel ) { g; }

#define MAXFUNC		100		/* max bounded function name length */
#define MAXCMD		256		/* max length of a \foobar */
#define MAXARG		4096		/* max {..} argument length */
#define MAXWORD		1024		/* max length of a word (no spaces) */
#define MAXVERB		1024		/* max length of \verb|string| */
#define MAXMATH		4096		/* max length of $...$ */
#define MAXVERBATIM	10240		/* max size of verbatim */
#define MAXOUTPUT	11000		/* output() max */
#define MAXCMDARGS	32
#define MAXENVNESTING	256		/* max depth of environment */

static void error(int eno, const char *file, int line); /* handle errors */
static void warn(int eno, const char *file, int line); /* handle errors */

#define ERR_UNEXPECTED_EOF	1	/* unexpected end-of-file */
#define ERR_RUNAWAY_ARGUMENT	2	/* runaway argument */
#define ERR_VERBATIM_TOO_LONG	3	/* verbatim env too large */
#define ERR_NOCMD_SPECS		4	/* cannot find command-specs */
#define ERR_BAD_COMMAND_SPEC	5	/* syntax error in command specs */
#define ERR_ENV_NESTING		6	/* environment-stack overflow */
#define ERR_UNDEF_FUNCTION	7	/* undefined function */
#define ERR_BAD_ARG_SPEC	8
#define ERR_BAD_ENV_SPEC	9
#define ERR_RUNAWAY_MATH       10	/* notclosed math env */
#define ERR_BAD_MATH_ENV_CLOSURE 11	/* $$ closed by $ */
#define ERR_ENV_UNDERFLOW      12	/* environment stack-underflow */
#define ERR_CMD_TOO_LONG       13
#define ERR_BAD_DIM	       14

const char *tex_error_strings[] =
{ "No error",
  "Unexpected end of file",
  "Runaway argument",
  "Verbatim environment too long",
  "Cannot find command specification file",
  "Syntax error",
  "Environment-stack overflow",
  "Undefined function",
  "Syntax error in argument definition",
  "Syntax error in environment definition",
  "$: Runaway argument",
  "$$ closed by single $",
  "Too many '}' or \\end{}",
  "Command name too long",
  "Bad dimension",
  NULL					/* allow for easy enumeration */
};

#ifndef FALSE
#define FALSE 0
#define TRUE  1
#endif

#define EOS '\0'			/* end-of-string */
#define streq(s1, s2) (strcmp((s1), (s2)) == 0)

		 /*******************************
		 *	     COMMANDS		*
		 *******************************/

#define F_NOSKIPBLANK	0x01		/* Argument handling */

#define CA_OPTIONAL	0x01		/* [optional arg] */
#define CA_TEXT		0x02		/* Argument contains output text */
#define CA_DIM		0x04		/* Argument is a dimension */

#define CMD_MODIFY	0x01		/* command allows for modifier */

#define PRE_COMMENT	-1		/* put %\n before command */

typedef struct _command *Command;
typedef struct _environment *Environment;
typedef struct _token *Token;
typedef struct _input *Input;
typedef struct _output *Output;

typedef void (*CallBack)(Token token, void *context);
typedef void (*CmdFunc)(Command cmd, Input fd, CallBack func, void *ctx);
typedef void (*EnvFunc)(Environment cmd, Input fd, CallBack func, void *ctx);
typedef void *AnyFunc;
static AnyFunc lookupFunction(const char *name);

typedef struct
{ int flags;				/* command arguments flags */
} cmd_arg, *CmdArg;

typedef struct _cmd_descr
{ const char*name;			/* name of the command */
  int	     flags;			/* command-flags */
  CmdArg     arguments;			/* argument specifiers */
  char	     arg_count;			/* # arguments */
  char	     pre_lines;			/* # newlines needed before */
  char	     post_lines;		/* # newlines needed after */
  CmdFunc    function;			/* associated function */
  const char*fname;			/* function-name */
  struct _cmd_descr *next;		/* next in hash-table */
} cmd_descr, *CmdDescr;

typedef struct _command
{ CmdDescr   command;			/* the commands */
  int	     flags;			/* general flags */
  char **    arguments;			/* the actual arguments */
} command;

typedef struct _env_descr
{ const char *name;			/* name of the environment */
  int	      flags;			/* environment flags */
  CmdArg      arguments;		/* argument-list */
  char	      arg_count;		/* # arguments */
  EnvFunc     function;			/* associated function */
  const char *fname;			/* function-name */
  struct _env_descr *next;
} env_descr, *EnvDescr;

typedef struct _environment
{ EnvDescr    environment;
  int	      flags;
  char **     arguments;
} environment;

#define INPUT_FILE	0		/* reading from a file */
#define INPUT_STRING	1		/* reading from a string */

typedef struct _input
{ int		type;
  int		lineno;
  const char *  name;			/* name (for feedback) */
  Input		parent;			/* Parent input */
  union
  { FILE       *fd;
    const char *string;
  } stream;
} input;

static Input	curin;			/* current input (file) */

static void cmd_prolog(Command g, Input fd, CallBack func, void *ctx);

		 /*******************************
		 *	       TOKENS		*
		 *******************************/


#define TOK_CMD		 0		/* \cmd */
#define TOK_BEGIN_GROUP	 1		/* { */
#define TOK_END_GROUP	 2		/* } */
#define TOK_BEGIN_ENV	 3		/* \begin{cmd} */
#define TOK_END_ENV	 4		/* \end{cmd} */
#define TOK_VERB	 5		/* \verb|foo| */
#define TOK_VERBATIM	 6		/* verbatim environment */
#define TOK_PRE		 7		/* pre environment */
#define TOK_MATH	 8		/* $...$ */
#define TOK_MATH_ENV	 9		/* $$...$$ */
#define TOK_PAR		10		/* implicit paragraph (blank line) */
#define TOK_WORD        11		/* general word */
#define TOK_NOSPACEWORD 12		/* word without reintroducing spaces */
#define TOK_SPACE       13		/* blank space */
#define TOK_LINE        14		/* single line */
#define TOK_EOF	        15		/* end-of-file */

typedef struct _token
{ int	type;				/* type identifier */
  int	prelines;			/* HTML stuff */
  int	postlines;			/* HTML stuff */
  char *context;			/* additional context info */
  union 
  { char *string;			/* related text */
    Command cmd;			/* related TeX command */
    Environment env;			/* related TeX environment */
  } value;
} token;

static const char *texarg;		/* argument for runaway message */

		 /*******************************
		 *   CHARACTER CLASSIFICATION	*
		 *******************************/

#define EF 0				/* end-of-file */
#define SP 1				/* space */
#define SC 2				/* start line comment (%) */
#define BG 3				/* begin group ({) */
#define EG 4				/* end group (}) */
#define MM 5				/* math-mode delimiter ($) */
#define TD 6				/* Table delimiter (&) */
#define NB 7				/* Non-breaking space (~) */
#define CM 8				/* command (\) */

#define PU 9				/* punctuation */
#define DI 10				/* digit */

#define LC 11				/* lower-case letter */
#define UC 12				/* uppercase letter */

#define CharType(c) (char_type[(c)+1])

#define isspace(c) 	(CharType(c) == SP)
#define isdigit(c) 	(CharType(c) == DI)
#define isalnum(c) 	(CharType(c) >= LC)
#define wbreak(c)	(CharType(c) <= CM) /* breaks a word */
#define isbegingroup(c) (CharType(c) == BG)
#define iscommand(c)    (CharType(c) == CM)

static char char_type[] = {
/* EOF */
   EF,
/* ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N  ^O    0-15 */
   EF, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, 
/* ^P  ^Q  ^R  ^S  ^T  ^U  ^V  ^W  ^X  ^Y  ^Z  ^[  ^\  ^]  ^^  ^_   16-31 */
   SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, 
/* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   32-47 */
   SP, PU, PU, PU, MM, SC, TD, PU, PU, PU, PU, PU, PU, PU, PU, PU, 
/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   48-63 */
   DI, DI, DI, DI, DI, DI, DI, DI, DI, DI, PU, PU, PU, PU, PU, PU, 
/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   64-79 */
   PU, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, 
/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   80-95 */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, PU, CM, PU, PU, UC, 
/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   96-111 */
   PU, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  ^?   112-127 */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, BG, PU, EG, NB, SP, 
			  /* 128-255 */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC
};


		 /*******************************
		 *	      INPUT		*
		 *******************************/

static Input
openInputFile(const char *name)
{ FILE *fd;

  if ( (fd = fopen(name, "r")) )
  { Input i = malloc(sizeof(input));

    if ( i )
    { i->type      = INPUT_FILE;
      i->stream.fd = fd;
      i->lineno    = 1;

      i->name   = name;
      i->parent = curin;
      curin = i;

      return i;
    }
  }

  return NULL;
}


static Input
openInputString(const char *str)
{ Input i = malloc(sizeof(input));

  if ( i )
  { i->type	     = INPUT_STRING;
    i->stream.string = str;
    i->lineno    = 1;

    i->name = str;
    i->parent = curin;
    curin = i;

    return i;
  }

  return NULL;
}


static void
closeInput(Input i)
{ if ( i->type == INPUT_FILE )
    fclose(i->stream.fd);

  curin = i->parent;

  free(i);
}


static const char *
texfile()
{ if ( curin )
  { Input i = curin;

    while(i && i->type == INPUT_STRING)
      i = i->parent;

    if ( i && i->type == INPUT_FILE )
      return i->name;

    return curin->name;
  }

  return "no input";
}


static int
texline()
{ if ( curin )
  { Input i = curin;
    int offset = 0;

    while(i && i->type == INPUT_STRING)
    { offset += i->lineno - 1;
      i = i->parent;
    }

    if ( i && i->type == INPUT_FILE )
      return i->lineno + offset;

    return curin->lineno;
  }

  return -1;
}


static int
mygetc(Input fd)
{ int c;

  switch(fd->type)
  { case INPUT_FILE:
      c = getc(fd->stream.fd);
      break;
    case INPUT_STRING:
    default:
      c = *fd->stream.string++;
      break;
  }

  if ( c == '\n' )
    fd->lineno++;

  return c;
}

static void
myungetc(int c, Input fd)
{ switch(fd->type)
  { case INPUT_FILE:
      ungetc(c, fd->stream.fd);
      break;
    case INPUT_STRING:
    default:
      fd->stream.string--;
      break;
  }

  if ( c == '\n' )
    fd->lineno--;
}


static char *
myfgets(char *buf, int size, Input fd)
{ char *s = buf;

  for(;;)
  { int	c = mygetc(fd);
    
    if ( c == EOF )
    { if ( s == buf )
	return NULL;

      *s = EOS;
      return buf;
    } else if ( c == '\n' )
    { *s++ = c;
      *s = EOS;
      return buf;
    }

    *s++ = c;
    if ( s >= &buf[size-1] )
    { *s = EOS;
      return buf;
    }
  }
}


#undef getc
#undef ungetc
#undef fgets
#define getc(fd) mygetc(fd)
#define ungetc(c, fd) myungetc(c, fd)
#define fgets(buf, size, fd) myfgets(buf, size, fd)

		 /*******************************
		 *	      STRINGS		*
		 *******************************/


int
stringHashValue(const char *t, int buckets)
{ unsigned int value = 0;
  unsigned int shift = 5;

  while(*t)
  { unsigned int c = *t++;
    
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  return (value ^ (value >> 16)) % buckets;
}


const char *
save_string(const char *s)
{ char *cp = malloc(strlen(s)+1);

  strcpy(cp, s);

  return (const char *)cp;
}


		 /*******************************
		 *	 COMMAND REGISTRY	*
		 *******************************/

#define CMD_HASH_SIZE 256

static CmdDescr cmd_table[CMD_HASH_SIZE];

static CmdDescr
lookupCommand(const char *name)
{ int v = stringHashValue(name, CMD_HASH_SIZE);
  CmdDescr c;

  for(c = cmd_table[v]; c; c = c->next)
  { if ( streq(c->name, name) )
      return c;
  }

  if ( isspace(*name) && name[1] == EOS )
    return lookupCommand(" ");

  return NULL;
}


static CmdDescr
newCommand(const char *name)
{ int v = stringHashValue(name, CMD_HASH_SIZE);
  CmdDescr c;

  for(c = cmd_table[v]; c; c = c->next)
  { if ( streq(c->name, name) )
    { c->flags = 0;
      c->arg_count = 0;
      if ( c->arguments )
      { free(c->arguments);
	c->arguments = NULL;
      }
      return c;
    }
  }

  c = malloc(sizeof(*c));
  c->name       = save_string(name);
  c->flags      = 0;
  c->arg_count  = 0;
  c->arguments  = NULL;
  c->pre_lines  = 0;
  c->post_lines = 0;
  c->function   = NULL;
  c->fname      = NULL;
  c->next       = cmd_table[v];
  cmd_table[v]  = c;

  return c;
}

#define ENV_HASH_SIZE 256

static EnvDescr env_table[ENV_HASH_SIZE];

static EnvDescr
lookupEnvironment(const char *name)
{ int v = stringHashValue(name, ENV_HASH_SIZE);
  EnvDescr e;

  for(e = env_table[v]; e; e = e->next)
  { if ( streq(e->name, name) )
      return e;
  }

  return NULL;
}


static EnvDescr
newEnvironment(const char *name)
{ int v = stringHashValue(name, ENV_HASH_SIZE);
  EnvDescr e;

  for(e = env_table[v]; e; e = e->next)
  { if ( streq(e->name, name) )
    { e->flags = 0;
      e->arg_count = 0;
      if ( e->arguments )
      { free(e->arguments);
	e->arguments = NULL;
      }
      return e;
    }
  }

  e = malloc(sizeof(*e));
  e->name       = save_string(name);
  e->flags      = 0;
  e->arg_count  = 0;
  e->arguments  = NULL;
  e->function   = NULL;
  e->fname      = NULL;
  e->next       = env_table[v];
  env_table[v]  = e;

  return e;
}

#define skipBanks(s)	while(isspace(*s)) s++

static int
parseArgSpec(const char *fname, int lineno, char **line, CmdArg args)
{ int nargs = 0;
  char *s = *line;

  skipBanks(s);

  for(;;s++)
  { switch(*s)
    { case '[':
	args[nargs].flags = 0;
        if ( s[2] != ']' )
	{ warn(ERR_BAD_ARG_SPEC, fname, lineno);
	  return -1;
	}
	args[nargs].flags |= CA_OPTIONAL;
	goto arg_cont;
      case '{':
	args[nargs].flags = 0;
        if ( s[2] != '}' )
	{ warn(ERR_BAD_ARG_SPEC, fname, lineno);
	  return -1;
	}
      arg_cont:
	switch(s[1])
	{ case '+':
	    args[nargs].flags |= CA_TEXT;
	  case 'd':
	    args[nargs].flags |= CA_DIM;
	  case '-':
	    break;
	  default:
	    warn(ERR_BAD_ARG_SPEC, fname, lineno);
	    return -1;
	}
        nargs++;
        s += 2;
        break;
      default:
	skipBanks(s);
        *line = s;
        return nargs;
    }
  }
}


static AnyFunc
parseFuncSpec(char **line, const char **fname)
{ char *s = *line;
  char b[MAXFUNC];
  char *q = b;

  skipBanks(s);
  if ( *s != '=' )
    return NULL;
  s++;
  while(isalnum(*s))
    *q++ = *s++;
  *q = EOS;
  skipBanks(s);

  *line = s;
  *fname = (const char *)save_string(b);

  return lookupFunction(b);
}


static int
parseEnvSpec(const char *fname, int line, char *s)
{ char *f = ++s;
  char tmp;
  EnvDescr e;
  cmd_arg args[MAXCMDARGS];		/* argument-list */
  
  while(isalnum(*s))
    s++;
  tmp = *s;
  *s = EOS;
  e = newEnvironment(f);
  *s = tmp;
  
  if ( *s == '*' )			/* \begin{figure*} */
  { e->flags |= CMD_MODIFY;
    s++;
  }

  skipBanks(s);
  if ( *s != '}' )			/* check for } */
  { warn(ERR_BAD_ENV_SPEC, fname, line);
    return FALSE;
  } else
    s++;
					/* parse arguments */
  e->arg_count = parseArgSpec(fname, line, &s, args);
  if ( e->arg_count < 0 )
    return FALSE;
  e->arguments = malloc(e->arg_count*sizeof(cmd_arg));
  memcpy(e->arguments, args, e->arg_count*sizeof(cmd_arg));

  if ( *s == '=' )			/* =function */
  { e->function = parseFuncSpec(&s, &e->fname);
    skipBanks(s);
  }

  if ( *s != EOS && *s != '%' )
  { warn(ERR_BAD_COMMAND_SPEC, fname, line);
    return FALSE;
  }

  return TRUE;
}


static int
parseCommandSpec(const char *fname, int lineno, char *line)
{ char *s = line;

  while(isspace(*s))
    s++;

  if ( *s == '%' || *s == EOS )
    return TRUE;			/* comment-line */

  if ( *s == '{' )			/* environment */
  { return parseEnvSpec(fname, lineno, s);
  } else if ( *s == '\\' )		/* normal command */
  { char *f = ++s;
    char tmp;
    cmd_arg args[MAXCMDARGS];		/* argument-list */
    CmdDescr c;

    if ( isalnum(*s) )
    { while(isalnum(*s))
      { s++;
      }
    } else
      s++;
    tmp = *s;
    *s = EOS;
    c = newCommand(f);
    *s = tmp;

    skipBanks(s);			/* spaces after command */
    if ( *s == '*' )			/* modified */
    { c->flags |= CMD_MODIFY;
      s++;
    }

    c->arg_count = parseArgSpec(fname, lineno, &s, args);
    if ( c->arg_count < 0 )
      return FALSE;
    c->arguments = malloc(c->arg_count*sizeof(cmd_arg));
    memcpy(c->arguments, args, c->arg_count*sizeof(cmd_arg));

    if ( *s == '=' )		/* associate function */
    { c->function = parseFuncSpec(&s, &c->fname);
    }

    if ( isdigit(*s) )		/* pre-lines */
    { c->pre_lines = *s - '0';
      s++;
      skipBanks(s);
    } else if ( *s == '%' )
    { c->pre_lines = PRE_COMMENT;	/* %\n */
      s++;
      skipBanks(s);
    }

    if ( isdigit(*s) )		/* post-lines */
    { c->post_lines = *s - '0';
      s++;
      skipBanks(s);
    }

    if ( *s != EOS && *s != '%' )
    { warn(ERR_BAD_COMMAND_SPEC, fname, lineno);
      return FALSE;
    }

    return TRUE;
  }

  warn(ERR_BAD_COMMAND_SPEC, fname, lineno);
  return FALSE;
}


static int
parseCmdSpecs(const char *fname)
{ char line[MAXCMD];
  int l = 0;
  Input fd;

  if ( (fd = openInputFile(fname)) == NULL )
  { warn(ERR_NOCMD_SPECS, fname, 0);
    return FALSE;
  }

  while(fgets(line, sizeof(line), fd))
    parseCommandSpec(fname, ++l, line);
  
  closeInput(fd);

  return TRUE;
}


		 /*******************************
		 *	   PARSING STUFF	*
		 *******************************/

static void
getCommand(Input fd, char *buf, int size)
{ int c;

  size--;				/* room for EOS */

  c = getc(fd);
  if ( isalnum(c) )			/* \blabla */
  { do
    { if ( --size <= 0 )
	error(ERR_CMD_TOO_LONG, texfile(), texline());
      *buf++ = c;
      c = getc(fd);
    } while(isalnum(c));
    ungetc(c, fd);
  } else				/* \" */
  { *buf++ = c;
  }

  *buf = EOS;
}


static void
getArgument(Input fd, int flags, char *buf, int size)
{ int c = getc(fd);
  int sz = size;

  if ( !(flags & F_NOSKIPBLANK) )
  { while(isspace(c))
      c = getc(fd);
  }

  if ( isbegingroup(c) )		/* { */
  { int nesting = 1; char *s = buf;
    int prev = c;
    
    for(;;)
    { c = getc(fd);

      switch(CharType(c))
      { case CM:
	  *s++ = c;
	  continue;
	case BG:
	  nesting++;
	  break;
	case EG:
	  nesting--;
	  break;
	case SP:
	  while(isspace(c))
	    c = getc(fd);
	  ungetc(c, fd);
	  c = ' ';
	  break;
	case EF:
	  error(ERR_UNEXPECTED_EOF, texfile(), texline());
      }

      if ( nesting > 0 )
      { if ( --sz < 0 )
	{ buf[size-1] = EOS;
	  texarg = buf;
	  error(ERR_RUNAWAY_ARGUMENT, texfile(), texline());
	}
	*s++ = c;
	prev = c;
      } else
	break;
    }

    *s++ = EOS;
  } else if ( iscommand(c) )		/* \ */
  { *buf++ = c;
    size--;
    getCommand(fd, buf, size);
  } else
  { *buf++ = c;
    *buf = EOS;
  }
}


static int
getOptionalArgument(Input fd, int flags, char *buf, int size)
{ int c = getc(fd);
  int sz = size;

  if ( c == '[' )
  { int nesting = 1; char *s = buf;
    int prev = c;

    for(;;)
    { c = getc(fd);

      switch(CharType(c))
      { case CM:
	  *s++ = c;
	  continue;
	case BG:
	  nesting++;
	  break;
	case EG:
	  nesting--;
	  break;
	case EF:
	  error(ERR_UNEXPECTED_EOF, texfile(), texline());
	default:
	  switch(c)
	  { case '[':
	      nesting++;
	      break;
	    case ']':
	      nesting--;
	      break;
	  }
      }

      if ( c != ']' || nesting > 0 )
      { if ( --sz < 0 )
	{ buf[size-1] = EOS;
	  texarg = buf;
	  error(ERR_RUNAWAY_ARGUMENT, texfile(), texline());
	}
	*s++ = c;
	prev = c;
      } else
	break;
    }

    *s++ = EOS;
    return TRUE;
  } else
    ungetc(c, fd);

  return FALSE;
}


static void
getDimension(Input fd, int flags, char *buf, int size)
{ int c = getc(fd);

  if ( !(flags & F_NOSKIPBLANK) )
  { while(isspace(c))
      c = getc(fd);
  }

  if ( isbegingroup(c) )
  { ungetc(c, fd);

    getArgument(fd, flags, buf, size);
  } else if ( iscommand(c) )
  { buf[0] = c;
    getCommand(fd, &buf[1], size-1);
  } else if ( isdigit(c) )
  { char *s = buf;
    do
    { *s++ = c;
      c = getc(fd);
    } while(isdigit(c) || c == '.' );
    if ( isalnum(c) )
    { *s++ = c;
      c = getc(fd);
    }
    if ( isalnum(c) )
    { *s++ = c;
      *s = EOS;
      return;
    }

    error(ERR_BAD_DIM, texfile(), texline());
  }
}


static void
parseCommand(Input fd, const char *name, CallBack func, void *ctx)
{ CmdDescr cmd = lookupCommand(&name[1]); /* skip \ */
  command g;
  token t;
  int n, c;
  int flags = 0;

  if ( !cmd )
  { fprintf(stderr, "[WARNING: Unknown command: %s]\n", name);
    cmd = newCommand(&name[1]);
  }

  g.command = cmd;
  g.flags   = 0;

  if ( cmd->name[1] == EOS && !isalnum(cmd->name[0]) )
    flags |= F_NOSKIPBLANK;

  c = getc(fd);
  if ( cmd->arg_count > 0 )
    g.arguments = alloca(sizeof(char *) * cmd->arg_count);
  else
    g.arguments = NULL;

  if ( !(flags & F_NOSKIPBLANK) )
  { while(isspace(c))
      c = getc(fd);
  }

  if ( cmd->flags & CMD_MODIFY && c == '*' ) /* \section* (modified) */
  { g.flags |= CMD_MODIFY;
    c = getc(fd);
    if ( !(flags & F_NOSKIPBLANK) )
    { while(isspace(c))
	c = getc(fd);
    }
  }
  ungetc(c, fd);

  for(n=0; n<cmd->arg_count; n++)	/* process the arguments */
  { char abuf[MAXARG];

    if ( cmd->arguments[n].flags & CA_OPTIONAL )
    { if ( getOptionalArgument(fd, flags, abuf, sizeof(abuf)) )
      { g.arguments[n] = alloca(strlen(abuf)+1);
	strcpy(g.arguments[n], abuf);
      } else
	g.arguments[n] = NULL;
    } else if ( cmd->arguments[n].flags & CA_DIM )
    { getDimension(fd, flags, abuf, sizeof(abuf));
      g.arguments[n] = alloca(strlen(abuf)+1);
      strcpy(g.arguments[n], abuf);
    } else
    { getArgument(fd, flags, abuf, sizeof(abuf));
      g.arguments[n] = alloca(strlen(abuf)+1);
      strcpy(g.arguments[n], abuf);
    }
  }

  if ( cmd->function )
  { (*cmd->function)(&g, fd, func, ctx);
  } else
  { t.type = TOK_CMD;
    t.value.cmd = &g;
    (*func)(&t, ctx);
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handle verbatim environment
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
env_verbatim(Environment e, Input fd, CallBack func, void *ctx)
{ char end[MAXCMD];
  char buf[MAXVERBATIM];
  int left = MAXVERBATIM-1;
  char *s = buf;
  int el;
  token t;

  sprintf(end, "\\end{%s}", e->environment->name);
  el = strlen(end);
  
  for(;;)
  { if ( --left == 0 )
      error(ERR_VERBATIM_TOO_LONG, texfile(), texline());
    *s++ = getc(fd);

    if ( s[-el] == '\\' && strncmp(&s[-el], end, el) == 0 )
    { s[-el] = EOS;
      t.type = TOK_VERBATIM;
      t.context = (char *)e->environment->name;
      t.value.string = buf;
      (*func)(&t, ctx);
      return;
    }
  }
}


static void
env_normal(Environment e, Input fd, CallBack func, void *ctx)
{ token t;

  t.type = TOK_BEGIN_ENV;
  t.value.env = e;
  (*func)(&t, ctx);
}


static void
cmd_normal(Command g, Input fd, CallBack func, void *ctx)
{ token t;

  t.type = TOK_CMD;
  t.value.cmd = g;
  (*func)(&t, ctx);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle \begin command
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
cmd_begin(Command g, Input fd, CallBack func, void *ctx)
{ char ename[MAXCMD];
  int enl, n;
  char *args[MAXCMDARGS];
  environment e;
  EnvDescr env;
  token t;

  e.flags = 0;
  e.arguments = args;
  getArgument(fd, 0, ename, sizeof(ename));
  enl = strlen(ename);

  if ( ename[enl-1] == '*' )		/* check for modified env */
  { ename[enl-1] = EOS;
    e.flags |= CMD_MODIFY;
  }

					/* find the environment */
  if ( !(env = lookupEnvironment(ename)) )
  { fprintf(stderr, "WARNING: undefined environment: %s\n", ename);
    env = newEnvironment(ename);
  }
  e.environment = env;

  for(n=0; n<env->arg_count; n++)	/* process the arguments */
  { char abuf[MAXARG];

    if ( env->arguments[n].flags & CA_OPTIONAL )
    { if ( getOptionalArgument(fd, 0, abuf, sizeof(abuf)) )
      { e.arguments[n] = alloca(strlen(abuf)+1);
        strcpy(e.arguments[n], abuf);
      } else
	e.arguments[n] = NULL;
    } else
    { getArgument(fd, 0, abuf, sizeof(abuf));
      e.arguments[n] = alloca(strlen(abuf)+1);
      strcpy(e.arguments[n], abuf);
    }
  }

  if ( env->function )
  { (*env->function)(&e, fd, func, ctx);
  } else
  { t.type = TOK_BEGIN_ENV;
    t.value.env = &e;
    (*func)(&t, ctx);
  }
}


static void
cmd_end(Command g, Input fd, CallBack func, void *ctx)
{ token t;

  t.type = TOK_END_ENV;
  t.value.string = g->arguments[0];	/* name of the environment */
  (*func)(&t, ctx);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle \verb command
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
cmd_verb(Command g, Input fd, CallBack func, void *ctx)
{ char buf[MAXVERB];
  char *s = buf;
  int delim = getc(fd);
  int c = getc(fd);
  token t;
  char ds[2];

  ds[0] = delim;
  ds[1] = EOS;
  while(c != delim && c != EOF)
  { *s++ = c;
    c = getc(fd);
  }
  if ( c == EOF )
    error(ERR_UNEXPECTED_EOF, texfile(), texline());
  *s++ = EOS;

  t.type = TOK_VERB;
  t.context = ds;
  t.value.string  = buf;
  (*func)(&t, ctx);
}

#define ACTIVE(n, f) { n, f }

typedef struct
{ char	  *name;			/* name */
  AnyFunc  function;			/* associated function */
} active, *Active;

static active active_list[] =
{ ACTIVE("verb",	cmd_verb),
  ACTIVE("begin",	cmd_begin),
  ACTIVE("end",		cmd_end),
  ACTIVE("item",	cmd_normal),
  ACTIVE("prolog",	cmd_prolog),
  ACTIVE("verbatim",	env_verbatim),
  ACTIVE("list",	env_normal),
  ACTIVE("float",	env_normal),
  ACTIVE(NULL,		NULL)
};

static AnyFunc
lookupFunction(const char *name)
{ Active a = active_list;

  for( ; a->name; a++ )
  { if ( streq(name, a->name) )
      return a->function;
  }

  error(ERR_UNDEF_FUNCTION, texfile(), texline());
  return NULL;
}


static void
parseMath(Input fd, CallBack func, void *ctx)
{ int c = getc(fd);
  char buf[MAXMATH];
  char *s = buf;
  token t;
  int nesting = 0;
  int left = MAXMATH-1;

  if ( CharType(c) == MM )		/* $$ */
  { t.type = TOK_MATH_ENV;
  } else
  { t.type = TOK_MATH;
    ungetc(c, fd);
  }

  for(;;)
  { c = getc(fd);

    switch(CharType(c))
    { case BG:
	nesting++;
	break;
      case EG:
	nesting--;
	break;
      case SP:
	while(isspace(c))
	  c = getc(fd);
	ungetc(c, fd);
	c = ' ';
	break;
      case EF:
	error(ERR_UNEXPECTED_EOF, texfile(), texline());
    }

    if ( c != '$' || nesting > 0 )
    { if ( --left < 0 )
      { texarg = buf;
	error(ERR_RUNAWAY_MATH, texfile(), texline());
      }
      *s++ = c;
    } else
      break;
  }

  *s = EOS;
  if ( t.type == TOK_MATH_ENV )
  { c = getc(fd);
    if ( c != '$' )
      error(ERR_BAD_MATH_ENV_CLOSURE, texfile(), texline());
  }
  
  t.value.string = buf;
  (*func)(&t, ctx);
}


static void
parseTeX(Input fd, CallBack func, void *ctx)
{ token t;

  int c = getc(fd);

  for(;;)
  { switch(CharType(c))
    { case SP:				/* blank space */
      { int lines = 0;
  
	do
	{ if ( c == '\n' )
	    lines++;
	  c = getc(fd);
	} while(isspace(c));
  
	if ( lines >= 2 )
	{ t.type = TOK_PAR;
	  t.value.string = NULL;

	  (*func)(&t, ctx);
	} else if ( lines == 1 )
	{ t.type = TOK_LINE;
	  t.value.string = NULL;

	  (*func)(&t, ctx);
	} else
	{ t.type = TOK_SPACE;
	  t.value.string = NULL;

	  (*func)(&t, ctx);
	}

	break;
      }
      case BG:
      { char buf[2];
	
	buf[0] = c;
	buf[1] = EOS;

	t.type = TOK_BEGIN_GROUP;
	t.value.string = buf;
	(*func)(&t, ctx);
	c = getc(fd);

	break;
      }
      case EG:
      { char buf[2];
	
	buf[0] = c;
	buf[1] = EOS;

	t.type = TOK_END_GROUP;
	t.value.string = buf;
	(*func)(&t, ctx); c = getc(fd);

	break;
      }
      case CM:				/* \command */
      { char buf[MAXCMD];

	buf[0] = c;
	getCommand(fd, &buf[1], MAXCMD-1);

	parseCommand(fd, buf, func, ctx);
	c = getc(fd);

	break;
      }
      case MM:
      { parseMath(fd, func, ctx);
 	c = getc(fd);

        break;
      }
      case SC:				/* % comment */
      { do
	{ c = getc(fd);
	} while( c != EOF && c != '\n' );

	while(isspace(c))
	  c = getc(fd);

	break;
      }
      case EF:				/* end-of-file */
      { t.type = TOK_EOF;
	t.value.string = NULL;
	(*func)(&t, ctx);

	return;
      }
      case TD:				/* & */
      case NB:				/* ~ */
      { char buf[2];

	buf[0] = c;
	buf[1] = EOS;
	t.type = TOK_WORD;
	t.value.string = buf;
	(*func)(&t, ctx);
	c = getc(fd);

	break;
      }
      default:				/* default: begin a word */
      { char buf[MAXWORD];
	char *s = buf;

	do
	{ *s++ = c;
	  c = getc(fd);
	} while(!wbreak(c));
	*s = EOS;
	t.type = TOK_WORD;
	t.value.string = buf;
	(*func)(&t, ctx);

	break;
      }
    }
  }
}


		 /*******************************
	         *           MAIN LOOP          *
		 *******************************/

static int
parseTeXFile(const char *file, CallBack func, void *ctx)
{ Input fd;

  if ( (fd = openInputFile(file)) == NULL )
  { fprintf(stderr, "Can't open %s: %s", file, strerror(errno));

    return FALSE;
  }
  parseTeX(fd, func, ctx);

  closeInput(fd);

  return TRUE;
}



		 /*******************************
  		 *	    HTML OUTPUT         *
		 *******************************/

#define VERB_NORMAL	0
#define VERB_VERBATIM	1
#define VERB_PRE	2

typedef struct
{ int	envnesting;			/* nesting of begin/end */
  int   last_type;			/* type of previous token */
  int	line_pos;			/* position in line */
  int   newlines;			/* # consecutive newlines */
  int	spaces;				/* # consecutive spaces */
  int	verbatim;			/* verbatim output */
  int   left_margin;			/* left margin for text */
  int   right_margin;			/* right margin for text */
  FILE *fd;				/* output descriptor */
} ppcontext, *PPContext;


static void
output(PPContext pp, const char *fmt, ...)
{ va_list args;
  char buf[MAXOUTPUT];
  char *s = buf;

  va_start(args, fmt);
  vsprintf(buf, fmt, args);
  va_end(args);

  if ( pp->verbatim )
  { for(;;s++)
    { int c;
  
      switch((c = *s))
      { case EOS:
	  return;
	case '\n':
	  putc(c, pp->fd);
	  pp->line_pos = 0;
	  pp->spaces++;
	  break;
	case '\t':
	  putc(c, pp->fd);
	  pp->line_pos |= 0x7;
	  pp->line_pos++;
	  pp->spaces++;
	  break;
	case ' ':
	  pp->spaces++;
	  putc(c, pp->fd);
	  pp->line_pos++;
	  break;
	default:
	  pp->spaces = 0;
	  if ( pp->verbatim == VERB_PRE	)
	  { switch(c)
	    { case '<':
		fputs("&lt;", pp->fd);
	        break;
	      case '>':
		fputs("&gt;", pp->fd);
	        break;
	      case '&':
		fputs("&amp;", pp->fd);
	        break;
	      default:
		putc(c, pp->fd);
	    }
	  } else
	    putc(c, pp->fd);
	  pp->line_pos++;
      }
    }
  } else
  { for(;;s++)
    { int c;
  
      switch((c = *s))
      { case EOS:
	  return;
	case '\n':
	  if ( ++pp->newlines <= 2 )
	    putc(c, pp->fd);
	  pp->line_pos = 0;
	  pp->spaces = 1;
	  break;
	case '\t':
	  c = ' ';
	case ' ':
	  pp->newlines = 0;
	  if ( ++pp->spaces <= 1 )
	  { putc(c, pp->fd);
	    pp->line_pos++;
	  }
	  break;
	default:
	  pp->newlines = 0;
	  pp->spaces = 0;
	  putc(c, pp->fd);
	  pp->line_pos++;
      }
    }
  }
}


static void
nl(PPContext pp)
{ int spaces = pp->left_margin % 8;
  int tabs   = pp->left_margin / 8;
  int n;

  output(pp, "\n");
  for(n=0; n<tabs; n++)
    output(pp, "\t");
  for(n=0; n<spaces; n++)
    output(pp, " ");
}


void
outputBlank(PPContext pp)
{ switch(pp->last_type)
  { case TOK_SPACE:
      if ( pp->newlines == 0 )
	output(pp, " ");
      break;
    case TOK_LINE:
      if ( pp->newlines < 1 )
	output(pp, "\n");
      break;
  }
}


static char *tok_names[] =
{ "CMD", "BG", "EG", "BE", "EE", "VERB",
  "VERBATIM", "$", "$$", "PAR", "W", "S", "L", "EOF"
};

void
put_token(Token t, void *ctx)
{ PPContext pp = ctx;
  static CmdDescr CMD_BEGIN, CMD_END;

  if ( !CMD_BEGIN )
  { CMD_BEGIN = lookupCommand("begin");
    CMD_END   = lookupCommand("end");
  }

  DEBUG(1, output(pp, "[%s]", tok_names[t->type]));

  switch(t->type)
  { case TOK_CMD:
    { Command g = t->value.cmd;
      int n;
      int args_printed = 0;

      outputBlank(pp);
      if ( g->command->pre_lines == PRE_COMMENT )
      { output(pp, "%\n");
      } else
      { while(pp->newlines < g->command->pre_lines)
	  output(pp, "\n");
      }
      output(pp, "\\%s", g->command->name);
      if ( g->flags & CMD_MODIFY )
	output(pp, "*");
      for(n=0; n<g->command->arg_count; n++)
      { if ( g->command->arguments[n].flags & CA_OPTIONAL )
	{ if ( g->arguments[n] )
	  { output(pp, "[%s]", g->arguments[n]);
	    args_printed++;
	  }
	} else
	{ output(pp, "{%s}", g->arguments[n]);
	  args_printed++;
	}
      }
      if ( !args_printed )
      { if ( isalnum(g->command->name[strlen(g->command->name)-1]) )
	  output(pp, " ");
      }
      while(pp->newlines < g->command->post_lines)
	output(pp, "\n");

      break;
    }
    case TOK_BEGIN_ENV:
    { Environment e = t->value.env;
      int n;

      outputBlank(pp);
      while(pp->newlines < CMD_BEGIN->pre_lines)
	output(pp, "\n");
      output(pp, "\\begin{%s", e->environment->name);
      if ( e->flags & CMD_MODIFY )
	output(pp, "*");
      output(pp, "}");
      for(n=0; n<e->environment->arg_count; n++)
      { if ( e->environment->arguments[n].flags & CA_OPTIONAL )
	{ if ( e->arguments[n] )
	    output(pp, "[%s]", e->arguments[n]);
	} else
	{ output(pp, "{%s}", e->arguments[n]);
	}
      }
      while(pp->newlines < CMD_BEGIN->post_lines)
	output(pp, "\n");

      break;
    }
    case TOK_END_ENV:
    { outputBlank(pp);
      while(pp->newlines < CMD_END->pre_lines)
	output(pp, "\n");
      output(pp, "\\begin{%s}", t->value.string);
      while(pp->newlines < CMD_END->post_lines)
	output(pp, "\n");

      break;
    }
    case TOK_BEGIN_GROUP:
      outputBlank(pp);
      output(pp, "{");
      break;
    case TOK_END_GROUP:
      outputBlank(pp);
      output(pp, "}");
      break;
    case TOK_MATH:
      outputBlank(pp);
      output(pp, "$%s$", t->value.string);
      break;
    case TOK_MATH_ENV:
      outputBlank(pp);
      output(pp, "$$%s$$", t->value.string);
      break;
    case TOK_VERB:
      outputBlank(pp);
      pp->verbatim = VERB_VERBATIM;
      output(pp, "\\verb%s%s%s", t->context, t->value.string, t->context);
      pp->verbatim = VERB_NORMAL;
      break;
    case TOK_VERBATIM:

      while( pp->newlines < CMD_BEGIN->pre_lines )
	output(pp, "\n");
      output(pp, "\\begin{%s}", t->context);
      pp->verbatim = VERB_VERBATIM;
      output(pp, "%s", t->value.string);
      pp->verbatim = VERB_NORMAL;
      output(pp, "\\end{%s}", t->context);
      while( pp->newlines < CMD_BEGIN->post_lines )
	output(pp, "\n");
      break;
    case TOK_PAR:
      output(pp, "\n\n");
      break;
    case TOK_WORD:
    { int pendingblank;

      if ( pp->last_type == TOK_LINE )
	pp->last_type = TOK_SPACE;
      pendingblank = (pp->last_type == TOK_SPACE);
      outputBlank(pp);			/* as space! */
      if ( pendingblank &&		/* no blanks in input: concatenate! */
	   strlen(t->value.string) + pp->line_pos > pp->right_margin )
	nl(pp);
      output(pp, "%s", t->value.string);
      break;
    }
    case TOK_LINE:
    case TOK_SPACE:
      break;
    case TOK_EOF:
      output(pp, "\n");
      break;
    default:
      assert(0);
  }

  pp->last_type = t->type;
}

#ifdef TEST

static void
error(int eno, const char *file, int line)
{ fprintf(stderr, "ERROR: %s:%d: %s\n", file, line, tex_error_strings[eno]);

  exit(1);
}


int
main(int argc, char **argv)
{ parseCmdSpecs("cmd.spec");

  if ( argc == 2 )
  { ppcontext pp;

    pp.envnesting   = 0;
    pp.last_type    = TOK_EOF;
    pp.line_pos     = 0;
    pp.newlines     = 0;
    pp.spaces       = 0;
    pp.verbatim     = FALSE;
    pp.left_margin  = 0;
    pp.right_margin = 72;

    parseTeXFile(argv[1], put_token, &pp);
  }

  exit(0);
}

#endif /*TEST*/

#ifdef __SWI_PROLOG__

		 /*******************************
		 *	 PROLOG CONNECTION	*
		 *******************************/

#include <SWI-Prolog.h>

static void build_list(Token t, void *context);

typedef struct
{ term_t list;				/* list we are working on */
  term_t head;				/* head (tmp term ref) */
  int    envnesting;			/* depth of stack */
  int	 prev_type0;			/* type of previous token */
  int	 prev_type1;			/* type of token before that */
  term_t stack[MAXENVNESTING];		/* Pushed environment */
} pl_context, *PlContext;


static functor_t FUNCTOR_verb1;		/* verb/1 */
static functor_t FUNCTOR_verb2;		/* verb/2 */
static functor_t FUNCTOR_verbatim2;	/* verbatim/2 */
static functor_t FUNCTOR_verbatim1;	/* verbatim/1 */
static functor_t FUNCTOR_pre1;		/* pre/1 */
static functor_t FUNCTOR_dot2;		/* ./2 */
static functor_t FUNCTOR_brace1;	/* {}/1 */
static functor_t FUNCTOR_cmd1;		/* \/1 */
static functor_t FUNCTOR_cmd2;		/* \/2 */
static functor_t FUNCTOR_cmd3;		/* \/3 */
static functor_t FUNCTOR_env2;		/* env/2 */
static functor_t FUNCTOR_env3;		/* env/3 */
static functor_t FUNCTOR_env4;		/* env/4 */
static functor_t FUNCTOR_math1;		/* $/1 */
static functor_t FUNCTOR_mathenv1;	/* $$/1 */
static functor_t FUNCTOR_html1;		/* html/1 */
static functor_t FUNCTOR_html3;		/* html/3 */
static functor_t FUNCTOR_nospace1;	/* nospace/1 */
static atom_t	 ATOM_begin_group;	/* '\{' */
static atom_t	 ATOM_end_group;	/* '\}' */
static atom_t	 ATOM_nbsp;		/* '~' */
static atom_t	 ATOM_par;		/* 'par' */
static atom_t	 ATOM_star;		/* * */
static atom_t	 ATOM_minus;		/* - */
static atom_t	 ATOM_space;		/* ' ' */
static atom_t	 ATOM_nl;		/* '\n' */
static atom_t	 ATOM_nil;		/* [] */
static atom_t	 ATOM_true;		/* true */
static atom_t	 ATOM_false;		/* false */

static void
initPrologConstants()
{ FUNCTOR_verb2     = PL_new_functor(PL_new_atom("verb"), 2);
  FUNCTOR_verb1     = PL_new_functor(PL_new_atom("verb"), 1);
  FUNCTOR_verbatim2 = PL_new_functor(PL_new_atom("verbatim"), 2);
  FUNCTOR_verbatim1 = PL_new_functor(PL_new_atom("verbatim"), 1);
  FUNCTOR_pre1	    = PL_new_functor(PL_new_atom("pre"), 1);
  FUNCTOR_dot2	    = PL_new_functor(PL_new_atom("."), 2);
  FUNCTOR_brace1    = PL_new_functor(PL_new_atom("{}"), 1);
  FUNCTOR_cmd1      = PL_new_functor(PL_new_atom("\\"), 1);
  FUNCTOR_cmd2      = PL_new_functor(PL_new_atom("\\"), 2);
  FUNCTOR_cmd3      = PL_new_functor(PL_new_atom("\\"), 3);
  FUNCTOR_env2      = PL_new_functor(PL_new_atom("env"), 2);
  FUNCTOR_env3      = PL_new_functor(PL_new_atom("env"), 3);
  FUNCTOR_env4      = PL_new_functor(PL_new_atom("env"), 4);
  FUNCTOR_math1	    = PL_new_functor(PL_new_atom("$"), 1);
  FUNCTOR_mathenv1  = PL_new_functor(PL_new_atom("$$"), 1);
  FUNCTOR_html1     = PL_new_functor(PL_new_atom("html"), 1);
  FUNCTOR_html3     = PL_new_functor(PL_new_atom("html"), 3);
  FUNCTOR_nospace1  = PL_new_functor(PL_new_atom("nospace"), 1);

  ATOM_begin_group  = PL_new_atom("\\{");
  ATOM_end_group    = PL_new_atom("\\}");
  ATOM_par	    = PL_new_atom("par");
  ATOM_nbsp	    = PL_new_atom("~");
  ATOM_star	    = PL_new_atom("*");
  ATOM_minus	    = PL_new_atom("-");
  ATOM_space	    = PL_new_atom(" ");
  ATOM_nl	    = PL_new_atom("\n");
  ATOM_nil	    = PL_new_atom("[]");
  ATOM_true	    = PL_new_atom("true");
  ATOM_false	    = PL_new_atom("false");
}

static ppcontext ppctx;

static foreign_t
pl_put_tex_token(term_t term)
{ token t;
  atom_t atom;
  functor_t f;
  static int last_is_word = FALSE;
  
  t.type = -1;

  if ( PL_get_atom(term, &atom) )
  { if ( atom == ATOM_begin_group )
    { t.type = TOK_BEGIN_GROUP;
      t.value.string = "{";
    } else if ( atom == ATOM_end_group )
    { t.type = TOK_END_GROUP;
      t.value.string = "}";
    } else if ( atom == ATOM_space )
    { t.type = TOK_SPACE;
      t.value.string = " ";
    } else if ( atom == ATOM_nl )
    { t.type = TOK_LINE;
      t.value.string = "\n";
    } else
    { if ( last_is_word )		/* regenerate the space tokens */
      { t.type = TOK_SPACE;
	t.value.string = " ";
	
	put_token(&t, &ppctx);
      } else
	last_is_word = TRUE;
      t.type = TOK_WORD;
      t.value.string = (char *)PL_atom_chars(atom);
    }
  } else if ( PL_get_functor(term, &f) )
  { term_t arg = PL_new_term_ref();
    char *s;

    if ( f == FUNCTOR_verb2 || f == FUNCTOR_verbatim2 )
    { if ( PL_get_arg(1, term, arg) && PL_get_chars(arg, &s, CVT_ATOMIC) )
      { t.context = s;

	if ( PL_get_arg(2, term, arg) && PL_get_chars(arg, &s, CVT_ATOMIC) )
	{ t.value.string = s;
	  t.type = (f == FUNCTOR_verb2 ? TOK_VERB : TOK_VERBATIM);
	}
      }
    } else if ( f == FUNCTOR_cmd1 )
    { char *cname;

      if ( PL_get_arg(1, term, arg) && PL_get_chars(arg, &cname, CVT_ATOMIC) )
      { command g;

	t.type = TOK_CMD;
	t.value.cmd = &g;
	g.flags = 0;
	g.arguments = 0;

	g.command = lookupCommand(cname);
	if ( !g.command )
	{ fprintf(stderr, "[WARNING: Undefined command: %s]\n", cname);
	  g.command = newCommand(cname);
	}
      }
    } else if ( f == FUNCTOR_cmd2 || f == FUNCTOR_cmd3 ||
		f == FUNCTOR_env2 || f == FUNCTOR_env3 )
    { char *cname;
      command g;
      environment e;
      int isenv = (f == FUNCTOR_env2 || f == FUNCTOR_env3);
      int ismod = (f == FUNCTOR_cmd3 || f == FUNCTOR_env3);
      term_t alist = PL_new_term_ref();

      if ( isenv )
      { t.type = TOK_BEGIN_ENV;
	t.value.env = &e;
	e.flags = 0;
      } else
      { t.type = TOK_CMD;
	t.value.cmd = &g;
	g.flags = 0;
      }

      if ( ismod )
      { PL_get_arg(2, term, arg);
	if ( PL_get_atom(arg, &atom) && atom == ATOM_star )
	{ if ( isenv )
	    e.flags |= CMD_MODIFY;
	  else
	    g.flags |= CMD_MODIFY;
	}
	PL_get_arg(3, term, alist);
      } else
	PL_get_arg(2, term, alist);

      PL_get_arg(1, term, arg);
      if ( PL_get_atom_chars(arg, &cname) )
      { int n;
	term_t a2 = PL_new_term_ref();
	int argn;
	char **args;

	if ( isenv )
	{ e.environment = lookupEnvironment(cname);
	  if ( !e.environment )
	  { fprintf(stderr, "[WARNING: Undefined environment: %s]\n", cname);
	    e.environment = newEnvironment(cname);
	  }
	  argn = e.environment->arg_count;
	  args = e.arguments = alloca(sizeof(cmd_arg)*argn);
	} else
	{ g.command = lookupCommand(cname);
	  if ( !g.command )
	  { fprintf(stderr, "[WARNING: Undefined command: %s]\n", cname);
	    g.command = newCommand(cname);
	  }
	  argn = g.command->arg_count;
	  args = g.arguments = alloca(sizeof(cmd_arg)*argn);
	}

	for(n=0; n<argn; n++)
	{ if ( PL_get_list(alist, arg, alist) &&
	       PL_get_arg(1, arg, a2) &&
	       PL_get_chars(a2, &s, CVT_ATOMIC) )
	    args[n] = s;
	  else
	    args[n] = NULL;
	}
      }
    } else if ( f == FUNCTOR_math1 || f == FUNCTOR_mathenv1 )
    { term_t arg = PL_new_term_ref();
      char *s;

      if ( PL_get_arg(1, term, arg) &&
	   PL_get_chars(arg, &s, CVT_ATOMIC) )
      { t.type = (f == FUNCTOR_math1 ? TOK_MATH : TOK_MATH_ENV);
	t.value.string = s;
      }
    }
  }

  if ( t.type != TOK_WORD )
    last_is_word = FALSE;

  if ( t.type >= 0 )
  { put_token(&t, &ppctx);
    PL_succeed;
  }

  return PL_warning("put_tex_token/1: instantiation error");
}


static void
tex2pl_from_string(const char *str, term_t tokens)
{ pl_context ctx;
  Input fd;

  ctx.list       = PL_copy_term_ref(tokens);
  ctx.head       = PL_new_term_ref();
  ctx.envnesting = 0;
  ctx.prev_type0 = TOK_EOF;
  ctx.prev_type1 = TOK_EOF;

  fd = openInputString(str);
  parseTeX(fd, build_list, &ctx);
  closeInput(fd);
}


static foreign_t
pl_tex_atom_to_tokens(term_t txt, term_t tokens)
{ char *s;

  if ( PL_get_chars(txt, &s, CVT_ALL) )
  { tex2pl_from_string(s, tokens);

    PL_succeed;
  }

  PL_fail;
}



static void
build_arguments(term_t alist, int nargs, CmdArg argspec, char **args)
{ int ga = 0;				/* goal argument */
  term_t tmp = PL_new_term_ref();

  for( ; ga < nargs; ga++ )
  { PL_unify_list(alist, tmp, alist);

    if ( argspec[ga].flags & CA_OPTIONAL )
    { if ( args[ga] == NULL )
      { PL_unify_atom(tmp, ATOM_nil);
      } else
      { if ( argspec[ga].flags & CA_TEXT )
	{ term_t arg = PL_new_term_ref();

	  tex2pl_from_string(args[ga], arg);
	  PL_unify_term(tmp,		/* [text] */
			PL_FUNCTOR, FUNCTOR_dot2,
			PL_TERM,    arg,
			PL_ATOM,    ATOM_nil);
	} else
	{ PL_unify_term(tmp,		/* [text] */
			PL_FUNCTOR, FUNCTOR_dot2,
			PL_CHARS,   args[ga],
			PL_ATOM,    ATOM_nil);
	}
      }
    } else
    { if ( argspec[ga].flags & CA_TEXT )
      { term_t arg = PL_new_term_ref();

	tex2pl_from_string(args[ga], arg);
	PL_unify_term(tmp,		/* {text} */
		      PL_FUNCTOR, FUNCTOR_brace1,
		      PL_TERM,   arg);

      } else
      { PL_unify_term(tmp,		/* {text} */
		      PL_FUNCTOR, FUNCTOR_brace1,
		      PL_CHARS,   args[ga]);
      }
    }
  }

  PL_unify_nil(alist);
}


static void
popStack(PlContext ctx)
{ if ( ctx->envnesting > 0 )
    ctx->list = ctx->stack[--ctx->envnesting];
  else
    error(ERR_ENV_UNDERFLOW, texfile(), texline());
}


static void
build_list(Token t, void *context)
{ PlContext ctx = context;

  DEBUG(1, put_token(t, &ppctx));

  switch(t->type)
  { case TOK_EOF:
      PL_unify_nil(ctx->list);
      return;
    case TOK_SPACE:
    case TOK_LINE:
      if ( !emit_space )
	goto out;
  }

  if ( !emit_space )
  { if ( (ctx->prev_type0 == TOK_SPACE || ctx->prev_type0 == TOK_LINE) &&
	 (ctx->prev_type1 != TOK_WORD || t->type != TOK_WORD) )
    { atom_t a = (ctx->prev_type0 == TOK_SPACE ? ATOM_space : ATOM_nl);
    
      PL_unify_list(ctx->list, ctx->head, ctx->list);
      PL_unify_atom(ctx->head, a);
    }
  }

  switch(t->type)
  { case TOK_END_GROUP:
    case TOK_END_ENV:
      PL_unify_nil(ctx->list);
      popStack(ctx);
      goto out;
  }

  PL_unify_list(ctx->list, ctx->head, ctx->list);

  switch(t->type)
  { case TOK_BEGIN_ENV:
    { Environment e   = t->value.env;
      atom_t modified = (e->flags & CMD_MODIFY ? ATOM_star : ATOM_minus);
      term_t clist    = PL_new_term_ref();
      term_t alist    = PL_new_term_ref();

      if ( e->environment->flags & CMD_MODIFY )
      { PL_unify_term(ctx->head,
		      PL_FUNCTOR, FUNCTOR_env4,
		      PL_CHARS,   e->environment->name,
		      PL_ATOM,	  modified,
		      PL_TERM,	  alist,
		      PL_TERM,	  clist);
      } else
      { PL_unify_term(ctx->head,
		      PL_FUNCTOR, FUNCTOR_env3,
		      PL_CHARS,   e->environment->name,
		      PL_TERM,	  alist,
		      PL_TERM,	  clist);
      }

      build_arguments(alist,		/* environment arguments */
		      e->environment->arg_count,
		      e->environment->arguments,
		      e->arguments);

      PL_reset_term_refs(alist);
					/* contents of the environment */
      if ( ctx->envnesting >= MAXENVNESTING )
	error(ERR_ENV_NESTING, texfile(), texline());
      ctx->stack[ctx->envnesting++] = ctx->list;
      ctx->list = clist;		/* no need to copy */

      break;
    }
    case TOK_CMD:
    { Command g       = t->value.cmd;
      term_t alist    = PL_new_term_ref();
      term_t modified = (g->flags & CMD_MODIFY ? ATOM_star : ATOM_minus);
      
      if ( g->command->flags & CMD_MODIFY )
      { PL_unify_term(ctx->head,
		      PL_FUNCTOR, FUNCTOR_cmd3,
		      PL_CHARS,   g->command->name,
		      PL_ATOM,	  modified,
		      PL_TERM,	  alist);
      } else
      { if ( g->command->arg_count == 0 )
	{ PL_unify_term(ctx->head,
			PL_FUNCTOR, FUNCTOR_cmd1,
			PL_CHARS,   g->command->name);
	  PL_reset_term_refs(alist);

	  break;
	} else
	{ PL_unify_term(ctx->head,
			PL_FUNCTOR, FUNCTOR_cmd2,
			PL_CHARS,   g->command->name,
			PL_TERM,    alist);
	}
      }

      build_arguments(alist,
		      g->command->arg_count,
		      g->command->arguments,
		      g->arguments);

      PL_reset_term_refs(alist);
      break;
    }
    case TOK_BEGIN_GROUP:
      if ( ctx->envnesting >= MAXENVNESTING )
	error(ERR_ENV_NESTING, texfile(), texline());
      ctx->stack[ctx->envnesting++] = ctx->list;
      ctx->list = PL_copy_term_ref(ctx->head);
      break;
    case TOK_MATH:
      PL_unify_term(ctx->head,
		    PL_FUNCTOR, FUNCTOR_math1,
		    PL_STRING,  t->value.string);
      break;
    case TOK_MATH_ENV:
      PL_unify_term(ctx->head,
		    PL_FUNCTOR, FUNCTOR_mathenv1,
		    PL_STRING,  t->value.string);
      break;
    case TOK_VERB:
      PL_unify_term(ctx->head,
		    PL_FUNCTOR, FUNCTOR_verb2,
		    PL_CHARS,	t->context,
		    PL_STRING,  t->value.string);
      break;
    case TOK_VERBATIM:
      PL_unify_term(ctx->head,
		    PL_FUNCTOR, FUNCTOR_verbatim2,
		    PL_CHARS,	t->context,
		    PL_STRING,  t->value.string);
      break;
    case TOK_PAR:
      PL_unify_term(ctx->head,
		    PL_FUNCTOR, FUNCTOR_cmd1,
		    PL_ATOM,	ATOM_par);
      break;
    case TOK_WORD:
      PL_unify_atom_chars(ctx->head, t->value.string);
      break;
    case TOK_SPACE:
    case TOK_LINE:
      PL_unify_atom(ctx->head, ATOM_space);
      break;
  }

out:
  ctx->prev_type1 = ctx->prev_type0;
  ctx->prev_type0 = t->type;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Calls tex:prolog_function(cmd([Star], [Args]))
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
cmd_prolog(Command g, Input fd, CallBack func, void *ctx)
{ fid_t  f        = PL_open_foreign_frame();
  term_t t0       = PL_new_term_ref();
  term_t alist    = PL_new_term_ref();
  term_t modified = (g->flags & CMD_MODIFY ? ATOM_star : ATOM_minus);
  predicate_t p   = PL_predicate("prolog_function", 1, "tex");

  if ( g->command->flags & CMD_MODIFY )
  { PL_unify_term(t0,
		  PL_FUNCTOR, FUNCTOR_cmd3,
		  PL_CHARS,   g->command->name,
		  PL_ATOM,    modified,
		  PL_TERM,    alist);
  } else
  { if ( g->command->arg_count == 0 )
    { PL_unify_term(t0,
		    PL_FUNCTOR, FUNCTOR_cmd1,
		    PL_CHARS,   g->command->name);
    } else
    { PL_unify_term(t0,
		    PL_FUNCTOR, FUNCTOR_cmd2,
		    PL_CHARS,   g->command->name,
		    PL_TERM,    alist);
    }
  }

  build_arguments(alist,
		  g->command->arg_count,
		  g->command->arguments,
		  g->arguments);

  PL_call_predicate(NULL, TRUE, p, t0);
  PL_discard_foreign_frame(f);

  cmd_normal(g, fd, func, ctx);
}


foreign_t
pl_tex_emit_spaces(term_t old, term_t new)
{ if ( PL_unify_atom(old, emit_space ? ATOM_true : ATOM_false) )
  { atom_t a;

    if ( PL_get_atom(new, &a) )
    { if ( a == ATOM_true )
      { emit_space = 1;
	return TRUE;
      } else
      { emit_space = 0;
	return TRUE;
      }
    }
  }

  return FALSE;
}


foreign_t
pl_tex_tokens(term_t file, term_t tokens)
{ char *fname;

  if ( PL_get_chars(file, &fname, CVT_ALL) )
  { pl_context ctx;

    ctx.list       = tokens;
    ctx.head       = PL_new_term_ref();
    ctx.envnesting = 0;
    ctx.prev_type0 = TOK_EOF;
    ctx.prev_type1 = TOK_EOF;

    parseTeXFile(fname, build_list, &ctx);
    PL_succeed;
  }

  PL_fail;
}


foreign_t
pl_tex_command_property(term_t name, term_t pre, term_t post)
{ char *cname;

  if ( PL_get_atom_chars(name, &cname) )
  { CmdDescr cmd = lookupCommand(cname);

    if ( cmd &&
	 PL_unify_integer(pre, cmd->pre_lines) &&
         PL_unify_integer(post, cmd->post_lines) )
      PL_succeed;
  }

  PL_fail;
}


foreign_t
pl_tex_debug(term_t old, term_t new)
{ if ( PL_unify_integer(old, debuglevel) &&
       PL_get_integer(new, &debuglevel) )
    PL_succeed;

  PL_fail;
}


foreign_t
pl_tex_tell(term_t file)
{ char *name;

  if ( PL_get_chars(file, &name, CVT_ALL) )
  { FILE *fd = (streq(name, "-") ? stdout : fopen(name, "w"));
    
    if ( fd )
    { ppctx.envnesting   = 0;		/* seperate predicate? */
      ppctx.last_type    = TOK_EOF;
      ppctx.line_pos     = 0;
      ppctx.newlines     = 0;
      ppctx.spaces       = 0;
      ppctx.verbatim     = FALSE;
      ppctx.left_margin  = 0;
      ppctx.right_margin = 72;
      ppctx.fd	         = fd;

      PL_succeed;
    }
  }

  PL_fail;
}


foreign_t
pl_tex_told()
{ fflush(ppctx.fd);
  if ( ppctx.fd != stdout )
    fclose(ppctx.fd);
  ppctx.fd = stdout;

  PL_succeed;
}


foreign_t
pl_tex_read_commands(term_t file)
{ char *name;

  if ( PL_get_chars(file, &name, CVT_ALL) &&
       parseCmdSpecs(name) )
    PL_succeed;

  PL_fail;
}


foreign_t
pl_tex_declare(term_t spec)
{ char *s;

  if ( PL_get_chars(spec, &s, CVT_ALL) &&
       parseCommandSpec("tex_declare/1", 0, s) )
    PL_succeed;

  PL_fail;
}


foreign_t
pl_tex_environment_function(term_t env, term_t func)
{ char *s;
  EnvDescr e;

  if ( PL_get_atom_chars(env, &s) &&
       (e = lookupEnvironment(s)) &&
       e->fname )
    return PL_unify_atom_chars(func, e->fname);

  PL_fail;
}


foreign_t
pl_tex_command_function(term_t cmd, term_t func)
{ char *s;
  CmdDescr c;

  if ( PL_get_atom_chars(cmd, &s) &&
       (c = lookupCommand(s)) &&
       c->fname )
    return PL_unify_atom_chars(func, c->fname);

  PL_fail;
}


		 /*******************************
		 *          HTML OUTPUT		*
		 *******************************/

static void
output_n(PPContext pp, const char *s, int l)
{ if ( l > 0 )
  { char buf[l+1];
	  
    memcpy(buf, s, l);
    buf[l] = EOS;
    output(pp, "%s", buf);
  }
}


static void
output_html(PPContext pp, const char *s)
{ int c;
  const char *from = s;

  for(; (c=*s); s++)
  { switch(c)
    { case '<':
	output_n(pp, from, s-from);
        from = s+1;
        output(pp, "%s", "&lt;");
	break;
      case '>':
	output_n(pp, from, s-from);
        from = s+1;
        output(pp, "%s", "&gt;");
	break;
      case '&':
	output_n(pp, from, s-from);
        from = s+1;
        output(pp, "%s", "&amp;");
	break;
    }
  }
    
  output_n(pp, from, s-from);
}


static void
put_html_token(Token t, void *ctx)
{ PPContext pp = ctx;

  switch(t->type)
  { case TOK_CMD:
    { outputBlank(pp);
      while(pp->newlines < t->prelines)
	output(pp, "\n");
      output(pp, "%s", t->value.string);
      while(pp->newlines < t->postlines)
	output(pp, "\n");

      break;
    }
    case TOK_VERBATIM:
    { pp->verbatim = VERB_VERBATIM;
      output(pp, "%s", t->value.string);
      pp->verbatim = VERB_NORMAL;
      break;
    }
    case TOK_PRE:
    { pp->verbatim = VERB_PRE;
      output(pp, "%s", t->value.string);
      pp->verbatim = VERB_NORMAL;
      break;
    }
    case TOK_VERB:
    { outputBlank(pp);
      pp->verbatim = VERB_VERBATIM;
      output(pp, "%s", t->value.string);
      pp->verbatim = VERB_NORMAL;
      
      break;
    }
    case TOK_SPACE:
      break;
    case TOK_LINE:
      break;
    case TOK_NOSPACEWORD:
      outputBlank(pp);
      output_html(pp, t->value.string);
      break;
    case TOK_WORD:
    { int pendingblank;

      if ( pp->last_type == TOK_LINE )
	pp->last_type = TOK_SPACE;
      pendingblank = (pp->last_type == TOK_SPACE);
      outputBlank(pp);			/* as space! */
      if ( pendingblank &&		/* no blanks in input: concatenate! */
	   strlen(t->value.string) + pp->line_pos > pp->right_margin )
	nl(pp);
      output_html(pp, t->value.string);
      break;
    }
    case TOK_EOF:
      output(pp, "\n");
      break;
    default:
      assert(0);
  }

  pp->last_type = t->type;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Control output of HTML data format:

  html(Text, [Pre, Post])	Output a command
  verbatim(Text)		Output verbatim text
  verb(Text)			Output short text
  Atom				Output plain text
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_put_html_token(term_t term)
{ token t;
  char *s;
  atom_t atom;
  static int last_is_word = FALSE;

  t.type = -1;

  if ( PL_is_functor(term, FUNCTOR_html3) )
  { term_t a = PL_new_term_ref();

    if ( PL_get_arg(1, term, a) &&
	 PL_get_chars(a, &s, CVT_ATOMIC) &&
	 PL_get_arg(2, term, a) &&
	 PL_get_integer(a, &t.prelines) &&
	 PL_get_arg(3, term, a) &&
	 PL_get_integer(a, &t.postlines) )
    { t.type = TOK_CMD;
      t.value.string = s;
    }
  } else if ( PL_is_functor(term, FUNCTOR_html1) )
  { term_t a = PL_new_term_ref();

    if ( PL_get_arg(1, term, a) &&
	 PL_get_chars(a, &s, CVT_ATOMIC) )
    { t.type = TOK_CMD;
      t.value.string = s;
      t.prelines = t.postlines = 0;
    }
  } else if ( PL_is_functor(term, FUNCTOR_verbatim1) )
  { term_t a = PL_new_term_ref();

    if ( PL_get_arg(1, term, a) &&
	 PL_get_chars(a, &s, CVT_ATOMIC) )
    { t.type = TOK_VERBATIM;
      t.value.string = s;
    }
  } else if ( PL_is_functor(term, FUNCTOR_verb1) )
  { term_t a = PL_new_term_ref();

    if ( PL_get_arg(1, term, a) &&
	 PL_get_chars(a, &s, CVT_ATOMIC) )
    { t.type = TOK_VERB;
      t.value.string = s;
    }
  } else if ( PL_is_functor(term, FUNCTOR_pre1) )
  { term_t a = PL_new_term_ref();

    if ( PL_get_arg(1, term, a) &&
	 PL_get_chars(a, &s, CVT_ATOMIC) )
    { t.type = TOK_PRE;
      t.value.string = s;
    }
  } else if ( PL_is_functor(term, FUNCTOR_nospace1) )
  { term_t a = PL_new_term_ref();

    if ( PL_get_arg(1, term, a) &&
	 PL_get_chars(a, &s, CVT_ATOMIC) )
    { t.type = TOK_NOSPACEWORD;
      t.value.string = s;
    }
  } else if ( PL_get_atom(term, &atom) )
  { if ( atom == ATOM_space )
    { t.type = TOK_SPACE;
      t.value.string = " ";
    } else if ( atom == ATOM_nl )
    { t.type = TOK_LINE;
      t.value.string = "\n";
    } else
    { if ( last_is_word )		/* regenerate the space tokens */
      { t.type = TOK_SPACE;
	t.value.string = " ";
	
	put_html_token(&t, &ppctx);
      } else
	last_is_word = TRUE;

      t.type = TOK_WORD;
      t.value.string = (char *)PL_atom_chars(atom);
    }
  } else if ( PL_get_chars(term, &s, CVT_ALL) )
  { if ( last_is_word )		/* regenerate the space tokens */
    { t.type = TOK_SPACE;
      t.value.string = " ";
	
      put_html_token(&t, &ppctx);
    } else
      last_is_word = TRUE;

    t.type = TOK_WORD;
    t.value.string = s;
  }

  if ( t.type != TOK_WORD )
    last_is_word = FALSE;

  if ( t.type >= 0 )
  { put_html_token(&t, &ppctx);
    PL_succeed;
  }

  return PL_warning("put_html_token/1: instantiation error");
}

		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static void
error(int eno, const char *file, int line)
{ fprintf(stderr,
	  "[TeX tokeniser: %s:%d: %s]\n",
	  file, line, tex_error_strings[eno]);
  switch(eno)
  { case ERR_RUNAWAY_ARGUMENT:
    case ERR_RUNAWAY_MATH:
    { char argstart[50];
      strncpy(argstart, texarg, 50);
      argstart[49] = EOS;
      fprintf(stderr, "Start: \"%s\"\n", argstart);
      break;
    }
  }

  exit(1);
}

static void
warn(int eno, const char *file, int line)
{ fprintf(stderr,
	  "WARNING: %s:%d: %s\n",
	  file, line, tex_error_strings[eno]);
}


extern void install_ps(void);

install_t
install()
{ initPrologConstants();

  PL_register_foreign("tex_tokens",           2, pl_tex_tokens,           0);
  PL_register_foreign("tex_command_property", 3, pl_tex_command_property, 0);
  PL_register_foreign("put_tex_token",        1, pl_put_tex_token,        0);
  PL_register_foreign("put_html_token",       1, pl_put_html_token,       0);
  PL_register_foreign("tex_debug",            2, pl_tex_debug,            0);
  PL_register_foreign("tex_tell",             1, pl_tex_tell,		  0);
  PL_register_foreign("tex_told",             0, pl_tex_told,             0);
  PL_register_foreign("tex_read_commands",    1, pl_tex_read_commands,	  0);
  PL_register_foreign("tex_declare",	      1, pl_tex_declare,	  0);
  PL_register_foreign("tex_environment_function",
					      2, pl_tex_environment_function,
									  0);
  PL_register_foreign("tex_command_function", 2, pl_tex_command_function, 0);
  PL_register_foreign("tex_atom_to_tokens",   2, pl_tex_atom_to_tokens,   0);
  PL_register_foreign("tex_emit_spaces",      2, pl_tex_emit_spaces,      0);

  ppctx.envnesting   = 0;		/* seperate predicate? */
  ppctx.last_type    = TOK_EOF;
  ppctx.line_pos     = 0;
  ppctx.newlines     = 0;
  ppctx.spaces       = 0;
  ppctx.verbatim     = FALSE;
  ppctx.left_margin  = 0;
  ppctx.right_margin = 72;
  ppctx.fd	     = stdout;

  install_ps();
}

#endif /*__SWI_PROLOG__*/
