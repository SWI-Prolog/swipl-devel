#ifdef MAKE_FUNCTORS

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_system_error2;

#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

static void
win_init_errors()
{ MKFUNCTOR(error, 2);
  MKFUNCTOR(system_error, 2);
}

#endif

static atom_t
WinError()
{ int id = GetLastError();
  char *msg;
  static WORD lang;
  static lang_initialised = 0;

  if ( !lang_initialised )
    lang = MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_UK);

again:
  if ( FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|
		     FORMAT_MESSAGE_IGNORE_INSERTS|
		     FORMAT_MESSAGE_FROM_SYSTEM,
		     NULL,			/* source */
		     id,			/* identifier */
		     lang,
		     (LPTSTR) &msg,
		     0,				/* size */
		     NULL) )			/* arguments */
  { atom_t a = PL_new_atom(msg);

    LocalFree(msg);
    lang_initialised = 1;

    return a;
  } else
  { if ( lang_initialised == 0 )
    { lang = MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT);
      lang_initialised = 1;
      goto again;
    }

    return PL_new_atom("Unknown Windows error");
  }
}


static int
win_error(const char *op)
{ atom_t msg = WinError();
  term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_system_error2,
		        PL_CHARS, op,
		        PL_ATOM, msg,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}
