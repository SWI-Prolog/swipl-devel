/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amsterdam. All rights reserved.
*/

#include <SWI-Prolog.h>
#include <string.h>
#include <malloc.h>

class PlTerm;
class PlTermv;

		 /*******************************
		 *	 PROLOG CONSTANTS	*
		 *******************************/

class PlFunctor
{
public:
  functor_t functor;

  PlFunctor(const char *name, int arity)
  { functor = PL_new_functor(PL_new_atom(name), arity);
  }
};


class PlAtom
{
public:
  atom_t handle;

  PlAtom(atom_t h)
  { handle = h;
  }
  PlAtom(const char *text)
  { handle = PL_new_atom(text);
  }
  PlAtom(const PlTerm &t);

  operator const char *(void)
  { return PL_atom_chars(handle);
  }

  int operator ==(const char *s)
  { return strcmp(s, PL_atom_chars(handle)) == 0;
  }
  int operator ==(const PlAtom &a)
  { return handle == a.handle;
  }
};

		 /*******************************
		 *     GENERIC PROLOG TERM	*
		 *******************************/


class PlTerm
{
public:
  term_t ref;

  PlTerm()
  { ref = PL_new_term_ref();
  }
  PlTerm(term_t t)
  { ref = t;
  }
  
					/* C --> PlTerm */
  PlTerm(const char *text)
  { ref = PL_new_term_ref();

    PL_put_atom_chars(ref, text);
  }
  PlTerm(long val)
  { ref = PL_new_term_ref();

    PL_put_integer(ref, val);
  }
  PlTerm(double val)
  { ref = PL_new_term_ref();

    PL_put_float(ref, val);
  }
  PlTerm(const PlAtom &a)
  { ref = PL_new_term_ref();

    PL_put_atom(ref, a.handle);
  }
  PlTerm(void *ptr)
  { ref = PL_new_term_ref();

    PL_put_pointer(ref, ptr);
  }

					/* PlTerm --> C */
  operator term_t(void)
  { return ref;
  }
  operator char *(void);
  operator long(void);
  operator int(void);
  operator double(void);
  operator PlAtom(void);
  operator void *(void);

  int type()
  { return PL_term_type(ref);
  }

					/* Compounds */
  PlTerm operator [](int index);
  int arity();
  const char *name();

					/* UNIFY */
  int operator =(const PlTerm &t2)	/* term */
  { return PL_unify(ref, t2.ref);
  }
  int operator =(const PlAtom &a)	/* atom */
  { return PL_unify_atom(ref, a.handle);
  }
  int operator =(const char *v)		/* atom (from char *) */
  { return PL_unify_atom_chars(ref, v);
  }
  int operator =(long v)		/* integer */
  { return PL_unify_integer(ref, v);
  }
  int operator =(int v)			/* integer */
  { return PL_unify_integer(ref, v);
  }
  int operator =(double v)		/* float */
  { return PL_unify_float(ref, v);
  }
  int operator =(const PlFunctor &f)	/* functor */
  { return PL_unify_functor(ref, f.functor);
  }

					/* Comparison standard order terms */
  int operator ==(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) == 0;
  }
  int operator !=(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) != 0;
  }
  int operator <(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) < 0;
  }
  int operator >(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) > 0;
  }
  int operator <=(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) <= 0;
  }
  int operator >=(const PlTerm &t2)
  { return PL_compare(ref, t2.ref) >= 0;
  }
					/* comparison (long) */
  int operator ==(long v);
  int operator !=(long v);
  int operator <(long v);
  int operator >(long v);
  int operator <=(long v);
  int operator >=(long v);

					/* comparison (string) */
  int operator ==(const char *s);
  int operator ==(const PlAtom &a);
};


		 /*******************************
		 *	   TERM VECTOR		*
		 *******************************/

class PlTermv
{
public:
  term_t a0;
  int    size;

  PlTermv(int n)
  { a0   = PL_new_term_refs(n);
    size = n;
  }
  PlTermv(int n, term_t t0)
  { a0   = t0;
    size = n;
  }

					/* create from args */
  PlTermv(PlTerm m0);
  PlTermv(PlTerm m0, PlTerm m1);
  PlTermv(PlTerm m0, PlTerm m1, PlTerm m2);
  PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3);
  PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3, PlTerm m4);

  PlTerm operator [](int n);
};

		 /*******************************
		 *	 SPECIALISED TERMS	*
		 *******************************/

class PlCompound : public PlTerm
{
public:
  
  PlCompound(const char *text);
  PlCompound(const char *functor, const PlTermv &args);
};


class PlString : public PlTerm
{
public:

  PlString(const char *text) : PlTerm()
  { PL_put_string_chars(ref, text);
  }
  PlString(const char *text, int len) : PlTerm()
  { PL_put_string_nchars(ref, len, text);
  }
};


class PlCodeList : public PlTerm
{ 
public:

  PlCodeList(const char *text) : PlTerm()
  { PL_put_list_codes(ref, text);
  }
};


class PlCharList : public PlTerm
{
public:

  PlCharList(const char *text) : PlTerm()
  { PL_put_list_chars(ref, text);
  }
};


		 /*******************************
		 *	      EXCEPTIONS	*
		 *******************************/

class PlException : public PlTerm
{
public:

  PlException(const PlTerm &t)
  { ref = t.ref;
  }

  operator char *(void);

  int plThrow()
  { return PL_raise_exception(ref);
  }

  void cppThrow();
};


class PlTypeError : public PlException
{
public:

  PlTypeError(const PlTerm &t) : PlException(t) {}

  PlTypeError(const char *expected, PlTerm actual) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("type_error",
					      PlTermv(expected, actual)),
				   PlTerm())))
  {
  }
};


class PlDomainError : public PlException
{
public:

  PlDomainError(const PlTerm &t) : PlException(t) {}

  PlDomainError(const char *expected, PlTerm actual) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("domain_error",
					      PlTermv(expected, actual)),
				   PlTerm())))
  {
  }
};


class PlTermvDomainError : public PlException
{
public:

  PlTermvDomainError(int size, int n) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("domain_error",
					      PlTermv(PlCompound("argv",
								 size),
						      n)),
				   PlTerm())))
  {
  }
};


		 /*******************************
		 *             LISTS		*
		 *******************************/

class PlTail : public PlTerm
{
public:
  
  PlTail(const PlTerm &l)
  { if ( PL_is_variable(l.ref) || PL_is_list(l.ref) )
      ref = PL_copy_term_ref(l.ref);
    else
      throw PlTypeError("list", l.ref);
  }

					/* building */
  int append(const PlTerm &e)
  { term_t tmp = PL_new_term_ref();

    if ( PL_unify_list(ref, tmp, ref) &&
	 PL_unify(tmp, e.ref) )
      return TRUE;

    return FALSE;
  }
  int close()
  { return PL_unify_nil(ref);
  }

					/* enumerating */
  int next(PlTerm &t)
  { if ( PL_get_list(ref, t, ref) )
      return TRUE;

    if ( PL_get_nil(ref) )
      return FALSE;

    throw PlTypeError("list", ref);
  }
};


		 /*******************************
		 *	     REGISTER		*
		 *******************************/


class PlRegister
{
public:
  
  PlRegister(const char *name, int arity,
	    foreign_t (f)(term_t t0, int a, control_t ctx))
  { PL_register_foreign(name, arity, (void *)f, PL_FA_VARARGS);
  }

  PlRegister(const char *name, foreign_t (*f)(PlTerm a0))
  { PL_register_foreign(name, 1, (void *)f, 0);
  }
  PlRegister(const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1))
  { PL_register_foreign(name, 2, (void *)f, 0);
  }
  PlRegister(const char *name, foreign_t (*f)(PlTerm a0, PlTerm a1, PlTerm a2))
  { PL_register_foreign(name, 3, (void *)f, 0);
  }
};


		 /*******************************
		 *	 CALLING PROLOG		*
		 *******************************/

class PlFrame
{
public:
  fid_t fid;

  PlFrame()
  { fid = PL_open_foreign_frame();
  }

  ~PlFrame()
  { PL_close_foreign_frame(fid);
  }

  void rewind()
  { PL_rewind_foreign_frame(fid);
  }
};


class PlQuery
{
public:
  qid_t qid;

  PlQuery(const char *name, const PlTermv &av)
  { predicate_t p = PL_predicate(name, av.size, "user");
    
    qid = PL_open_query((module_t)0, PL_Q_CATCH_EXCEPTION, p, av.a0);
  }
  PlQuery(const char *module, const char *name, const PlTermv &av)
  { predicate_t p = PL_predicate(name, av.size, module);
    
    qid = PL_open_query((module_t)0, PL_Q_CATCH_EXCEPTION, p, av.a0);
  }

  ~PlQuery()
  { PL_cut_query(qid);
  }

  int next_solution();
};


__inline int
PlCall(const char *predicate, const PlTermv &args)
{ PlQuery q(predicate, args);
  return q.next_solution();
}

__inline int
PlCall(const char *module, const char *predicate, const PlTermv &args)
{ PlQuery q(module, predicate, args);
  return q.next_solution();
}

__inline int
PlCall(const char *goal)
{ PlQuery q("call", PlTermv(PlCompound(goal)));
  return q.next_solution();
}



		 /*******************************
		 *	    ATOM (BODY)		*
		 *******************************/

__inline
PlAtom::PlAtom(const PlTerm &t)
{ atom_t a;

  if ( PL_get_atom(t.ref, &a) )
    handle = a;
  else
    throw PlTypeError("atom", t);
}


		 /*******************************
		 *	    TERM (BODY)		*
		 *******************************/

					/* PlTerm --> C */

__inline PlTerm::operator char *(void)
{ char *s;

  if ( PL_get_chars(ref, &s, CVT_ALL|CVT_WRITE|BUF_RING) )
    return s;

  throw PlTypeError("text", ref);
}

__inline PlTerm::operator long(void)
{ long v;
  
  if ( PL_get_long(ref, &v) )
    return v;

  throw PlTypeError("integer", ref);
}

__inline PlTerm::operator int(void)
{ int v;
  
  if ( PL_get_integer(ref, &v) )
    return v;

  throw PlTypeError("integer", ref);
}

__inline PlTerm::operator double(void)
{ double v;
  
  if ( PL_get_float(ref, &v) )
    return v;

  throw PlTypeError("float", ref);
}

__inline PlTerm::operator PlAtom(void)
{ atom_t v;
  
  if ( PL_get_atom(ref, &v) )
    return PlAtom(v);

  throw PlTypeError("atom", ref);
}

__inline PlTerm::operator void *(void)
{ void *ptr;

  if ( PL_get_pointer(ref, &ptr) )
    return ptr;

  throw PlTypeError("pointer", ref);
}

					/* compounds */

__inline PlTerm
PlTerm::operator [](int index)
{ PlTerm t;

  if ( PL_get_arg(index, ref, t.ref) )
    return t;

  if ( !PL_is_compound(ref) )
    throw PlTypeError("compound", ref);
  else
  { PL_put_integer(t.ref, index);

    if ( index < 1 )
      throw PlDomainError("not_less_than_zero", t.ref);
    else
      throw PlDomainError("arity", t.ref); /* TBD: proper exception */
  }
}


__inline int
PlTerm::arity()
{ atom_t name;
  int arity;
  
  if ( PL_get_name_arity(ref, &name, &arity) )
    return arity;

  throw PlTypeError("compound", ref);
}


__inline const char *
PlTerm::name()
{ atom_t name;
  int arity;
  
  if ( PL_get_name_arity(ref, &name, &arity) )
    return PL_atom_chars(name);

  throw PlTypeError("compound", ref);
}

					/* comparison */


__inline int PlTerm::operator ==(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 == v;

  throw PlTypeError("integer", ref);
}

__inline int PlTerm::operator !=(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 != v;

  throw PlTypeError("integer", ref);
}

__inline int PlTerm::operator <(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 < v;

  throw PlTypeError("integer", ref);
}

__inline int PlTerm::operator >(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 > v;

  throw PlTypeError("integer", ref);
}

__inline int PlTerm::operator <=(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 <= v;

  throw PlTypeError("integer", ref);
}

__inline int PlTerm::operator >=(long v)
{ long v0;

  if ( PL_get_long(ref, &v0) )
    return v0 >= v;

  throw PlTypeError("integer", ref);
}

				      /* comparison (string) */

__inline int PlTerm::operator ==(const char *s)
{ char *s0;

  if ( PL_get_chars(ref, &s0, CVT_ALL) )
    return strcmp(s0, s) == 0;
  
  throw PlTypeError("text", ref);
}


__inline int PlTerm::operator ==(const PlAtom &a)
{ atom_t v;

  if ( PL_get_atom(ref, &v) )
    return v == a.handle;
  
  throw PlTypeError("atom", ref);
}


		 /*******************************
		 *	COMPPOUND (BODY)	*
		 *******************************/


__inline
PlCompound::PlCompound(const char *text) : PlTerm()
{ term_t t = PL_new_term_ref();

  if ( !PL_chars_to_term(text, t) )
    throw PlException(t);

  PL_put_term(ref, t);
}

__inline
PlCompound::PlCompound(const char *functor, const PlTermv &args) : PlTerm()
{ PL_cons_functor_v(ref,
		    PL_new_functor(PL_new_atom(functor), args.size),
		    args.a0);
}

		 /*******************************
		 *	   TERMV (BODY)		*
		 *******************************/


__inline PlTermv::PlTermv(PlTerm m0)
{ size = 1;
  a0 = m0.ref;
}

__inline PlTermv::PlTermv(PlTerm m0, PlTerm m1)
{ size = 2;
  a0 = PL_new_term_refs(2);
  PL_put_term(a0+0, m0);
  PL_put_term(a0+1, m1);
}

__inline PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2)
{ size = 3;
  a0 = PL_new_term_refs(3);
  PL_put_term(a0+0, m0);
  PL_put_term(a0+1, m1);
  PL_put_term(a0+2, m2);
}

__inline PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2, PlTerm m3)
{ size = 4;
  a0 = PL_new_term_refs(4);
  PL_put_term(a0+0, m0);
  PL_put_term(a0+1, m1);
  PL_put_term(a0+2, m2);

  PL_put_term(a0+3, m3);
}

__inline PlTermv::PlTermv(PlTerm m0, PlTerm m1, PlTerm m2,
			  PlTerm m3, PlTerm m4)
{ size = 5;
  a0 = PL_new_term_refs(5);
  PL_put_term(a0+0, m0);
  PL_put_term(a0+1, m1);
  PL_put_term(a0+2, m2);
  PL_put_term(a0+3, m3);
  PL_put_term(a0+4, m4);
}


__inline PlTerm
PlTermv::operator [](int n)
{ if ( n < 0 || n >= size )
    throw PlTermvDomainError(size, n);

  return PlTerm(a0+n);
}


		 /*******************************
		 *	EXCEPTIONS (BODY)       *
		 *******************************/

__inline PlException::operator char *(void)
{ PlFrame fr;
#ifdef USE_PRINT_MESSAGE
  PlTermv av(2);

  av[0] = PlCompound("print_message",
		     PlTermv("error", ref));
  PlQuery q("$write_on_string", av);
  if ( q.next_solution() )
    return (char *)av[1];
#else
  PlTermv av(2);
  av[0] = PlTerm(ref);
  PlQuery q("$messages", "message_to_string", av);
  if ( q.next_solution() )
    return (char *)av[1];
#endif
  return "[ERROR: Failed to generate message.  Internal error]\n";
}


__inline void
PlException::cppThrow()
{ term_t a = PL_new_term_ref();
  atom_t name;
  int arity;

  if ( PL_get_arg(1, ref, a) &&
       PL_get_name_arity(a, &name, &arity) )
  { const char *s = PL_atom_chars(name);

    if ( strcmp(s, "type_error") == 0 )
      throw PlTypeError(ref);
    if ( strcmp(s, "domain_error") == 0 )
      throw PlDomainError(ref);
  }

  throw *this;
}


		 /*******************************
		 *	    QUERY (BODY)	*
		 *******************************/

__inline int
PlQuery::next_solution()
{ int rval;

  if ( !(rval = PL_next_solution(qid)) )
  { term_t ex;

    if ( (ex = PL_exception(qid)) )
      PlException(ex).cppThrow();
  }
  return rval;
}


		 /*******************************
		 *	      ENGINE		*
		 *******************************/

class PlError
{
public:
  char *message;

  PlError(const char *msg)
  { message = new char[strlen(msg+1)];
    strcpy(message, msg);
  }
};


class PlEngine
{
public:

  PlEngine(int argc, char **argv)
  { if ( !PL_initialise(argc, argv) )
      throw PlError("failed to initialise");
  }

  PlEngine(char *av0)
  { int ac = 0;
    char **av = (char **)malloc(sizeof(char *) * 2);

    av[ac++] = av0;

    if ( !PL_initialise(1, av) )
      throw PlError("failed to initialise");
  }
};


		 /*******************************
		 *     REGISTER PREDICATES	*
		 *******************************/

#ifdef PROLOG_MODULE
#define PlPredName(name) PROLOG_MODULE ":" #name
#else
#define PlPredName(name) #name
#endif

#define PREDICATE(name, arity) \
	static foreign_t \
	pl_ ## name ## __ ## arity(PlTermv _av); \
	static foreign_t \
	_pl_ ## name ## __ ## arity(term_t t0, int a, control_t c) \
	{ try \
	  { \
	    return pl_ ## name ## __ ## arity(PlTermv(arity, t0)); \
	  } catch ( PlException &ex ) \
	  { return ex.plThrow(); \
	  } \
	} \
	static PlRegister _x ## name ## __ ## arity(PlPredName(name), \
						    arity, \
					    _pl_ ## name ## __ ## arity); \
	static foreign_t pl_ ## name ## __ ## arity(PlTermv _av)
#define A1  _av[0]        
#define A2  _av[1]        
#define A3  _av[2]        
#define A4  _av[3]        
#define A5  _av[4]        
#define A6  _av[5]        
#define A7  _av[6]        
#define A8  _av[7]        
#define A9  _av[8]        
#define A10 _av[9]        
