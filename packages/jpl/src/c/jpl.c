/*	$Id$

    Part of JPL -- SWI-Prolog/Java interface

    Author:	   Paul Singleton, Fred Dushin and Jan Wielemaker
    E-mail:	   paul@jbgb.com
    WWW:	   http://www.swi-prolog.org
    Copyright (C): 1985-2004, Paul Singleton

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
this source file (jpl.c)  combines   my  Prolog-calls-Java stuff (mostly
prefixed 'JNI' or 'jni'  here)  with   my  adaptation  of  Fred Dushin's
Java-calls-Prolog stuff (mostly prefixed 'JPL' or 'jpl' here)

recent fixes:
 * using PL_get_pointer(), PL_put_pointer() consistently (?)
 * replaced all "Class:  jpl_fli_PL" by "Class:  jpl_fli_Prolog"

still to do:
 * make it completely thread-safe
   (both to multiple Prolog (engine-enabled) threads and to multiple Java threads)
 * suss JVM 'abort' and 'exit' handling, and 'vfprintf' redirection
 * rationalise initialisation; perhaps support startup from C?

refactoring (trivial):
 * initialise a functor_t for jpl_tidy_iref_type_cache/1
 * initialise a functor_t for -/2
 * initialise a module_t for jpl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* update this to distinguish releases of this C library: */
#define	JPL_C_LIB_VERSION	 "3.1.3-alpha"
#define	JPL_C_LIB_VERSION_MAJOR	 3
#define	JPL_C_LIB_VERSION_MINOR	 1
#define	JPL_C_LIB_VERSION_PATCH	 3
#define	JPL_C_LIB_VERSION_STATUS "alpha"

#define DEBUG(n, g) ((void)0)
/*#define DEBUG(n, g) g */

/* disable type-of-ref caching (at least until GC issues are resolved) */
#define		JPL_CACHE_TYPE_OF_REF		FALSE

/*=== includes ===================================================================================== */

#ifdef __WINDOWS__
/* OS-specific header (SWI-Prolog FLI and Java Invocation API both seem to need this): */
/* but not if we use the .NET 2.0 C compiler */
#include    <windows.h>
#define	    SIZEOF_WCHAR_T 2
#endif

/* SWI-Prolog headers: */
#include    <SWI-Prolog.h>
#include    <SWI-Stream.h>

/* Java Native Interface and Invocation Interface header: */
#include    <jni.h>

/* ANSI/ISO C library header (?): */
#include    <stdlib.h>
#include    <ctype.h>
#include    <errno.h>
#include    <string.h>

/* POSIX 'pthreads' headers (initially for JPL's Prolog engine pool, useful for locking generally?): */
#include    <pthread.h>
#include    <semaphore.h>


/*=== JNI constants ================================================================================ */

#define	    JNI_MIN_JCHAR		 0
#define	    JNI_MAX_JCHAR	     65535

#define	    JNI_MIN_JBYTE	      -128
#define	    JNI_MAX_JBYTE	       127

#define	    JNI_MIN_JSHORT	    -32768
#define	    JNI_MAX_JSHORT	     32767


#define	    JNI_XPUT_VOID		 0
#define	    JNI_XPUT_BOOLEAN		 1
#define	    JNI_XPUT_BYTE		 2
#define	    JNI_XPUT_CHAR		 3
#define	    JNI_XPUT_SHORT		 4
#define	    JNI_XPUT_INT		 5
#define	    JNI_XPUT_LONG		 6
#define	    JNI_XPUT_FLOAT		 7
#define	    JNI_XPUT_DOUBLE		 8
#define	    JNI_XPUT_FLOAT_TO_DOUBLE	 9
#define	    JNI_XPUT_LONG_TO_FLOAT	10
#define	    JNI_XPUT_LONG_TO_DOUBLE	11
#define	    JNI_XPUT_REF		12
#define	    JNI_XPUT_ATOM		13
#define	    JNI_XPUT_JVALUEP		14
#define	    JNI_XPUT_JVALUE		15


/* JNI "hashed refs" constants */

#define	    JNI_HR_LOAD_FACTOR		 0.75

/* jni_hr_add() return codes: */
#define	    JNI_HR_ADD_FAIL		-1
#define	    JNI_HR_ADD_NEW		 0
#define	    JNI_HR_ADD_OLD		 1


/*=== JPL constants ================================================================================ */

/* legit values for jpl_status_jpl_ini and jpl_status_pvm_ini */
#define	    JPL_INIT_RAW		101
#define	    JPL_INIT_PVM_MAYBE		102
#define	    JPL_INIT_OK			103
#define	    JPL_INIT_JPL_FAILED		104
#define	    JPL_INIT_PVM_FAILED		105

#define	    JPL_MAX_POOL_ENGINES	10 /* max pooled Prolog engines */
#define	    JPL_INITIAL_POOL_ENGINES	 1 /* initially created ones */


/*=== JNI Prolog<->Java conversion macros ========================================================== */

/* JNI (Prolog-calls-Java) conversion macros; mainly used in jni_{func|void}_{0|1|2|3|4}_plc; */
/* for re-entrancy, ensure that any variables which they use are declared dynamically */
/* (e.g. or i.e. are local to the host function); */
/* beware of evaluating *expressions* passed as actual parameters more than once; */

#define JNI_term_to_jboolean(T,JB) \
    ( PL_get_functor((T),&fn) \
      && fn==JNI_functor_at_1 \
    ? ( ( a1=PL_new_term_ref(), \
	  PL_get_arg(1,(T),a1) \
	) \
	&& PL_get_atom(a1,&a) \
      ? ( a==JNI_atom_false \
	? ( (JB)=0, TRUE) \
	: ( a==JNI_atom_true \
	  ? ( (JB)=1, TRUE) \
	  : FALSE \
	  ) \
	) \
      : FALSE \
      ) \
    : FALSE \
    )

#define JNI_term_to_jchar(T,J) \
	( PL_get_integer((T),&i) \
	  && i >= JNI_MIN_JCHAR \
	  && i <= JNI_MAX_JCHAR \
	  && ( (J)=(jchar)i, TRUE) \
    )

#define JNI_term_to_jbyte(T,J) \
	( PL_get_integer((T),&i) \
	  && i >= JNI_MIN_JBYTE \
	  && i <= JNI_MAX_JBYTE \
	  && ( (J)=(jbyte)i, TRUE) \
    )

#define JNI_term_to_jshort(T,J) \
	( PL_get_integer((T),&i) \
	  && i >= JNI_MIN_JSHORT \
	  && i <= JNI_MAX_JSHORT \
	  && ( (J)=(jshort)i, TRUE) \
    )

/* JW: jint is always 32-bit! */

#define JNI_term_to_jint(T,J) \
    ( PL_get_integer((T),&i) \
      && ((J)=i, TRUE) \
    )

#define JNI_term_to_non_neg_jint(T,J) \
    ( PL_get_long((T),&i) \
	  && i >= 0 \
	  && ( (J)=(jint)i, TRUE) \
    )

#define JNI_term_to_jlong(T,J) \
	( PL_get_int64((T),&i64) \
	  && ( (J)=(jlong)i64, TRUE) \
    )

#define JNI_term_to_jfloat(T,J) \
	( PL_get_float((T),&d) \
	? ( (J)=(jfloat)d, TRUE) \
	: ( PL_get_int64((T),&i64) \
	    && ( (J)=(jfloat)i64, TRUE) \
      ) \
    )

#define JNI_term_to_jdouble(T,J) \
    ( PL_get_float((T),&(J)) \
    ? TRUE \
	: ( PL_get_int64((T),&i64) \
	    && ( (J)=(jdouble)i64, TRUE) \
      ) \
    )

#define JNI_term_to_jfieldID(T,J) \
    ( PL_get_functor((T),&fn) \
      && fn==JNI_functor_jfieldID_1 \
      && ( a1=PL_new_term_ref(), \
	   PL_get_arg(1,(T),a1) \
	 ) \
      && PL_get_pointer(a1,(void**)&(J)) \
    )

#define JNI_term_to_jmethodID(T,J) \
    ( PL_get_functor((T),&fn) \
      && fn==JNI_functor_jmethodID_1 \
      && ( a1=PL_new_term_ref(), \
	   PL_get_arg(1,(T),a1) \
	 ) \
      && PL_get_pointer(a1,(void**)&(J)) \
    )

/* converts: */
/*   atom -> String */
/*	 @(Tag) -> obj */
/*   @(null) -> NULL */
/* (else fails) */
/* */
#define JNI_term_to_ref(T,J) \
	( PL_get_atom((T),&a) \
	? jni_atom_to_String(env,a,(jobject*)&(J)) \
    : PL_get_functor((T),&fn) \
      && fn==JNI_functor_at_1 \
      && ( a1=PL_new_term_ref(), \
	   PL_get_arg(1,(T),a1) \
	 ) \
      && PL_get_atom(a1,&a) \
      && ( a==JNI_atom_null \
	 ? ( (J)=0, TRUE) \
	 : jni_tag_to_iref(a,(long*)&(J)) \
	 ) \
    )

/* converts: */
/*   atom -> String */
/*	 @(Tag) -> obj */
/* (else fails) */
/* stricter than JNI_term_to_ref(T,J) */
/* */
#define JNI_term_to_jobject(T,J) \
	(  JNI_term_to_ref(T,J) \
	&& (J) != 0 \
    )

/* for now, these specific test-and-convert macros */
/* are merely mapped to their nearest ancestor... */

#define JNI_term_to_jclass(T,J)		    JNI_term_to_jobject(T,J)

#define JNI_term_to_throwable_jclass(T,J)   JNI_term_to_jobject(T,J)

#define JNI_term_to_non_array_jclass(T,J)   JNI_term_to_jobject(T,J)

#define JNI_term_to_throwable_jobject(T,J)  JNI_term_to_jobject(T,J)

#define JNI_term_to_jstring(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_jarray(T,J)		    JNI_term_to_jobject(T,J)

#define JNI_term_to_object_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_boolean_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_byte_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_char_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_short_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_int_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_long_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_float_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_double_jarray(T,J)	    JNI_term_to_jobject(T,J)

#define JNI_term_to_jbuf(T,J,TP) \
    ( PL_get_functor((T),&fn) \
      && fn==JNI_functor_jbuf_2 \
      && ( a2=PL_new_term_ref(), \
	   PL_get_arg(2,(T),a2) \
	 ) \
      && PL_get_atom(a2,&a) \
      && a==(TP) \
      && ( a1=PL_new_term_ref(), \
	   PL_get_arg(1,(T),a1) \
	 ) \
      && PL_get_pointer(a1,(void**)&(J)) \
    )

#define JNI_term_to_charP(T,J) \
    PL_get_atom_chars((T),&(J))

#define JNI_term_to_pointer(T,J) \
    PL_get_pointer((T),(void**)&(J))


/* JNI Java-to-Prolog conversion macros: */

#define JNI_unify_void(T) \
    PL_unify_term((T), \
      PL_FUNCTOR, JNI_functor_at_1, \
      PL_ATOM,	  JNI_atom_void \
    )

#define JNI_unify_false(T) \
    PL_unify_term((T), \
      PL_FUNCTOR, JNI_functor_at_1, \
      PL_ATOM,	  JNI_atom_false \
    )

#define JNI_unify_true(T) \
    PL_unify_term((T), \
      PL_FUNCTOR, JNI_functor_at_1, \
      PL_ATOM,	  JNI_atom_true \
    )

#define JNI_jboolean_to_term(J,T) \
    ( (J)==0 \
    ? JNI_unify_false((T)) \
    : JNI_unify_true((T)) \
    )

#define JNI_jchar_to_term(J,T) \
    PL_unify_integer((T),(int)(J))

#define JNI_jbyte_to_term(J,T) \
    PL_unify_integer((T),(int)(J))

#define JNI_jshort_to_term(J,T) \
    PL_unify_integer((T),(int)(J))

#define JNI_jint_to_term(J,T) \
    PL_unify_integer((T),(int)(J))

#define JNI_jlong_to_term(J,T) \
	PL_unify_int64((T),(int64_t)(J))

#define JNI_jfloat_to_term(J,T) \
    PL_unify_float((T),(double)(J))

#define JNI_jdouble_to_term(J,T) \
    PL_unify_float((T),(double)(J))

/* J can be an *expression* parameter to this macro; */
/* we must evaluate it exactly once; hence we save its value */
/* in the variable j, which must be dynamic (e.g. local) */
/* if this macro is to be re-entrant */
#define JNI_jobject_to_term(J,T) \
    ( ( j=(J), j==NULL ) \
    ? PL_unify_term((T), \
	PL_FUNCTOR, JNI_functor_at_1, \
	PL_ATOM, JNI_atom_null \
      ) \
    : ( (*env)->IsInstanceOf(env,j,str_class) \
	  ? jni_String_to_atom(env,j,&a) \
	&& PL_unify_term((T), \
	     PL_ATOM, a \
	   ) \
      : jni_object_to_iref(j,&i) \
	&& jni_iref_to_tag(i,&a) \
	&& PL_unify_term((T), \
	     PL_FUNCTOR, JNI_functor_at_1, \
	     PL_ATOM, a \
	   ) \
      ) \
    )

#define JNI_jfieldID_to_term(J,T) \
    PL_unify_term((T), \
      PL_FUNCTOR, JNI_functor_jfieldID_1, \
      PL_POINTER, (void*)(J) \
    )

#define JNI_jmethodID_to_term(J,T) \
    PL_unify_term((T), \
      PL_FUNCTOR, JNI_functor_jmethodID_1, \
      PL_POINTER, (void*)(J) \
    )

#define JNI_jbuf_to_term(J,T,TP) \
    PL_unify_term((T), \
      PL_FUNCTOR, JNI_functor_jbuf_2, \
      PL_POINTER, (void*)(J), \
      PL_ATOM,	  (TP) \
    )

#define JNI_pointer_to_term(J,T) \
    PL_unify_pointer((T),(void*)(J))

#define JNI_charP_to_term(J,T) \
    PL_unify_atom_chars((T),(J))


/*=== JNI initialisation macro (typically succeeds cheaply) ======================================== */

#define jni_ensure_jvm()	( ( jvm != NULL \
				  ||  jni_create_default_jvm() \
				  ) \
				  && (*jvm)->GetEnv(jvm,(void**)&env,JNI_VERSION_1_2)==JNI_OK \
				)


/*=== JPL initialisation macros (typically succeed cheaply) ======================================== */

/* outcomes: */
/*	fail to find jpl.*, jpl.fli.* classes or to convert init args to String[]: exception, FALSE */
/*	JPL is (newly or already) out of RAW state: TRUE */
#define 	jpl_ensure_jpl_init(e)	(	jpl_status != JPL_INIT_RAW \
									||	jpl_ensure_jpl_init_1(e) \
									)
/* outcomes: */
/*		JPL or PVM init has already failed: FALSE */
/*		JPL or PVM init fails while being necessarily attempted: exception */
/*		JPL is (newly or already) fully initialised: TRUE */
#define 	jpl_ensure_pvm_init(e)	(	jpl_status == JPL_INIT_OK \
									||	jpl_ensure_pvm_init_1(e) \
									)


/*=== types (structs and typedefs) ================================================================= */

typedef	    struct Hr_Entry HrEntry;	/* enables circular definition... */

struct Hr_Entry {		/* a single interned reference */
	jobject	    obj;	/* a JNI global ref */
	int	    hash;	/* identityHashCode(obj) */
	HrEntry	    *next;	/* next entry in this chain, or NULL */
	};

typedef	    struct Hr_Table HrTable;

struct Hr_Table {
	int	    count;	/* current # entries */
	int	    threshold;	/* rehash on add when count==threshold */
	int	    length;	/* # slots in slot array */
	HrEntry	    **slots;	/* pointer to slot array */
	};

typedef	    long    pointer;	/* for JPL (I reckon 'int' is enough on 32-bit systems) */
typedef	    int	    bool;	/* for JNI/JPL functions returning only TRUE or FALSE */


/*=== JNI constants: sizes of JNI primitive types ================================================== */

int	size[16] = {	/* NB relies on sequence of JNI_XPUT_* defs */
	    0,
	    sizeof(jboolean),	/* size[JNI_XPUT_BOOLEAN] */
	    sizeof(jbyte),	/* size[JNI_XPUT_BYTE] */
	    sizeof(jchar),	/* size[JNI_XPUT_CHAR] */
	    sizeof(jshort),	/* size[JNI_XPUT_SHORT] */
	    sizeof(jint),	/* size[JNI_XPUT_INT] */
	    sizeof(jlong),	/* size[JNI_XPUT_LONG] */
	    sizeof(jfloat),	/* size[JNI_XPUT_FLOAT] */
	    sizeof(jdouble),	/* size[JNI_XPUT_DOUBLE] */
	    0,			/* n/a - JNI_FLOAT_TO_DOUBLE */
	    0,			/* n/a - JNI_LONG_TO_FLOAT */
	    0,			/* n/a - JNI_LONG_TO_DOUBLE */
	    0,			/* n/a - JNI_REF */
	    0,			/* n/a - JNI_ATOM */
	    0,			/* n/a - JNI_JVALUEP */
	    sizeof(jvalue)	/* size[JNI_XPUT_JVALUE] */
	    };


/*=== JNI "constants", lazily initialised by jni_init() ============================================ */

static atom_t	    JNI_atom_false;		    /* false */
static atom_t	    JNI_atom_true;		    /* true */

static atom_t	    JNI_atom_boolean;		    /* boolean */
static atom_t	    JNI_atom_char;		    /* char */
static atom_t	    JNI_atom_byte;		    /* byte */
static atom_t	    JNI_atom_short;		    /* short */
static atom_t	    JNI_atom_int;		    /* int */
static atom_t	    JNI_atom_long;		    /* long */
static atom_t	    JNI_atom_float;		    /* float */
static atom_t	    JNI_atom_double;		    /* double */

static atom_t	    JNI_atom_null;		    /* null */
static atom_t	    JNI_atom_void;		    /* void */

static functor_t   JNI_functor_at_1;		    /* @(_) */
static functor_t   JNI_functor_jbuf_2;		    /* jbuf(_,_) */
static functor_t   JNI_functor_jlong_2;	    /* jlong(_,_) */
static functor_t   JNI_functor_jfieldID_1;	    /* jfieldID(_) */
static functor_t   JNI_functor_jmethodID_1;	    /* jmethodID(_) */
static functor_t   JNI_functor_error_2;		    /* error(_, _) */
static functor_t   JNI_functor_java_exception_1;    /* java_exception(_) */
static functor_t   JNI_functor_jpl_error_1;	    /* jpl_error(_) */


/*=== JNI's static JVM references, lazily initialised by jni_init() ================================ */

static jclass	   c_class;	    /* java.lang.Class                       (rename to jClass_c ?) */
static jmethodID   c_getName;	    /* java.lang.Class' getName()            (rename to jClassGetName_m ?) */
static jclass	   str_class;	    /* java.lang.String                      (this duplicates jString_c below) */
static jclass	   term_class;		/* jpl.Term */
static jclass	   termt_class; 	/* jpl.fli.term_t */

static jclass	   sys_class;	    /* java.lang.System                      (rename to jSystem_c ?) */
static jmethodID   sys_ihc;	    /* java.lang.System's identityHashCode() (rename to jSystemIdentityHashCode_m ?) */
static jmethodID   term_getTerm;	/* jpl.Term's getTerm() */
static jmethodID   term_put;		/* jpl.Term's put() */
static jmethodID   term_putTerm;	/* jpl.Term's static putTerm(Term,term_t) */


/*=== JPL's reusable global class object refs, initialised by jpl_ensure_jpl_init() ================ */

static jclass	    jString_c;
static jclass	    jJPLException_c;
static jclass	    jTermT_c;
static jclass	    jAtomT_c;
static jclass	    jFunctorT_c;
static jclass	    jFidT_c;
static jclass	    jPredicateT_c;
static jclass	    jQidT_c;
static jclass	    jModuleT_c;
static jclass	    jEngineT_c;

static jclass	    jLongHolder_c;	    
static jclass	    jPointerHolder_c;
static jclass	    jIntHolder_c;
static jclass		jInt64Holder_c;
static jclass	    jDoubleHolder_c;
static jclass	    jStringHolder_c;
static jclass	    jObjectHolder_c;
static jclass	    jBooleanHolder_c;


/*=== JPL's reusable constant field IDs, set before first use by jpl_ensure_jpl_init() ============= */

static jfieldID    jLongHolderValue_f;
static jfieldID    jPointerHolderValue_f;
static jfieldID    jIntHolderValue_f;
static jfieldID    jInt64HolderValue_f;
static jfieldID    jDoubleHolderValue_f;
static jfieldID    jStringHolderValue_f;
static jfieldID    jObjectHolderValue_f;
static jfieldID    jBooleanHolderValue_f;


/*=== JPL's default args for PL_initialise() (NB these are not really good enough) ================= */

const char  *default_args[] = { "pl",
				"-g", "true",
				"-nosignals",
				NULL
			    };	/* *must* have final NULL */


/*=== JNI global state (initialised by jni_create_jvm_c) =========================================== */

static JavaVM	*jvm = NULL;	/* non-null -> JVM successfully loaded & initialised */
static JNIEnv	*env;		/* if jvm is defined, then so will this be */
static char		*jvm_ia[2] = {"-Xrs", NULL};
static char		**jvm_dia = jvm_ia;		/* default JVM init args (after jpl init, until jvm init) */
static char		**jvm_aia = NULL;		/* actual JVM init args (after jvm init) */


/*=== JNI global state (hashed global refs) ======================================================== */

static HrTable	*hr_table =	NULL;	/* static handle to allocated-on-demand table */
static int	hr_add_count =	0;  /* cumulative total of new refs interned */
static int	hr_old_count =	0;  /* cumulative total of old refs reused */
static int	hr_del_count =	0;  /* cumulative total of dead refs released */


/*=== JPL global state, initialised by jpl_ensure_jpl_init() or jpl_ensure_jvm_init() ============== */

static int		jpl_status =	JPL_INIT_RAW;  /* neither JPL nor PVM initialisation has occurred */
static jobject			pvm_dia =		NULL;  /* default PVM init args (after jpl init, until pvm init) */
static jobject			pvm_aia =		NULL;  /* actual PVM init args (after pvm init) */
static PL_engine_t	*engines =	NULL;  /* handles of the pooled Prolog engines */
static int		engines_allocated = 0; /* size of engines array */
static pthread_mutex_t	engines_mutex = PTHREAD_MUTEX_INITIALIZER;  /* for controlling pool access */
static pthread_cond_t	engines_cond =	PTHREAD_COND_INITIALIZER;  /* for controlling pool access */

static pthread_mutex_t	jvm_init_mutex = PTHREAD_MUTEX_INITIALIZER;	/* for controlling lazy initialisation */
static pthread_mutex_t	pvm_init_mutex = PTHREAD_MUTEX_INITIALIZER;	/* for controlling lazy initialisation */


/*=== common functions ============================================================================= */

static char *
jpl_c_lib_version()
    {
    static char v[100]; /* version string */
    static char *vp = NULL; /* set to v at first call */

    if ( vp != NULL ) /* already set? */
	{
	return vp;
	}
    sprintf( v, "%d.%d.%d-%s", JPL_C_LIB_VERSION_MAJOR, JPL_C_LIB_VERSION_MINOR, JPL_C_LIB_VERSION_PATCH, JPL_C_LIB_VERSION_STATUS);
    vp = v;
    return vp;
    }


static foreign_t
jpl_c_lib_version_1_plc(
	term_t		ta	/* -atom: this library's version as an atom, e.g. '3.1.0-alpha' */
    )
    {

    return  PL_unify_atom_chars(ta,jpl_c_lib_version());
    }


static foreign_t
jpl_c_lib_version_4_plc(
	term_t		tmajor,	/* -integer: major version number */
	term_t		tminor,	/* -integer: minor version number */
	term_t		tpatch,	/* -integer: patch version number */
	term_t		tstatus	/* -atom: status of this version */
    )
    {

    return  PL_unify_integer(tmajor,JPL_C_LIB_VERSION_MAJOR)
	&&  PL_unify_integer(tminor,JPL_C_LIB_VERSION_MINOR)
	&&  PL_unify_integer(tpatch,JPL_C_LIB_VERSION_PATCH)
	&&  PL_unify_atom_chars(tstatus,JPL_C_LIB_VERSION_STATUS);
    }


/*=== JNI function prototypes (to resolve unavoidable forward references) ========================== */

static int	    jni_hr_add(jobject, long*);
static int	    jni_hr_del(long);


/*=== JNI functions (NB first 6 are cited in macros used subsequently) ============================= */

/* this now checks that the atom's name resembles a tag (PS 18/Jun/2004) */
static bool
jni_tag_to_iref(
    atom_t	a,
    long	*iref
    )
    {
    const char  *s = PL_atom_chars(a);
    long r;

	if ( strlen(s) == 22
	 && s[0] == 'J'
	 && s[1] == '#'
	 && isdigit(s[2])
	 && isdigit(s[3])
	 && isdigit(s[4])
	 && isdigit(s[5])
	 && isdigit(s[6])
	 && isdigit(s[7])
	 && isdigit(s[8])
	 && isdigit(s[9])
	 && isdigit(s[10])
		&& isdigit(s[11])
		&& isdigit(s[12])
		&& isdigit(s[13])
		&& isdigit(s[14])
		&& isdigit(s[15])
		&& isdigit(s[16])
		&& isdigit(s[17])
		&& isdigit(s[18])
		&& isdigit(s[19])
		&& isdigit(s[20])
		&& isdigit(s[21])			 /* s is like 'J#01234567890123456789' */
	 && (r=atol(&s[2])) != 0 )
		{
		*iref = r;
      return 1;
    }
	else
		{
    return 0;
    }
	}


static bool
jni_iref_to_tag(
    long	iref,
    atom_t	*a
    )
    {
	char		abuf[23];

	sprintf( abuf, "J#%020lu", iref);	/* oughta encapsulate this mapping... */
    *a = PL_new_atom(abuf);
    PL_unregister_atom(*a);		/* empirically decrement reference count... */
    return TRUE;			/* can't fail (?!) */
    }


static bool
jni_object_to_iref(
    jobject	obj,	    /* a newly returned JNI local ref */
    long	*iref	    /* gets an integerised, canonical, global equivalent */
    )
    {
    int		r;	    /* temp for result code */
    
    if ( (r=jni_hr_add(obj,iref)) == JNI_HR_ADD_NEW )
	{
	hr_add_count++;	    /* obj was novel, has been added to dict */
	return TRUE;
	}
    else
    if ( r == JNI_HR_ADD_OLD )
	{
	hr_old_count++;	    /* obj was already in dict */
	return TRUE;
	}
    else
	{
	return FALSE;	    /* r == JNI_HR_ADD_FAIL, presumably */
	}
    }


/* retract all jpl_iref_type_cache(Iref,_) facts */
static bool
jni_tidy_iref_type_cache(
	long 		iref
    )
    {
	term_t		goal;

	if ( JPL_CACHE_TYPE_OF_REF )
		{
		goal = PL_new_term_ref();
    PL_unify_term( goal,
	PL_FUNCTOR, PL_new_functor(PL_new_atom("jpl_tidy_iref_type_cache"),1),
	PL_INT, iref
	);
    return  PL_call(
		goal,
					PL_new_module(PL_new_atom("jpl"))	/* "jpl" was erroneously "user" (bugfix 29/3/2005) */
	    );
    }
	else
		{
		return TRUE;
		}
	}


/* could merge this into jni_hr_del() ? */
static bool
jni_free_iref(		    /* called indirectly from agc hook when a possible iref is unreachable */
    long	iref
    )
    {

    if ( jni_hr_del(iref) )	/* iref matched a hashedref table entry? (in which case, was deleted) */
	{
	if ( !jni_tidy_iref_type_cache(iref) )
	    {
	      DEBUG(0, Sdprintf( "[JPL: jni_tidy_iref_type_cache(%u) failed]\n", iref));
	    } 
	hr_del_count++;
	return TRUE;
	}
    else
	{
	return FALSE;
	}
    }


/* NB this delivers an atom_t, not a term_t */
/* returns FALSE if the String arg is NULL */
static bool
 jni_String_to_atom( 	/* called from JNI_jobject_to_term(J,T) and jpl.fli.Prolog#new_atom() */
	JNIEnv		*env,
	jobject 	s,
    atom_t	*a
    )
    {
	jsize			len = (*env)->GetStringLength(env,s);
	const jchar		*jcp = (*env)->GetStringChars(env,s,NULL);

	if ( s == NULL )
		{
		return FALSE;
		}
#if SIZEOF_WCHAR_T == 2
		{
		*a = PL_new_atom_wchars(len,jcp); /* easy, huh? (thanks, Jan) */
		}
#else
		{
		pl_wchar_t		*wp;
		jsize			i;

		if ( (wp=(pl_wchar_t*)malloc(sizeof(pl_wchar_t)*len)) == NULL) {
		        (*env)->ReleaseStringChars(env,s,jcp);
			return FALSE;
		}
		for ( i=0 ; i<len ; i++ )
			{
			wp[i] = (pl_wchar_t)jcp[i];
			}
		*a = PL_new_atom_wchars(len,wp); /* this works now */
		free( wp);
		}
#endif
	(*env)->ReleaseStringChars(env,s,jcp);
	return TRUE;
    }


/* NB this takes an atom_t, not a term_t */
static bool
 jni_atom_to_String(
	JNIEnv		*env,
	atom_t		a,
	jobject 	*s
	)
	{
	size_t		len;
	pl_wchar_t	*wp;
	jchar		*jcp;
	unsigned char   *cp;
	unsigned int	i;

	if ( (cp=(unsigned char*)PL_atom_nchars(a,&len)) != NULL ) /* got 8-bit chars from trad atom */
		{
		jcp = (jchar*)malloc(sizeof(jchar)*len);
		for ( i=0 ; i<len ; i++ )
			{
			jcp[i] = (jchar)cp[i]; /* widen */
			}
		*s = (*env)->NewString(env,jcp,(jsize)len);
		free(jcp);
		return TRUE;
		}
	else if ( (wp=(pl_wchar_t*)PL_atom_wchars(a,&len)) != NULL ) /* got (wide) chars from wide atom */
		{
#if SIZEOF_WCHAR_T == 2
			{
			*s = (*env)->NewString(env,wp,(jsize)len);
			}
#else
			{
			jcp = (jchar*)malloc(sizeof(jchar)*len);
			for ( i=0 ; i<len ; i++ )
				{
				jcp[i] = (jchar)wp[i]; /* narrow */
				}
			*s = (*env)->NewString(env,jcp,len);
			free(jcp);
			}
#endif
		return TRUE;
		}
	else
		{
		return FALSE;
		}
	}


/* checks that the term_t is a string and delivers a String representation of it */
static bool
 jni_string_to_String(
	JNIEnv		*env,
	term_t		t,	/* a term which may or may not be a SWIPL string */
	jobject 	*s
	)
	{
	size_t		len;
	pl_wchar_t	*wp;
	jchar		*jcp;
	char		*cp;
	unsigned int	i;

	if ( PL_get_nchars(t,&len,&cp,CVT_ATOM) ) /* got 8-bit chars from string? */
		{
		jcp = (jchar*)malloc(sizeof(jchar)*len);
		for ( i=0 ; i<len ; i++ )
			{
			jcp[i] = (jchar)cp[i]; /* widen */
			}
		*s = (*env)->NewString(env,jcp,(jsize)len);
		free(jcp);
		return TRUE;
		}
	else if ( PL_get_wchars(t,&len,&wp,CVT_STRING) ) /* got (wide) chars from string? */
		{
#if SIZEOF_WCHAR_T == 2
			{
			*s = (*env)->NewString(env,wp,(jsize)len);
			}
#else
			{
			jcp = (jchar*)malloc(sizeof(jchar)*len);
			for ( i=0 ; i<len ; i++ )
				{
				jcp[i] = (jchar)wp[i]; /* narrow */
				}
			*s = (*env)->NewString(env,jcp,len);
			free(jcp);
			}
#endif
		return TRUE;
		}
	else
		{
		return FALSE;
		}
	}


/* an FLI wrapper for jni_tag_to_iref() above */
/* is currently called by jpl_tag_to_type/2, jpl_cache_type_of_object/2 */
/* jpl_tag_to_type/2 is called by jpl_object_to_type/2, jpl_ref_to_type/2 */
static foreign_t
jni_tag_to_iref_plc(
	term_t		tt,	/* +atom: a tag */
	term_t		ti	/* -integer: its corresponding iref */
    )
    {
    atom_t	a;
    long	iref;

    return  PL_get_atom(tt,&a)
	&&  jni_tag_to_iref(a,&iref)
	&&  PL_unify_integer(ti,iref);
    }


/* this will be hooked to SWI-Prolog's PL_agc_hook, */
/* and is called just before each redundant atom is expunged from the dict */
/* NB need to be able to switch this on and off from Prolog... */
static bool
jni_atom_freed(
    atom_t	a
    )
    {
    const char	*cp = PL_atom_chars(a);
    long	iref;
    char	cs[23]; /* was 11 until 24/Apr/2007 */

    if ( jni_tag_to_iref( a, &iref) )	/* check format and convert digits to int if ok */
        {
        sprintf( cs, "%020lu", iref);	/* reconstruct digits part of tag in cs */
        if ( strcmp(&cp[2],cs) != 0 )	/* original digits != reconstructed digits? */
            {
	      DEBUG(0, Sdprintf( "[JPL: garbage-collected tag '%s'=%u is bogus (not canonical)]\n", cp, iref));
	    }
	else
	if ( !jni_free_iref(iref) )		/* free it (iff it's in the hashedref table) */
	    {
	      DEBUG(0, Sdprintf( "[JPL: garbage-collected tag '%s' is bogus (not in HashedRefs)]\n", cp));
	    }
	}
    else
	{
	}
    return TRUE;    /* means "go ahead and expunge the atom" (we do this regardless) */
    }


/*=== "hashed ref" (canonical JNI global reference) support ======================================== */

static foreign_t
jni_hr_info_plc(    /* implements jni_hr_info/4 */
    term_t	t1, /* -integer:       # object references currently in hash table */
    term_t	t2, /* -integer: total # object references so far added */
    term_t	t3, /* -integer: total # object references so far found to be already in table */
    term_t	t4  /* -integer: total # object references deleted from table (by atom GC) */
    )
    {
    return  PL_unify_integer(t1,(hr_table==NULL?0:hr_table->count))	/* 0 was -1 (??) */
	&&  PL_unify_integer(t2,hr_add_count)
	&&  PL_unify_integer(t3,hr_old_count)
	&&  PL_unify_integer(t4,hr_del_count);
    }


/* unifies t2 with a Prolog term which represents the contents of the hashtable slot */
static bool
jni_hr_table_slot(
    term_t	t2,
    HrEntry	*slot
    )
    {
    term_t	tp = PL_new_term_ref();

    if ( slot == NULL )
	{
	return PL_unify_nil(t2);
	}
    else
	{
	return	PL_unify_list(t2,tp,t2)
	    &&	PL_unify_term(tp,
		    PL_FUNCTOR, PL_new_functor(PL_new_atom("-"),2),
		    PL_INT, slot->hash,
		    PL_LONG, slot->obj
		)
	    &&	jni_hr_table_slot(t2,slot->next)
	    ;
	}
    }


/* unifies t with a list of hash table slot representations */
static foreign_t
jni_hr_table_plc(
    term_t	t
    )
    {
    term_t	t1 = PL_copy_term_ref(t);
    term_t	t2 = PL_new_term_ref();
    int		i;

    for ( i=0 ; i<hr_table->length ; i++ )
	{
	if ( !PL_unify_list(t1,t2,t1) || !jni_hr_table_slot(t2,hr_table->slots[i]) )
	    {
	    return FALSE;
	    }
	}
    return PL_unify_nil(t1);
    }

    
/* an empty table of length is successfully created, where none was before */
static bool
jni_hr_create(
    int		length	    /* required # slots in table */
    )
    {
    int		i;	    /* temp for iterative slot initialisation */

    if ( hr_table != NULL )
	{
	return FALSE;	    /* table already exists (destroy before recreating) */
	}
    if ( length <= 0 )
	{
	return FALSE;	    /* unsuitable length */
	}
    if ( (hr_table=(HrTable*)malloc(sizeof(HrTable))) == NULL )
	{
	return FALSE;	    /* malloc failed (out of memory, presumably) */
	}
    hr_table->length = length;
    hr_table->threshold = (int)(hr_table->length*JNI_HR_LOAD_FACTOR);
    if ( (hr_table->slots=(HrEntry**)malloc(length*sizeof(HrEntry*))) == NULL )
	{
	return FALSE;	    /* malloc failed: out of memory, presumably */
	}
    for ( i=0 ; i<hr_table->length ; i++ )
	{
	hr_table->slots[i] = NULL;
	}
    hr_table->count = 0;
    return TRUE;
    }


/* an empty table of some default length is successfully created, where none was before */
static bool
jni_hr_create_default()
    {

    return jni_hr_create( 101);
    }


/* ep must point to a chain of zero or more entries; they are freed */
static void
jni_hr_free_chain_entries(
    HrEntry	*ep
    )
    {

    if ( ep != NULL )
	{
	jni_hr_free_chain_entries( ep->next);
	free( ep);
	}
    }


/* table t is emptied */
static void
jni_hr_free_table_chains(
    HrTable	*t
    )
    {
    int	    index;

    for ( index=0 ; index<(t->length) ; index++ )
	{
	jni_hr_free_chain_entries( t->slots[index]);
	t->slots[index] = NULL;
	}
    t->count = 0;
    }


/* all dynamic space used by the pointed-to table is freed */
static bool
jni_hr_free_table(
    HrTable	*t
    )
    {

    if ( t == NULL )
	{
	return FALSE;	/* table does not exist */
	}
    else
	{
	jni_hr_free_table_chains( t);
	free( t);
	return TRUE;
	}
    }


/* the current table is replaced by an equivalent one with more free space */
static bool
jni_hr_rehash()
    {
    HrTable	*t0;	/* old table while building new one from it */
    int		i;	    /* for iterating through slots in old table */
    HrEntry	*ep1;	/* for iterating through all entries in old table */
    HrEntry	*ep2;	/* an old table entry being relinked into new table */
    int		index;	/* slot index in new table of entry being transferred */

    t0 = hr_table;	/* temporarily hold onto former table */
    hr_table = NULL;	/* precondition for jni_hr_create */
    if ( !jni_hr_create(2*t0->length+1) )   /*	new bigger table in its place */
	{
	hr_table = t0;	/* replace former table for tidiness */
	return FALSE;	/* failed to create replacement table during rehash */
	}
    for ( i=0 ; i<t0->length ; i++ )	/* for each slot in *former* table */
	{
	for ( ep1=t0->slots[i] ; ep1!=NULL ; )
	    {			/* for each entry in that slot's chain */
	    ep2 = ep1;		/* grab this entry */
	    ep1 = ep1->next;	/* advance to next entry or NULL */
	    index = (ep2->hash & 0x7fffffff) % hr_table->length;    /* new */
	    ep2->next = hr_table->slots[index]; /* relink into new array */
	    hr_table->slots[index] = ep2;	/*  " */
	    }
	t0->slots[i] = NULL;	/* tidy old array for generic freeing later */
	}
    hr_table->count = t0->count;    /* new table's count is old table's count */
    jni_hr_free_table( t0); /* free all space used by old table (NB no entries) */
    return TRUE;
    }


static bool
 jni_hr_hash(			/* renamed in v3.0.4 from jni_object_to_hash (it belongs with this hr stuff) */
    jobject	obj,	/* MUST BE a valid non-null reference to a Java object */
    int		*hash	/* gets obj's System.identityHashCode() */
    )
    {
    jobject	e;	/* for possible (but unlikely?) exception */

    *hash = (*env)->CallStaticIntMethod(env,sys_class,sys_ihc,obj,obj);
    return (e=(*env)->ExceptionOccurred(env))==NULL;
    }


/* returns */
/*   JNI_HR_ADD_NEW -> referenced object is novel */
/*   JNI_HR_ADD_OLD -> referenced object is already known */
/*   JNI_HR_ADD_FAIL -> something went wrong */
/* and, in *iref, an integerised canonical global ref to the object */
static int
jni_hr_add(
    jobject	lref,	/* new JNI local ref from a regular JNI call  */
    long	*iref	/* for integerised canonical global ref */
    )
    {
    int		hash;	/* System.identityHashCode of lref */
    int		index;	/* lref's slot index, from hash */
    HrEntry	*ep;	/* temp entry pointer for chain traversal */
    jobject	gref;	/* iff lref is novel, will hold a global surrogate */

    if ( hr_table==NULL && !jni_hr_create_default() )
	{
	return JNI_HR_ADD_FAIL; /* lazy table creation failed: oughta sort return codes */
	}
	if ( !jni_hr_hash(lref,&hash) ) 	/* renamed in v3.0.4 from jni_object_to_hash */
	{
	return JNI_HR_ADD_FAIL; /* System.identityHashCode() failed (?) */
	}
    index = (hash & 0x7fffffff) % hr_table->length;	/* make this a macro? */
    for ( ep=hr_table->slots[index] ; ep!=NULL ; ep=ep->next )
	{
	if ( ep->hash==hash )
	    {
	    if ( (*env)->IsSameObject(env,ep->obj,lref) )
		{ /* newly referenced object is already interned */
		(*env)->DeleteLocalRef(env,lref);   /* free redundant new ref */
		*iref = (long)ep->obj; /* old, equivalent (global) ref */
		return JNI_HR_ADD_OLD;
		}
	    }
	}
    if ( hr_table->count >= hr_table->threshold )
	{
	(void)jni_hr_rehash();	/* oughta check for failure, and return it... */
	return jni_hr_add(lref,iref);	/* try again with new, larger table */
	}
    /* referenced object is novel, and we can add it to table */
    if ( (gref=(*env)->NewGlobalRef(env,lref)) == NULL )    /* derive a global ref */
	{
	return JNI_HR_ADD_FAIL;
	}
    (*env)->DeleteLocalRef(env,lref);	/* free redundant (local) ref */
    ep = (HrEntry*)malloc(sizeof(HrEntry));
    ep->hash = hash;
    ep->obj = gref;
    ep->next = hr_table->slots[index];	/* insert at front of chain */
    hr_table->slots[index] = ep;
    hr_table->count++;
    *iref = (long)gref;	/* pass back the (new) global ref */
    return JNI_HR_ADD_NEW;  /* obj was newly interned, under iref as supplied */
    }


/* iref corresponded to an entry in the current HashedRef table; */
/* now that entry is gone, its space is recovered, counts are adjusted etc. */
/* called only from jni_free_iref() */
static bool
jni_hr_del(
    long	iref	/* a possibly spurious canonical global iref */
    )
    {
    int		index;	/* index to a HashedRef table slot */
    HrEntry	*ep;	/* pointer to a HashedRef table entry */
    HrEntry	**epp;	/* pointer to ep's handle, in case it needs updating */

    DEBUG(1, Sdprintf( "[removing possible object reference %u]\n", iref));
    for ( index=0 ; index<hr_table->length ; index++ )		/* for each slot */
	{
	for ( epp=&(hr_table->slots[index]), ep=*epp ; ep!=NULL ; epp=&(ep->next), ep=*epp )
	    {
	    if ( (long)(ep->obj) == iref )			/* found the sought entry? */
		{
		(*env)->DeleteGlobalRef( env, ep->obj);		/* free the global object reference */
		*epp = ep->next;				/* bypass the entry */
		free( ep);					/* free the now-redundant space */
		hr_table->count--;				/* adjust table's entry count */
		DEBUG(1, Sdprintf( "[found & removed hashtable entry for object reference %u]\n", iref));
		return TRUE;					/* entry found and removed */
		}
	    }
	}
    DEBUG(1, Sdprintf("[JPL: failed to find hashtable entry for (presumably bogus) object reference %u]\n", iref));
    return FALSE;
    }


/*=== JNI initialisation =========================================================================== */

/* called once: after successful PVM & JVM creation/discovery, before any JNI calls */
static int
jni_init()
    {
    jclass	lref;	    /* temporary local ref, replaced by global */

    /* these initialisations require an active PVM: */
    JNI_atom_false	= PL_new_atom( "false");
    JNI_atom_true	= PL_new_atom( "true");

    JNI_atom_boolean	= PL_new_atom( "boolean");
    JNI_atom_char	= PL_new_atom( "char");
    JNI_atom_byte	= PL_new_atom( "byte");
    JNI_atom_short	= PL_new_atom( "short");
    JNI_atom_int	= PL_new_atom( "int");
    JNI_atom_long	= PL_new_atom( "long");
    JNI_atom_float	= PL_new_atom( "float");
    JNI_atom_double	= PL_new_atom( "double");

    JNI_atom_null	= PL_new_atom( "null");
    JNI_atom_void	= PL_new_atom( "void");	    /* not yet used properly (?) */

    JNI_functor_at_1	= PL_new_functor( PL_new_atom("@"), 1);
    JNI_functor_jbuf_2	= PL_new_functor( PL_new_atom("jbuf"), 2);
    JNI_functor_jlong_2 = PL_new_functor( PL_new_atom("jlong"), 2);
    JNI_functor_jfieldID_1  = PL_new_functor( PL_new_atom("jfieldID"), 1);
    JNI_functor_jmethodID_1 = PL_new_functor( PL_new_atom("jmethodID"), 1);

    JNI_functor_error_2 = PL_new_functor(PL_new_atom("error"), 2);
    JNI_functor_java_exception_1 = PL_new_functor( PL_new_atom("java_exception"), 1);
    JNI_functor_jpl_error_1 = PL_new_functor( PL_new_atom("jpl_error"), 1);

    (void)PL_agc_hook( jni_atom_freed); /* link atom GC to object GC (cool:-) */

    /* these initialisations require an active JVM: */
    return  (  (lref=(*env)->FindClass(env,"java/lang/Class")) != NULL
	    && (c_class=(*env)->NewGlobalRef(env,lref)) != NULL
	    && ( (*env)->DeleteLocalRef(env,lref), TRUE)

	    && (lref=(*env)->FindClass(env,"java/lang/String")) != NULL
	    && (str_class=(*env)->NewGlobalRef(env,lref)) != NULL
	    && ( (*env)->DeleteLocalRef(env,lref), TRUE)
	    && (c_getName=(*env)->GetMethodID(env,c_class,"getName","()Ljava/lang/String;")) != NULL

	    && (lref=(*env)->FindClass(env,"java/lang/System")) != NULL
	    && (sys_class=(*env)->NewGlobalRef(env,lref)) != NULL
	    && ( (*env)->DeleteLocalRef(env,lref), TRUE)
	    && (sys_ihc=(*env)->GetStaticMethodID(env,sys_class,"identityHashCode","(Ljava/lang/Object;)I")) != NULL

			&& (lref=(*env)->FindClass(env,"jpl/Term")) != NULL
			&& (term_class=(*env)->NewGlobalRef(env,lref)) != NULL
			&& ( (*env)->DeleteLocalRef(env,lref), TRUE)
			&& (term_getTerm=(*env)->GetStaticMethodID(env,term_class,"getTerm","(Ljpl/fli/term_t;)Ljpl/Term;")) != NULL
			&& (term_put=(*env)->GetMethodID(env,term_class,"put","(Ljpl/fli/term_t;)V")) != NULL
			&& (term_putTerm=(*env)->GetStaticMethodID(env,term_class,"putTerm","(Ljava/lang/Object;Ljpl/fli/term_t;)V")) != NULL

			&& (lref=(*env)->FindClass(env,"jpl/fli/term_t")) != NULL
			&& (termt_class=(*env)->NewGlobalRef(env,lref)) != NULL
			&& ( (*env)->DeleteLocalRef(env,lref), TRUE)

	    ?	0
	    :	-7	    /* NB #define this? */
	    )
	;
    }


/*=== JNI exception/error processing/support ======================================================= */

/* returns a new error(java_exception(@(tag)),msg) to represent a caught Java exception */
static term_t
 jni_new_java_exception(
	atom_t		tag,
	atom_t		msg
    )
    {
    term_t	e = PL_new_term_ref();

    PL_unify_term(e,
		  PL_FUNCTOR, JNI_functor_error_2,
		    PL_FUNCTOR, JNI_functor_java_exception_1,
		      PL_FUNCTOR, JNI_functor_at_1,
						PL_ATOM, tag,
					PL_ATOM, msg);
    return e;
    }


/* returns a new error(jpl_error(@(tag)),msg) to represent an exceptional condition raised within JPL */
static term_t
 jni_new_jpl_error(
	atom_t		tag,
	atom_t		msg
    )
    {
    term_t	e = PL_new_term_ref();

    PL_unify_term(e,
		  PL_FUNCTOR, JNI_functor_error_2,
		    PL_FUNCTOR, JNI_functor_jpl_error_1,
		      PL_FUNCTOR, JNI_functor_at_1,
						PL_ATOM, tag,
				    PL_ATOM, msg);
    return e;
    }


/* test for a raised exception; clear and report it if found */
static bool
jni_check_exception()
    {
    jobject	ej;	/* the pending Java exception, if any */
    jobject	c;	/* its class */
    jobject	s;	/* its class name as a JVM String, for the report */
    term_t	ep;	/* a newly created Prolog exception */
    long	i;	/* temp for an iref denoting a Java exception */
	atom_t		tag;	/* temp for a tag denoting a Java exception */
	atom_t		msg;	/* temp for impl-def comment (classname) within error/2 */

    if ( (ej=(*env)->ExceptionOccurred(env)) == NULL )
	{
	return TRUE;
	}
    else
	{
	(*env)->ExceptionClear(env);	/* clear "exception-pending" state so we can do JNI calls */
		if (  (c=(*env)->GetObjectClass(env,ej)) != NULL ) /* get class of exception */
			{
			if ( (s=(*env)->CallObjectMethod(env,c,c_getName)) != NULL ) /* get name of class */
				{
		if ( jni_object_to_iref(ej,&i) )
		    {
					if ( jni_iref_to_tag(i,&tag) )
			{
						if ( jni_String_to_atom(env,s,&msg) )
			    {
							ep = jni_new_java_exception(tag,msg);
			    }
			else
			    {
							ep = jni_new_jpl_error(PL_new_atom("FailedToGetUTFCharsOfNameOfClassOfException"),tag);
			    }
			}
		    else
			{
						ep = jni_new_jpl_error(PL_new_atom("FailedToConvertExceptionIrefToTagatom"),JNI_atom_null);
			}
		    }
		else
		    {
					ep = jni_new_jpl_error(PL_new_atom("FailedToConvertExceptionObjectToIref"),JNI_atom_null);
		    }
		(*env)->DeleteLocalRef(env,s);
		}
	    else
		{
				ep = jni_new_jpl_error(PL_new_atom("FailedToGetNameOfClassOfException"),JNI_atom_null);
		}
	    (*env)->DeleteLocalRef(env,c);
	    }
	else
	    {
			ep = jni_new_jpl_error(PL_new_atom("FailedToGetClassOfException"),JNI_atom_null);
	    }
	return	PL_raise_exception(ep);
	}
    }


/*=== buffer and method param transput ============================================================= */

static foreign_t
 jni_byte_buf_length_to_codes_plc(
	term_t		tbb,	/* +integer */
	term_t		tlen,	/* +integer */
	term_t		tcs		/* -term */
    )
    {
    functor_t	fn;
    term_t	a1;
    atom_t	a;
    term_t	a2;
    jbyte	*bb;
    int		len;
    int		i;
    term_t	tl = PL_copy_term_ref( tcs);
    term_t	ta = PL_new_term_ref();
    void	*ptr;

    if	(   !(	PL_get_functor(tbb,&fn)
	    &&	fn==JNI_functor_jbuf_2
	    &&	(   a2=PL_new_term_ref(),
		    PL_get_arg(2,tbb,a2)
		)
	    &&	PL_get_atom(a2,&a)
	    &&	a==JNI_atom_byte
	    &&	(   a1=PL_new_term_ref(),
		    PL_get_arg(1,tbb,a1)
		)
	    &&	PL_get_pointer(a1,&ptr)
	    )
	||  !PL_get_integer(tlen,&len)
	)
	{
	return FALSE;
	}
    bb = ptr;

    for ( i=0 ; i<len ; i++ )
	{
	if  (	!PL_unify_list(tl,ta,tl)
	    ||	!PL_unify_integer(ta,(int)(bb[i]))
	    )
	    {
	    return FALSE;
	    }
	}
    return PL_unify_nil( tl);
    }


static foreign_t		/* can a "user" mistake cause this to fail? */
jni_param_put_plc(
	term_t		tn, 	/* +integer: index, as Prolog integer, of this method param (0 -> first) */
	term_t		txc,	/* +integer: transput code, as Prolog integer, appropriate to this param */
	term_t		tt, 	/* +term: param value as datum (value or ref) */
	term_t		tjvp	/* +pointer: param buffer (allocated just for this call) */
    )
    {
    int		n;	/* got from tn (see above) */
    int		xc;	/* got from txc (see above) */
    jvalue	*jvp;	/* got from tjvp (see above) */
    functor_t	fn;	/* temp for conversion macros */
    term_t	a1;	/*  " */
    atom_t	a;	/*  " */
    int		i;	/*  " */
	int64_t		i64;	/*	" */
	double		d;		/*	" */

    if ( !PL_get_integer(tn,&n) ||
	 !PL_get_integer(txc,&xc) ||
		 !PL_get_pointer(tjvp,(void*)&jvp) )
	{
	return FALSE;
	}

    switch ( xc )
	{
    case JNI_XPUT_BOOLEAN:
	return	JNI_term_to_jboolean(tt,jvp[n].z);

    case JNI_XPUT_BYTE:
		return	JNI_term_to_jbyte(tt,jvp[n].b);

    case JNI_XPUT_CHAR:
		return	JNI_term_to_jchar(tt,jvp[n].c);

    case JNI_XPUT_SHORT:
		return	JNI_term_to_jshort(tt,jvp[n].s);

    case JNI_XPUT_INT:
	return	JNI_term_to_jint(tt,jvp[n].i);

    case JNI_XPUT_LONG:
	return	JNI_term_to_jlong(tt,jvp[n].j);

    case JNI_XPUT_FLOAT:
		return	JNI_term_to_jfloat(tt,jvp[n].f);

    case JNI_XPUT_DOUBLE:
	return	JNI_term_to_jdouble(tt,jvp[n].d);

    case JNI_XPUT_REF:
	return	JNI_term_to_ref(tt,jvp[n].l);

    default:
	return	FALSE;	/* unknown or inappropriate JNI_XPUT_* code */
	}
    }


/* for completeness, allocates zero-length buffers too, while avoiding malloc() problems */
static foreign_t
jni_alloc_buffer_plc(
	term_t	txc,	/* +integer: transput code */
	term_t	tlen,	/* +integer: required length (# items) */
	term_t	tbp 	/* -pointer: PL_POINTER to newly allocated buffer */
    )
    {
    int	    xc;
    int	    len;
    void    *bp;

    return  PL_get_integer(txc,&xc)
	&&  ( ( xc>=JNI_XPUT_BOOLEAN && xc<=JNI_XPUT_DOUBLE ) || xc==JNI_XPUT_JVALUE )
	&&  PL_get_integer(tlen,&len)
	&&  len >= 0
	&&  (bp=malloc((len==0?1:len)*size[xc])) != NULL    /* avoid (unsafe) malloc(0) */
	&&  (	PL_unify_pointer(tbp,(void*)bp)
	    ?	TRUE
	    :	( free(bp), FALSE)
	    )
	;
    }


static foreign_t
jni_free_buffer_plc(
	term_t	tbp 	/* +integer: PL_POINTER to redundant buffer */
    )
    {
    void    *bp;

    return  PL_get_pointer(tbp,&bp)
	&&  ( free(bp), TRUE);
    }


static foreign_t
jni_fetch_buffer_value_plc(
	term_t	tbp,	/* +pointer: PL_POINTER to an active buffer from jni_alloc_buffer/3 */
    term_t  ti,	    /* +integer: index into buffer; 0 <= i < length */
	term_t	tv,		/* -term: required value (@(false), @(true), integer or float) from buffer */
    term_t  txc	    /* +integer: transput code (one of JNI_XPUT_*) */
    )
    {
    void    *bp;    /* buffer address (trusted to be valid) */
    int	    i;	    /* buffer index (trusted to be valid) */
    int	    xc;	    /* transput code (range-checked by switch statement) */

    if ( !PL_get_pointer(tbp,&bp) || !PL_get_integer(ti,&i) || !PL_get_integer(txc,&xc) )
	{
	return FALSE;
	}

	switch ( xc ) /* primitive type only */
	{
    case JNI_XPUT_BOOLEAN:
		return	JNI_jboolean_to_term(((jboolean*)bp)[i],tv);

    case JNI_XPUT_CHAR:
		return	PL_unify_integer(tv,((jchar*)bp)[i]);

    case JNI_XPUT_BYTE:
		return	PL_unify_integer(tv,((jbyte*)bp)[i]);

    case JNI_XPUT_SHORT:
		return	PL_unify_integer(tv,((jshort*)bp)[i]);

    case JNI_XPUT_INT:
		return	PL_unify_integer(tv,((jint*)bp)[i]);

    case JNI_XPUT_LONG:
		return	PL_unify_int64(tv,((jlong*)bp)[i]);

    case JNI_XPUT_FLOAT:
		return	PL_unify_float(tv,((jfloat*)bp)[i]);

    case JNI_XPUT_DOUBLE:
		return	PL_unify_float(tv,((jdouble*)bp)[i]);

    default:
	return	FALSE;
	}
    }


static foreign_t
jni_stash_buffer_value_plc(
	term_t	tbp,	/* +integer: PL_POINTER to buffer */
	term_t	ti,		/* +integer: index into buffer */
	term_t	tv,		/* +term: @(false), @(true), integer or float */
	term_t	txc		/* +integer: transput code (one of JNI_XPUT_*) */
    )
    {
    void    *bp;
    int	    i;
    int	    idx;
	int64_t	i64;
    int	    xc;
	double	d;
	functor_t	fn;
	term_t		a1;
	atom_t		a;

	if (  !PL_get_pointer(tbp,&bp)
       || !PL_get_integer(ti,&idx)
       || !PL_get_integer(txc,&xc)
       )
	{
	return FALSE;
	}

    switch ( xc )
	{
    case JNI_XPUT_BOOLEAN:
		return	JNI_term_to_jboolean(tv,((jboolean*)bp)[idx]);

    case JNI_XPUT_CHAR:
		return	JNI_term_to_jchar(tv,((jchar*)bp)[idx]);

    case JNI_XPUT_BYTE:
		return	JNI_term_to_jbyte(tv,((jbyte*)bp)[idx]);

    case JNI_XPUT_SHORT:
		return	JNI_term_to_jshort(tv,((jshort*)bp)[idx]);

    case JNI_XPUT_INT:
		return	JNI_term_to_jint(tv,((jint*)bp)[idx]);

    case JNI_XPUT_LONG:
		return	JNI_term_to_jlong(tv,((jlong*)bp)[idx]);

    case JNI_XPUT_FLOAT:
		return	JNI_term_to_jfloat(tv,((jfloat*)bp)[idx]);

    case JNI_XPUT_DOUBLE:
		return  JNI_term_to_jdouble(tv,((jdouble*)bp)[idx]);

    default:
	return	FALSE;
	}
    }


/*=== JVM initialisation, startup etc. ============================================================= */

static int
jni_get_created_jvm_count()
    {
    jint n;

    return  (	JNI_GetCreatedJavaVMs(NULL,0,&n) == 0		/* what does the '0' arg mean? */
	    ?	n
	    :	-1
	    )
	;
    }
    

#if 0	/* This function is no longer used, having been inlined */
static int /* really just a boolean */
jni_get_env()
    {
    JNIEnv	    *env0 = env;
    int		    r;

    r = (*jvm)->GetEnv(jvm,(void**)&env,JNI_VERSION_1_2) == JNI_OK;
    if ( env != env0 )
	{
     DEBUG(1, Sdprintf( "[new env=%u]\n", (void*)env));
	}
    return r;
    }
#endif /*0*/


#define 		MAX_JVM_OPTIONS 100

static int
jni_create_jvm_c(
    char    *classpath
    )
    {
    JavaVMInitArgs	vm_args;
 /*	char				cpopt[10000]; */
	char				*cpoptp;
    JavaVMOption	opt[MAX_JVM_OPTIONS];
    int			r;
    jint		n;
    int			optn = 0;

    DEBUG(1, Sdprintf( "[creating JVM with 'java.class.path=%s']\n", classpath));
    vm_args.version = JNI_VERSION_1_2;	    /* "Java 1.2 please" */
    if ( classpath )
		{
		cpoptp = (char *)malloc(strlen(classpath)+20);
		strcpy( cpoptp, "-Djava.class.path="); /* was cpopt */
		strcat( cpoptp, classpath);					/* oughta check length... */
      vm_args.options = opt;
		opt[optn].optionString = cpoptp; /* was cpopt */
      optn++;
    }
 /* opt[optn++].optionString = "-Djava.compiler=NONE"; */
 /* opt[optn].optionString = "exit";	    // I don't understand this yet... */
 /* opt[optn++].extraInfo = jvm_exit;		// this function has been moved to jpl_extras.c */
 /* opt[optn].optionString = "abort";	    // I don't understand this yet... */
 /* opt[optn++].extraInfo = jvm_abort;		// this function has been moved to jpl_extras.c */
 /* opt[optn++].optionString = "-Xcheck:jni";    // extra checking of JNI calls */
 /* opt[optn++].optionString = "-Xnoclassgc";    // so method/field IDs remain valid (?) */
 /* opt[optn].optionString = "vfprintf"; */
 /* opt[optn++].extraInfo = fprintf;		    // no O/P, then SEGV */
 /* opt[optn++].extraInfo = xprintf;		    // one message, then SEGV */
 /* opt[optn++].optionString = "-verbose:jni"; */

	if ( jvm_dia != NULL )
		{
		int			i;

		for ( i=0 ; jvm_dia[i]!=NULL ; i++ )
			{
			opt[optn++].optionString = jvm_dia[i];
			}
		jvm_aia = jvm_dia;
		jvm_dia = NULL;
		}

    vm_args.nOptions = optn;
 /* vm_args.ignoreUnrecognized = TRUE; */

    return
	( JNI_GetCreatedJavaVMs(&jvm,1,&n) == 0    /* what does the '1' arg mean? */
	  && n == 1
		  && (*jvm)->GetEnv(jvm,(void**)&env,JNI_VERSION_1_2) == JNI_OK
	/*	  && jni_get_env((void**)&env) */
	? 2				    /* success (JVM already available) */
	: ( (r=JNI_CreateJavaVM(&jvm,(void**)&env,&vm_args)) == 0
	  ? 0				    /* success (JVM created OK) */
	  : ( jvm=NULL, r)		    /* -ve, i.e. some create error */
	  )
	);
    }


static foreign_t
jni_get_created_jvm_count_plc(
    term_t	t1
    )
    {

    return  PL_unify_integer(t1,jni_get_created_jvm_count());
    }


static int
jni_create_jvm(
    char	*cp
    )
    {
    int	    r1;
    int	    r2;

    DEBUG(1, Sdprintf("[JPL: checking for Java VM...]\n"));
    return
	( jvm != NULL
	? 1				    /* already initialised */
	: ( (r1=jni_create_jvm_c(cp)) < 0
	  ? r1				    /* err code from JVM-specific routine */
	  : ( (r2=jni_init()) < 0
	    ? r2			    /* err code from jni_init() */
	    : ( r1 == 0			    /* success code from JVM-specific routine */
	      ? ( DEBUG(0, Sdprintf("[JPL: Java VM created]\n")), r1)
	      : ( DEBUG(0, Sdprintf("[JPL: Java VM found]\n")), r1)
	      )
	    )
	  )
	);
    }


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
JW: Note: recent SWI-Prolog set the environment  using Win32 API. We can
only get the proper value using the   Win32 API; getenv only returns the
value at startup of Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
jni_create_default_jvm()
    {
	int  r;
#ifdef __WINDOWS__
	char *cp;
	DWORD len;

	if ( (len=GetEnvironmentVariable("CLASSPATH", NULL, 0)) > 0 )
	{ cp = malloc(len+1);

	  GetEnvironmentVariable("CLASSPATH", cp, len+1);
	} else
	  cp = NULL;
#else
	char *cp = getenv("CLASSPATH");
#endif

	DEBUG(0, Sdprintf("jni_create_default_jvm(): cp=%s\n", cp));

	if ( (r=jni_create_jvm(cp)) < 0 )
		{
		Sdprintf("[JPL: failed to create Java VM (error %d)]\n", r);
		}
	return r >= 0;    /* e.g. 2 -> "JVM already available" */
    }


static foreign_t
jni_ensure_jvm_plc()
    {

    return  jni_ensure_jvm();
    }


/* NB after any JNI call which clearly indicates success, */
/* it is unnecessary to check for an exception */
/* (potential for slight economy here...) */
static foreign_t
jni_void_0_plc( /* C identifiers distinguished _0_ etc, Prolog name is overloaded */
	term_t		tn		/* +integer */
    )
    {
    int		n;	/* JNI function index */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()           /* ought this either succeed or throw a JPL error? */
	||  !PL_get_integer(tn,&n)      /* ought this either succeed or throw a Prolog type error? */
	)
	{
	return FALSE;
	}

    switch ( n )
	{
    case  17:
	r = ( (*env)->ExceptionClear(env) , TRUE ); /* could just return... */
	break;
    default:
	return FALSE;  /* oughta throw exception (design-time error :-) */
	break;
	}

    return jni_check_exception() && r;
    }


static foreign_t
jni_void_1_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		ta1 	/* +term: Arg1 */
    )
    {
    int		n;	/* JNI function index */
 /* functor_t	fn;	// temp for conversion macros */
 /* term_t	a1;	//  " */
 /* term_t	a2;	//  " */
 /* atom_t	a;	//  " */
 /* char	*cp;	//  " */
 /* int		i;	//  " */
 /* int		xhi;	//  " */
 /* int		xlo;	//  " */
 /* jobject	j;	//  " */
 /* jlong	jl;	//  " */
 /* void	*p1;	// temp for converted (JVM) arg */
    char	*c1;	/*  " */
 /* int		i1;	//  " */
 /* jlong	l1;	//  " */
 /* double	d1;	//  " */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
    case  18:
	r = JNI_term_to_charP(ta1,c1)
	    &&	( (*env)->FatalError(env,(char*)c1) , TRUE );
	break;
    default:
	return FALSE;  /* oughta throw exception (design-time error :-) */
	break;
	}

    return jni_check_exception() && r;
    }


static foreign_t
jni_void_2_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		ta1,	/* +term: Arg1 */
	term_t		ta2 	/* +term: Arg2 */
    )
    {
    int		n;	/* JNI function index */
 /*	functor_t	fn; 	// temp for conversion macros */
 /*	term_t		a1; 	//	" */
 /*	term_t		a2; 	//	" */
 /*	atom_t		a;		//	" */
 /*	char		*cp;	//	" */
 /* int		i;	//  " */
 /* int		xhi;	//  " */
 /* int		xlo;	//  " */
 /* jobject	j;	//  " */
 /* jlong	jl;	//  " */
 /*	void		*p1;	// temp for converted (JVM) arg */
 /*	void		*p2;	//	" */
 /* char	*c1;	//  " */
 /*	char		*c2;	//	" */
 /* int		i1;	//  " */
 /* int		i2;	//  " */
 /* jlong	l1;	//  " */
 /* jlong	l2;	//  " */
 /* double	d1;	//  " */
 /* double	d2;	//  " */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
 /*	case 166: */
 /*		r = JNI_term_to_jstring(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_char) */
 /*			&&	( (*env)->ReleaseStringChars(env,(jstring)p1,(jchar*)p2) , TRUE ); */
 /*		break; */
 /*	case 170: */
 /*		r = JNI_term_to_jstring(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,c2,JNI_atom_byte) */
 /*			&&	( (*env)->ReleaseStringUTFChars(env,(jstring)p1,(char*)c2) , TRUE ); */
 /*		break; */
    default:
	return FALSE;  /* oughta throw exception (design-time error :-) */
	break;
	}

    return jni_check_exception() && r;
    }


static foreign_t
jni_void_3_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		ta1,	/* +term: Arg1 */
	term_t		ta2,	/* +term: Arg2 */
	term_t		ta3 	/* +term: Arg3 */
    )
    {
    int		n;	/* JNI function index */
    functor_t	fn;	/* temp for conversion macros */
    term_t	a1;	/*  " */
 /*	term_t		a2; 	//	" */
    atom_t	a;	/*  " */
 /*	char		*cp;	//	" */
    int		i;	/*  " */
	int64_t		i64;	/*	" */
	double 		d;		/*	" */
 /*	int 		xhi;	//	" */
 /*	int 		xlo;	//	" */
 /* jobject	j;	//  " */
 /*	jlong		jl; 	//	" */
    void	*p1;	/* temp for converted (JVM) arg */
    void	*p2;	/*  " */
    void	*p3;	/*  " */
 /* char	*c1;	//  " */
 /* char	*c2;	//  " */
 /* char	*c3;	//  " */
 /* int		i1;	//  " */
    int		i2;	/*  " */
    int		i3;	/*  " */
 /* jlong	l1;	//  " */
 /* jlong	l2;	//  " */
    jlong	l3;	/*  " */
 /*	float		f1; 	//	" */
 /*	float		f2; 	//	" */
	float		f3; 	/*	" */
 /* double	d1;	//  " */
 /* double	d2;	//  " */
    double	d3;	/*  " */
    jvalue	*jvp = NULL; /* if this is given a buffer, it will be freed after the call */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
    case  63:
	r = JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	( (*env)->CallVoidMethodA(env,(jobject)p1,(jmethodID)p2,jvp) , TRUE );
	break;
    case 104:
	r = JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_ref(ta3,p3)
	    &&	( (*env)->SetObjectField(env,(jobject)p1,(jfieldID)p2,(jobject)p3) , TRUE );
	break;
    case 105:
	r = JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jboolean(ta3,i3)
	    &&	( (*env)->SetBooleanField(env,(jobject)p1,(jfieldID)p2,(jboolean)i3) , TRUE );
	break;
    case 106:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jbyte(ta3,i3)
	    &&	( (*env)->SetByteField(env,(jobject)p1,(jfieldID)p2,(jbyte)i3) , TRUE );
	break;
    case 107:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jchar(ta3,i3)
	    &&	( (*env)->SetCharField(env,(jobject)p1,(jfieldID)p2,(jchar)i3) , TRUE );
	break;
    case 108:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jshort(ta3,i3)
	    &&	( (*env)->SetShortField(env,(jobject)p1,(jfieldID)p2,(jshort)i3) , TRUE );
	break;
    case 109:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	( (*env)->SetIntField(env,(jobject)p1,(jfieldID)p2,(jint)i3) , TRUE );
	break;
    case 110:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jlong(ta3,l3)
	    &&	( (*env)->SetLongField(env,(jobject)p1,(jfieldID)p2,(jlong)l3) , TRUE );
	break;
    case 111:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
			&&	JNI_term_to_jfloat(ta3,f3) /* f3 was d3 */
			&&	( (*env)->SetFloatField(env,(jobject)p1,(jfieldID)p2,(jfloat)f3) , TRUE ); /* f3 was d3 */
	break;
    case 112:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jdouble(ta3,d3)
	    &&	( (*env)->SetDoubleField(env,(jobject)p1,(jfieldID)p2,(jdouble)d3) , TRUE );
	break;
	case 143:
		r = 	JNI_term_to_jclass(ta1,p1)
			&&	JNI_term_to_jmethodID(ta2,p2)
			&&	JNI_term_to_pointer(ta3,jvp)
			&&	( (*env)->CallStaticVoidMethodA(env,(jclass)p1,(jmethodID)p2,jvp) , TRUE );
		break;
    case 154:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_ref(ta3,p3)
	    &&	( (*env)->SetStaticObjectField(env,(jclass)p1,(jfieldID)p2,(jobject)p3) , TRUE );
	break;
    case 155:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jboolean(ta3,i3)
	    &&	( (*env)->SetStaticBooleanField(env,(jclass)p1,(jfieldID)p2,(jboolean)i3) , TRUE );
	break;
    case 156:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jbyte(ta3,i3)
	    &&	( (*env)->SetStaticByteField(env,(jclass)p1,(jfieldID)p2,(jbyte)i3) , TRUE );
	break;
    case 157:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jchar(ta3,i3)
	    &&	( (*env)->SetStaticCharField(env,(jclass)p1,(jfieldID)p2,(jchar)i3) , TRUE );
	break;
    case 158:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jshort(ta3,i3)
	    &&	( (*env)->SetStaticShortField(env,(jclass)p1,(jfieldID)p2,(jshort)i3) , TRUE );
	break;
    case 159:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	( (*env)->SetStaticIntField(env,(jclass)p1,(jfieldID)p2,(jint)i3) , TRUE );
	break;
    case 160:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jlong(ta3,l3)
	    &&	( (*env)->SetStaticLongField(env,(jclass)p1,(jfieldID)p2,(jlong)l3) , TRUE );
	break;
    case 161:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
			&&	JNI_term_to_jfloat(ta3,f3) /* f3 was d3 */
			&&	( (*env)->SetStaticFloatField(env,(jclass)p1,(jfieldID)p2,(jfloat)f3) , TRUE ); /* f3 was d3 */
	break;
    case 162:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_term_to_jdouble(ta3,d3)
	    &&	( (*env)->SetStaticDoubleField(env,(jclass)p1,(jfieldID)p2,(jdouble)d3) , TRUE );
	break;
    case 174:
	r =	JNI_term_to_object_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_ref(ta3,p3)
	    &&	( (*env)->SetObjectArrayElement(env,(jobjectArray)p1,(jsize)i2,(jobject)p3) , TRUE );
	break;
 /*	case 191: */
 /*		r = 	JNI_term_to_boolean_jarray(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_boolean) */
 /*			&&	JNI_term_to_jint(ta3,i3) */
 /*			&&	( (*env)->ReleaseBooleanArrayElements(env,(jbooleanArray)p1,(jboolean*)p2,(jint)i3) , TRUE ); */
 /*		break; */
 /*	case 192: */
 /*		r = 	JNI_term_to_byte_jarray(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_byte) */
 /*			&&	JNI_term_to_jint(ta3,i3) */
 /*			&&	( (*env)->ReleaseByteArrayElements(env,(jbyteArray)p1,(jbyte*)p2,(jint)i3) , TRUE ); */
 /*		break; */
 /*	case 193: */
 /*		r = 	JNI_term_to_char_jarray(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_char) */
 /*			&&	JNI_term_to_jint(ta3,i3) */
 /*			&&	( (*env)->ReleaseCharArrayElements(env,(jcharArray)p1,(jchar*)p2,(jint)i3) , TRUE ); */
 /*		break; */
 /*	case 194: */
 /*		r = 	JNI_term_to_short_jarray(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_short) */
 /*			&&	JNI_term_to_jint(ta3,i3) */
 /*			&&	( (*env)->ReleaseShortArrayElements(env,(jshortArray)p1,(jshort*)p2,(jint)i3) , TRUE ); */
 /*		break; */
 /*	case 195: */
 /*		r = 	JNI_term_to_int_jarray(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_int) */
 /*			&&	JNI_term_to_jint(ta3,i3) */
 /*			&&	( (*env)->ReleaseIntArrayElements(env,(jintArray)p1,(jint*)p2,(jint)i3) , TRUE ); */
 /*		break; */
 /*	case 196: */
 /*		r = 	JNI_term_to_long_jarray(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_long) */
 /*			&&	JNI_term_to_jint(ta3,i3) */
 /*			&&	( (*env)->ReleaseLongArrayElements(env,(jlongArray)p1,(jlong*)p2,(jint)i3) , TRUE ); */
 /*		break; */
 /*	case 197: */
 /*		r = 	JNI_term_to_float_jarray(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_float) */
 /*			&&	JNI_term_to_jint(ta3,i3) */
 /*			&&	( (*env)->ReleaseFloatArrayElements(env,(jfloatArray)p1,(jfloat*)p2,(jint)i3) , TRUE ); */
 /*		break; */
 /*	case 198: */
 /*		r = 	JNI_term_to_double_jarray(ta1,p1) */
 /*			&&	JNI_term_to_jbuf(ta2,p2,JNI_atom_double) */
 /*			&&	JNI_term_to_jint(ta3,i3) */
 /*			&&	( (*env)->ReleaseDoubleArrayElements(env,(jdoubleArray)p1,(jdouble*)p2,(jint)i3) , TRUE ); */
 /*		break; */
    default:
	return FALSE;  /* oughta throw exception (design-time error :-) */
	break;
	}

    if ( jvp != NULL )
	{
	free( jvp);
	}

    return jni_check_exception() && r;
    }


static foreign_t
jni_void_4_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		ta1,	/* +term: Arg1 */
	term_t		ta2,	/* +term: Arg2 */
	term_t		ta3,	/* +term: Arg3 */
	term_t		ta4 	/* +term: Arg4 */
    )
    {
    int		n;	/* JNI function index */
    functor_t	fn;	/* temp for conversion macros */
    term_t	a1;	/*  " */
    term_t	a2;	/*  " */
    atom_t	a;	/*  " */
 /*	char		*cp;	//	" */
    int		i;	/*  " */
 /* int		xhi;	//  " */
 /* int		xlo;	//  " */
 /* jobject	j;	//  " */
 /* jlong	jl;	//  " */
    void	*p1;	/* temp for converted (JVM) arg */
 /*	void		*p2;	//	" */
 /*	void		*p3;	//	" */
    void	*p4;	/*  " */
 /* char	*c1;	//  " */
 /* char	*c2;	//  " */
 /* char	*c3;	//  " */
 /* char	*c4;	//  " */
 /* int		i1;	//  " */
    int		i2;	/*  " */
    int		i3;	/*  " */
 /* int		i4;	//  " */
 /* jlong	l1;	//  " */
 /* jlong	l2;	//  " */
 /* jlong	l3;	//  " */
 /* jlong	l4;	//  " */
 /* double	d1;	//  " */
 /* double	d2;	//  " */
 /* double	d3;	//  " */
 /* double	d4;	//  " */
    jvalue	*jvp = NULL; /* if this is given a buffer, it will be freed after the call */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
 /*	case  93: */
 /*		r = 	JNI_term_to_jobject(ta1,p1) */
 /*			&&	JNI_term_to_jclass(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	( (*env)->CallNonvirtualVoidMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp) , TRUE ); */
 /*		break; */
    case 199:
	r =	JNI_term_to_boolean_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_boolean)
	    &&	( (*env)->GetBooleanArrayRegion(env,(jbooleanArray)p1,(jsize)i2,(jsize)i3,(jboolean*)p4) , TRUE );
	break;
    case 200:
	r =	JNI_term_to_byte_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_byte)
	    &&	( (*env)->GetByteArrayRegion(env,(jbyteArray)p1,(jsize)i2,(jsize)i3,(jbyte*)p4) , TRUE );
	break;
    case 201:
	r =	JNI_term_to_char_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_char)
	    &&	( (*env)->GetCharArrayRegion(env,(jcharArray)p1,(jsize)i2,(jsize)i3,(jchar*)p4) , TRUE );
	break;
    case 202:
	r =	JNI_term_to_short_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_short)
	    &&	( (*env)->GetShortArrayRegion(env,(jshortArray)p1,(jsize)i2,(jsize)i3,(jshort*)p4) , TRUE );
	break;
    case 203:
	r =	JNI_term_to_int_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_int)
	    &&	( (*env)->GetIntArrayRegion(env,(jintArray)p1,(jsize)i2,(jsize)i3,(jint*)p4) , TRUE );
	break;
    case 204:
	r =	JNI_term_to_long_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_long)
	    &&	( (*env)->GetLongArrayRegion(env,(jlongArray)p1,(jsize)i2,(jsize)i3,(jlong*)p4) , TRUE );
	break;
    case 205:
	r =	JNI_term_to_float_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_float)
	    &&	( (*env)->GetFloatArrayRegion(env,(jfloatArray)p1,(jsize)i2,(jsize)i3,(jfloat*)p4) , TRUE );
	break;
    case 206:
	r =	JNI_term_to_double_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_double)
	    &&	( (*env)->GetDoubleArrayRegion(env,(jdoubleArray)p1,(jsize)i2,(jsize)i3,(jdouble*)p4) , TRUE );
	break;
    case 207:
	r =	JNI_term_to_boolean_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_boolean)
	    &&	( (*env)->SetBooleanArrayRegion(env,(jbooleanArray)p1,(jsize)i2,(jsize)i3,(jboolean*)p4) , TRUE );
	break;
    case 208:
	r =	JNI_term_to_byte_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_byte)
	    &&	( (*env)->SetByteArrayRegion(env,(jbyteArray)p1,(jsize)i2,(jsize)i3,(jbyte*)p4) , TRUE );
	break;
    case 209:
	r =	JNI_term_to_char_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_char)
	    &&	( (*env)->SetCharArrayRegion(env,(jcharArray)p1,(jsize)i2,(jsize)i3,(jchar*)p4) , TRUE );
	break;
    case 210:
	r =	JNI_term_to_short_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_short)
	    &&	( (*env)->SetShortArrayRegion(env,(jshortArray)p1,(jsize)i2,(jsize)i3,(jshort*)p4) , TRUE );
	break;
    case 211:
	r =	JNI_term_to_int_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_int)
	    &&	( (*env)->SetIntArrayRegion(env,(jintArray)p1,(jsize)i2,(jsize)i3,(jint*)p4) , TRUE );
	break;
    case 212:
	r =	JNI_term_to_long_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_long)
	    &&	( (*env)->SetLongArrayRegion(env,(jlongArray)p1,(jsize)i2,(jsize)i3,(jlong*)p4) , TRUE );
	break;
    case 213:
	r =	JNI_term_to_float_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_float)
	    &&	( (*env)->SetFloatArrayRegion(env,(jfloatArray)p1,(jsize)i2,(jsize)i3,(jfloat*)p4) , TRUE );
	break;
    case 214:
	r =	JNI_term_to_double_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2)
	    &&	JNI_term_to_jint(ta3,i3)
	    &&	JNI_term_to_jbuf(ta4,p4,JNI_atom_double)
	    &&	( (*env)->SetDoubleArrayRegion(env,(jdoubleArray)p1,(jsize)i2,(jsize)i3,(jdouble*)p4) , TRUE );
	break;
    default:
	return FALSE;  /* oughta throw exception (design-time error :-) */
	break;
	}

    if ( jvp != NULL )
	{
	free( jvp);
	}

    return jni_check_exception() && r;
    }


static foreign_t
jni_func_0_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		tr		/* -term: Result */
    )
    {
    int		n;	/* JNI function index */
 /* functor_t	fn;	// temp for conversion macros */
 /* term_t	a1;	//  " */
 /* term_t	a2;	//  " */
 /*	atom_t		a;		//	" */
 /* char	*cp;	//  " */
 /*	long 		i;		//	" */
 /* int		xhi;	//  " */
 /* int		xlo;	//  " */
 /*	jobject 	j;		//	" */
 /* jlong	jl;	//  " */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
 /*	case   4: */
 /*		r =    JNI_jint_to_term((*env)->GetVersion(env),tr); */
 /*		break; */
 /*	case  15: */
 /*		r =    JNI_jobject_to_term((*env)->ExceptionOccurred(env),tr); */
 /*		break; */
    default:
	return FALSE;  /* oughta throw exception (design-time error :-) */
	break;
	}

    return jni_check_exception() && r;	/* surely NEITHER of these throws an exception! */
    }


static foreign_t
jni_func_1_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		ta1,	/* +term: Arg1 */
	term_t		tr		/* -term: Result */
    )
    {
    int		n;	/* JNI function index */
    functor_t	fn;	/* temp for conversion macros */
    term_t	a1;	/*  " */
 /* term_t	a2;	//  " */
    atom_t	a;	/*  " */
 /*	char		*cp;	//	" */
    long	i;	/*  " */
 /* int		xhi;	//  " */
 /* int		xlo;	//  " */
    jobject	j;	/*  " */
 /* jlong	jl;	//  " */
    void	*p1;	/* temp for converted (JVM) arg */
    char	*c1;	/*  " */
    int		i1;	/*  " */
 /* jlong	l1;	//  " */
 /* double	d1;	//  " */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
    case   6:
	r =	JNI_term_to_charP(ta1,c1)
			&&	JNI_jobject_to_term((*env)->FindClass(env,(char*)c1),tr); /* *NOT* Unicode */
	break;
    case  10:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_jobject_to_term((*env)->GetSuperclass(env,(jclass)p1),tr);
	break;
 /*	case  13: */
 /*		r = 	JNI_term_to_throwable_jobject(ta1,p1) */
 /*			&&	JNI_jint_to_term((*env)->Throw(env,(jthrowable)p1),tr); */
 /*		break; */
 /*	case  27: */
 /*		r = 	JNI_term_to_non_array_jclass(ta1,p1) */
 /*			&&	JNI_jobject_to_term((*env)->AllocObject(env,(jclass)p1),tr); */
 /*		break; */
    case  31:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_jobject_to_term((*env)->GetObjectClass(env,(jobject)p1),tr);
	break;
 /*	case 164: // not used */
 /*		r = 	JNI_term_to_jstring(ta1,p1) */
 /*			&&	JNI_jint_to_term((*env)->GetStringLength(env,(jstring)p1),tr); */
 /*		break; */
 /*	case 167: // not used */
 /*		r = 	JNI_term_to_charP(ta1,c1) */
 /*			&&	JNI_jobject_to_term((*env)->NewStringUTF(env,(char*)c1),tr); */
 /*		break; */
 /*	case 168: */
 /*		r = 	JNI_term_to_jstring(ta1,p1) */
 /*			&&	JNI_jint_to_term((*env)->GetStringUTFLength(env,(jstring)p1),tr); */
 /*		break; */
    case 171:
	r =	JNI_term_to_jarray(ta1,p1)
	    &&	JNI_jint_to_term((*env)->GetArrayLength(env,(jarray)p1),tr);
	break;
    case 175:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_jobject_to_term((*env)->NewBooleanArray(env,(jsize)i1),tr);
	break;
    case 176:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_jobject_to_term((*env)->NewByteArray(env,(jsize)i1),tr);
	break;
    case 177:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_jobject_to_term((*env)->NewCharArray(env,(jsize)i1),tr);
	break;
    case 178:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_jobject_to_term((*env)->NewShortArray(env,(jsize)i1),tr);
	break;
    case 179:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_jobject_to_term((*env)->NewIntArray(env,(jsize)i1),tr);
	break;
    case 180:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_jobject_to_term((*env)->NewLongArray(env,(jsize)i1),tr);
	break;
    case 181:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_jobject_to_term((*env)->NewFloatArray(env,(jsize)i1),tr);
	break;
    case 182:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_jobject_to_term((*env)->NewDoubleArray(env,(jsize)i1),tr);
	break;
 /*	case 217: */
 /*		r = 	JNI_term_to_jobject(ta1,p1) */
 /*			&&	JNI_jint_to_term((*env)->MonitorEnter(env,(jobject)p1),tr); */
 /*		break; */
 /*	case 218: */
 /*		r = 	JNI_term_to_jobject(ta1,p1) */
 /*			&&	JNI_jint_to_term((*env)->MonitorExit(env,(jobject)p1),tr); */
 /*		break; */
    default:
	return	FALSE;	/* oughta throw exception (design-time error :-) */
	break;
	}

    return jni_check_exception() && r;
    }


static foreign_t
jni_func_2_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		ta1,	/* +term: Arg1 */
	term_t		ta2,	/* +term: Arg2 */
	term_t		tr		/* -term: Result */
    )
    {
    int		n;	/* JNI function index */
    functor_t	fn;	/* temp for conversion macros */
    term_t	a1;	/*  " */
 /* term_t	a2;	//  " */
    atom_t	a;	/*  " */
 /*	char		*cp;	//	" */
    long	i;	/*  " */
 /*	int 		xhi;	//	" */
 /*	int 		xlo;	//	" */
    jobject	j;	/*  " */
 /*	jlong		jl; 	//	" */
    void	*p1;	/* temp for converted (JVM) arg */
    void	*p2;	/*  " */
 /*	char		*c1;	//	" */
 /*	char		*c2;	//	" */
 /* int		i1;	//  " */
    int		i2;	/*  " */
 /* jlong	l1;	//  " */
 /* jlong	l2;	//  " */
 /* double	d1;	//  " */
 /* double	d2;	//  " */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
    case  11:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jclass(ta2,p2)
	    &&	JNI_jboolean_to_term((*env)->IsAssignableFrom(env,(jclass)p1,(jclass)p2),tr);
	break;
 /*	case  14: */
 /*		r = 	JNI_term_to_throwable_jclass(ta1,p1) */
 /*			&&	JNI_term_to_charP(ta2,c2) */
 /*			&&	JNI_jint_to_term((*env)->ThrowNew(env,(jclass)p1,(char*)c2),tr); */
 /*		break; */
 /*	case  24: */
 /*		r = 	JNI_term_to_ref(ta1,p1) */
 /*			&&	JNI_term_to_ref(ta2,p2) */
 /*			&&	JNI_jboolean_to_term((*env)->IsSameObject(env,(jobject)p1,(jobject)p2),tr); */
 /*		break; */
 /*	case  32: */
 /*		r = 	JNI_term_to_ref(ta1,p1) */
 /*			&&	JNI_term_to_jclass(ta2,p2) */
 /*			&&	JNI_jboolean_to_term((*env)->IsInstanceOf(env,(jobject)p1,(jclass)p2),tr); */
 /*		break; */
    case  95:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jobject_to_term((*env)->GetObjectField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case  96:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jboolean_to_term((*env)->GetBooleanField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case  97:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jbyte_to_term((*env)->GetByteField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case  98:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jchar_to_term((*env)->GetCharField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case  99:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jshort_to_term((*env)->GetShortField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case 100:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
		&&	JNI_jint_to_term((*env)->GetIntField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case 101:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jlong_to_term((*env)->GetLongField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case 102:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jfloat_to_term((*env)->GetFloatField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case 103:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jdouble_to_term((*env)->GetDoubleField(env,(jobject)p1,(jfieldID)p2),tr);
	break;
    case 145:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jobject_to_term((*env)->GetStaticObjectField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
    case 146:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jboolean_to_term((*env)->GetStaticBooleanField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
    case 147:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jbyte_to_term((*env)->GetStaticByteField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
    case 148:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jchar_to_term((*env)->GetStaticCharField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
    case 149:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jshort_to_term((*env)->GetStaticShortField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
    case 150:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jint_to_term((*env)->GetStaticIntField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
    case 151:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jlong_to_term((*env)->GetStaticLongField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
    case 152:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jfloat_to_term((*env)->GetStaticFloatField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
    case 153:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jfieldID(ta2,p2)
	    &&	JNI_jdouble_to_term((*env)->GetStaticDoubleField(env,(jclass)p1,(jfieldID)p2),tr);
	break;
 /*	case 163: */
 /*		r = 	JNI_term_to_charP(ta1,c1)	// oughta be _jcharP, i.e. Unicode */
 /*			&&	JNI_term_to_non_neg_jint(ta2,i2) */
 /*			&&	JNI_jobject_to_term((*env)->NewString(env,(jchar*)c1,(jsize)i2),tr); */
 /*		break; */
 /*	case 165: */
 /*		r = 	JNI_term_to_jstring(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetStringChars(env,(jstring)p1,(jboolean*)&i2),tr,JNI_atom_boolean) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
 /*	case 169: */
 /*		r = 	JNI_term_to_jstring(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetStringUTFChars(env,(jstring)p1,(jboolean*)&i2),tr,JNI_atom_byte) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
    case 173:
    {   int i;				/* JW: i is long in this function */

        i2 = 0;			/* JW: make compiler happy */
	r =	JNI_term_to_object_jarray(ta1,p1)
	    &&	JNI_term_to_jint(ta2,i2);
    }
    if ( r )
        r = JNI_jobject_to_term((*env)->GetObjectArrayElement(env,(jobjectArray)p1,(jsize)i2),tr);
	break;
 /*	case 183: */
 /*		r = 	JNI_term_to_boolean_jarray(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetBooleanArrayElements(env,(jbooleanArray)p1,(jboolean*)&i2),tr,JNI_atom_boolean) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
 /*	case 184: */
 /*		r = 	JNI_term_to_byte_jarray(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetByteArrayElements(env,(jbyteArray)p1,(jboolean*)&i2),tr,JNI_atom_byte) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
 /*	case 185: */
 /*		r = 	JNI_term_to_char_jarray(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetCharArrayElements(env,(jcharArray)p1,(jboolean*)&i2),tr,JNI_atom_char) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
 /*	case 186: */
 /*		r = 	JNI_term_to_short_jarray(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetShortArrayElements(env,(jshortArray)p1,(jboolean*)&i2),tr,JNI_atom_short) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
 /*	case 187: */
 /*		r = 	JNI_term_to_int_jarray(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetIntArrayElements(env,(jintArray)p1,(jboolean*)&i2),tr,JNI_atom_int) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
 /*	case 188: */
 /*		r = 	JNI_term_to_long_jarray(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetLongArrayElements(env,(jlongArray)p1,(jboolean*)&i2),tr,JNI_atom_long) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
 /*	case 189: */
 /*		r = 	JNI_term_to_float_jarray(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetFloatArrayElements(env,(jfloatArray)p1,(jboolean*)&i2),tr,JNI_atom_float) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
 /*	case 190: */
 /*		r = 	JNI_term_to_double_jarray(ta1,p1) */
 /*			&&	JNI_jbuf_to_term((*env)->GetDoubleArrayElements(env,(jdoubleArray)p1,(jboolean*)&i2),tr,JNI_atom_double) */
 /*			&&	JNI_jboolean_to_term(i2,ta2); */
 /*		break; */
    default:
	return	FALSE;	/* oughta throw exception (design-time error :-) */
	break;
	}

    return jni_check_exception() && r;
    }


static foreign_t
jni_func_3_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		ta1,	/* +term: Arg1 */
	term_t		ta2,	/* +term: Arg2 */
	term_t		ta3,	/* +term: Arg3 */
	term_t		tr		/* -term: Result */
    )
    {
    int		n;	/* JNI function index */
    functor_t	fn;	/* temp for conversion macros */
    term_t	a1;	/*  " */
 /* term_t	a2;	//  " */
    atom_t	a;	/*  " */
 /*	char		*cp;	//	" */
    long	i;	/*  " */
 /*	int 		xhi;	//	" */
 /*	int 		xlo;	//	" */
    jobject	j;	/*  " */
 /*	jlong		jl; 	//	" */
    void	*p1;	/* temp for converted (JVM) arg */
    void	*p2;	/*  " */
    void	*p3;	/*  " */
 /* char	*c1;	//  " */
    char	*c2;	/*  " */
    char	*c3;	/*  " */
    int		i1;	/*  " */
 /* int		i2;	//  " */
 /* int		i3;	//  " */
 /* jlong	l1;	//  " */
 /* jlong	l2;	//  " */
 /* jlong	l3;	//  " */
 /* double	d1;	//  " */
 /* double	d2;	//  " */
 /* double	d3;	//  " */
    jvalue	*jvp = NULL; /* if this is given a buffer, it will be freed after the call */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
    case  30:
	r =	JNI_term_to_non_array_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jobject_to_term((*env)->NewObjectA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
	case  33:
		r = 	JNI_term_to_jclass(ta1,p1)
			&&	JNI_term_to_charP(ta2,c2)
			&&	JNI_term_to_charP(ta3,c3)
			&&	JNI_jmethodID_to_term((*env)->GetMethodID(env,(jclass)p1,(char*)c2,(char*)c3),tr);
		break;
    case  36:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jobject_to_term((*env)->CallObjectMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
    case  39:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jboolean_to_term((*env)->CallBooleanMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
    case  42:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jbyte_to_term((*env)->CallByteMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
    case  45:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jchar_to_term((*env)->CallCharMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
    case  48:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jshort_to_term((*env)->CallShortMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
    case  51:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jint_to_term((*env)->CallIntMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
    case  54:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jlong_to_term((*env)->CallLongMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
    case  57:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jfloat_to_term((*env)->CallFloatMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
    case  60:
	r =	JNI_term_to_jobject(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jdouble_to_term((*env)->CallDoubleMethodA(env,(jobject)p1,(jmethodID)p2,jvp),tr);
	break;
	case  94:
		r = 	JNI_term_to_jclass(ta1,p1)
			&&	JNI_term_to_charP(ta2,c2)
			&&	JNI_term_to_charP(ta3,c3)
			&&	JNI_jfieldID_to_term((*env)->GetFieldID(env,(jclass)p1,(char*)c2,(char*)c3),tr);
		break;
	case 113:
		r = 	JNI_term_to_jclass(ta1,p1)
			&&	JNI_term_to_charP(ta2,c2)
			&&	JNI_term_to_charP(ta3,c3)
			&&	JNI_jmethodID_to_term((*env)->GetStaticMethodID(env,(jclass)p1,(char*)c2,(char*)c3),tr);
		break;
    case 116:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jobject_to_term((*env)->CallStaticObjectMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 119:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jboolean_to_term((*env)->CallStaticBooleanMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 122:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jbyte_to_term((*env)->CallStaticByteMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 125:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jchar_to_term((*env)->CallStaticCharMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 128:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jshort_to_term((*env)->CallStaticShortMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 131:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jint_to_term((*env)->CallStaticIntMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 134:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jlong_to_term((*env)->CallStaticLongMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 137:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jfloat_to_term((*env)->CallStaticFloatMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 140:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_jmethodID(ta2,p2)
	    &&	JNI_term_to_pointer(ta3,jvp)
	    &&	JNI_jdouble_to_term((*env)->CallStaticDoubleMethodA(env,(jclass)p1,(jmethodID)p2,jvp),tr);
	break;
    case 144:
	r =	JNI_term_to_jclass(ta1,p1)
	    &&	JNI_term_to_charP(ta2,c2)
	    &&	JNI_term_to_charP(ta3,c3)
	    &&	JNI_jfieldID_to_term((*env)->GetStaticFieldID(env,(jclass)p1,(char*)c2,(char*)c3),tr);
	break;
    case 172:
	r =	JNI_term_to_non_neg_jint(ta1,i1)
	    &&	JNI_term_to_jclass(ta2,p2)
	    &&	JNI_term_to_ref(ta3,p3)
	    &&	JNI_jobject_to_term((*env)->NewObjectArray(env,(jsize)i1,(jclass)p2,(jobject)p3),tr);
	break;
    default:
	return	FALSE;	/* oughta throw exception (design-time error :-) */
	break;
	}

    if ( jvp != NULL )
	{
	free( jvp);
	}

    return jni_check_exception() && r;
    }


static foreign_t
jni_func_4_plc(
	term_t		tn, 	/* +integer: JNI function index */
	term_t		ta1,	/* +term: Arg1 */
	term_t		ta2,	/* +term: Arg2 */
	term_t		ta3,	/* +term: Arg3 */
	term_t		ta4,	/* +term: Arg4 */
	term_t		tr		/* -term: Result */
    )
    {
    int		n;	/* JNI function index */
 /*	functor_t	fn; 	// temp for conversion macros */
 /*	term_t		a1; 	//	" */
 /*	term_t		a2; 	//	" */
 /*	atom_t		a;		//	" */
 /*	char		*cp;	//	" */
 /*	long 		i;		//	" */
 /*	int 		xhi;	//	" */
 /*	int 		xlo;	//	" */
 /*	jobject 	j;		//	" */
 /*	jlong		jl; 	//	" */
 /*	void		*p1;	// temp for converted (JVM) arg */
 /*	void		*p2;	//	" */
 /*	void		*p3;	//	" */
 /* void	*p4;	//  " */
 /*	char		*c1;	//	" */
 /* char	*c2;	//  " */
 /* char	*c3;	//  " */
 /* char	*c4;	//  " */
 /* int		i1;	//  " */
 /* int		i2;	//  " */
 /* int		i3;	//  " */
 /*	int 		i4; 	//	" */
 /* jlong	l1;	//  " */
 /* jlong	l2;	//  " */
 /* jlong	l3;	//  " */
 /* jlong	l4;	//  " */
 /* double	d1;	//  " */
 /* double	d2;	//  " */
 /* double	d3;	//  " */
 /* double	d4;	//  " */
    jvalue	*jvp = NULL; /* if this is given a buffer, it will be freed after the call */
    jboolean	r;	/* Prolog exit/fail outcome */

    if	(   !jni_ensure_jvm()
	||  !PL_get_integer(tn,&n)
	)
	{
	return FALSE;
	}

    switch ( n )
	{
 /*	case   5: */
 /*		r = 	JNI_term_to_charP(ta1,c1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jbuf(ta3,p3,JNI_atom_byte) */
 /*			&&	JNI_term_to_jint(ta4,i4) */
 /*			&&	JNI_jobject_to_term((*env)->DefineClass(env,(char*)c1,(jobject)p2,(jbyte*)p3,(jsize)i4),tr); */
 /*		break; */
 /*	case  66: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jobject_to_term((*env)->CallNonvirtualObjectMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
 /*	case  69: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jboolean_to_term((*env)->CallNonvirtualBooleanMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
 /*	case  72: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jbyte_to_term((*env)->CallNonvirtualByteMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
 /*	case  75: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jchar_to_term((*env)->CallNonvirtualCharMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
 /*	case  78: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jshort_to_term((*env)->CallNonvirtualShortMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
 /*	case  81: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jint_to_term((*env)->CallNonvirtualIntMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
 /*	case  84: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jlong_to_term((*env)->CallNonvirtualLongMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
 /*	case  87: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jfloat_to_term((*env)->CallNonvirtualFloatMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
 /*	case  90: */
 /*		r = 	JNI_term_to_jclass(ta1,p1) */
 /*			&&	JNI_term_to_jobject(ta2,p2) */
 /*			&&	JNI_term_to_jmethodID(ta3,p3) */
 /*			&&	JNI_term_to_pointer(ta4,jvp) */
 /*			&&	JNI_jdouble_to_term((*env)->CallNonvirtualDoubleMethodA(env,(jobject)p1,(jclass)p2,(jmethodID)p3,jvp),tr); */
 /*		break; */
	default:
		return	FALSE;	/* oughta throw exception (design-time error :-) */
	break;
	}

    if ( jvp != NULL )
	{
	free( jvp);
	}

    return jni_check_exception() && r;
    }


/*=== JPL functions ================================================================================ */

static int
 create_pool_engines();

static int
jpl_num_initial_default_args()	/* used only once, by jpl_do_jpl_init() */
    {
    int		i;

    for ( i=0 ; default_args[i]!=NULL ; i++ )
	{
	}
    return i;
    }


/* outcomes: */
/*	fail to find jpl.*, jpl.fli.* classes or to convert init args to String[]: exception, FALSE */
/*	all OK: TRUE */
static bool
jpl_do_jpl_init(		/* to be called once only, after PL init, before any JPL calls */
    JNIEnv     *env
    )
    {
    jclass	tc;	/* temporary class ref */
    jobject	ta;	/* temporary array ref */
    char	*msg;	/* error message for exceptions thrown here */
    int		i;	/* loop counter */
    jobject	to;	/* temporary (String) object ref */

    if ( jpl_status != JPL_INIT_RAW )	/* jpl init already attempted? (shouldn't happen) */
	{
	DEBUG(1, Sdprintf( "[JPL: jpl_do_jpl_init() called AGAIN (skipping...)]\n"));
	return TRUE;
	}

	/* prerequisites for setting initial default args into String[] pvm_dia: */
    if (    (tc=(*env)->FindClass(env,"java/lang/String")) == NULL
	||  (jString_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (ta=(*env)->NewObjectArray(env,jpl_num_initial_default_args(),jString_c,NULL)) == NULL
		||	(pvm_dia=(*env)->NewGlobalRef(env,ta)) == NULL
	||  ( (*env)->DeleteLocalRef(env,ta), FALSE)
       )
	{
		msg = "jpl_do_jpl_init(): failed to find java.lang.String or create String[] pvm_dia";
	goto err;
	}

	/* copy the initial default args into String[] pvm_dia: */
    for ( i=0 ; default_args[i]!=NULL ; i++ )
	{
	if ( (to=(*env)->NewStringUTF(env,default_args[i])) == NULL )
	    {
	    msg = "jpl_do_jpl_init(): failed to convert an initial default arg to a String";
	    goto err;
	    }
		(*env)->SetObjectArrayElement(env,pvm_dia,i,to);  /* any errors/exceptions to be handled here? */
	}

    if (    (tc=(*env)->FindClass(env,"jpl/JPLException")) == NULL
	||  (jJPLException_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/term_t")) == NULL
	||  (jTermT_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/atom_t")) == NULL
	||  (jAtomT_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/functor_t")) == NULL
	||  (jFunctorT_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/fid_t")) == NULL
	||  (jFidT_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/predicate_t")) == NULL
	||  (jPredicateT_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/qid_t")) == NULL
	||  (jQidT_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/module_t")) == NULL
	||  (jModuleT_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/engine_t")) == NULL
	||  (jEngineT_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/LongHolder")) == NULL
	||  (jLongHolder_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/PointerHolder")) == NULL
	||  (jPointerHolder_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/IntHolder")) == NULL
	||  (jIntHolder_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

		||	(tc=(*env)->FindClass(env,"jpl/fli/Int64Holder")) == NULL
		||	(jInt64Holder_c=(*env)->NewGlobalRef(env,tc)) == NULL
		||	( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/DoubleHolder")) == NULL
	||  (jDoubleHolder_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/StringHolder")) == NULL
	||  (jStringHolder_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/ObjectHolder")) == NULL
	||  (jObjectHolder_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (tc=(*env)->FindClass(env,"jpl/fli/BooleanHolder")) == NULL
	||  (jBooleanHolder_c=(*env)->NewGlobalRef(env,tc)) == NULL
	||  ( (*env)->DeleteLocalRef(env,tc), FALSE)

	||  (jLongHolderValue_f=(*env)->GetFieldID(env,jLongHolder_c,"value","J")) == NULL

	||  (jPointerHolderValue_f=(*env)->GetFieldID(env,jPointerHolder_c,"value","J")) == NULL

	||  (jIntHolderValue_f=(*env)->GetFieldID(env,jIntHolder_c,"value","I")) == NULL

		||	(jInt64HolderValue_f=(*env)->GetFieldID(env,jInt64Holder_c,"value","J")) == NULL

	||  (jDoubleHolderValue_f=(*env)->GetFieldID(env,jDoubleHolder_c,"value","D")) == NULL

	||  (jStringHolderValue_f=(*env)->GetFieldID(env,jStringHolder_c,"value","Ljava/lang/String;")) == NULL

	||  (jObjectHolderValue_f=(*env)->GetFieldID(env,jObjectHolder_c,"value","Ljava/lang/Object;")) == NULL

	||  (jBooleanHolderValue_f=(*env)->GetFieldID(env,jBooleanHolder_c,"value","Z")) == NULL
       )
	{
	msg = "jpl_do_jpl_init(): failed to find jpl.* or jpl.fli.* classes";
	goto err;
	}

    DEBUG(1, Sdprintf( "[jpl_do_jpl_init() sets jpl_status = JPL_INIT_PVM_MAYBE, returns TRUE]\n"));
    jpl_status = JPL_INIT_PVM_MAYBE;
    return TRUE;

err:
    jpl_status = JPL_INIT_JPL_FAILED;
    (*env)->ThrowNew(env,jJPLException_c,msg);
    return FALSE;
    }


/* prerequisite: */
/*	called only from jpl_test_pvm_init() and jpl_do_pvm_init() */
/* outcomes: */
/*	error setting up post-PVM-init JPL state:  throws exception, sets status = PVM_FAILED, returns FALSE */
/*	OK:  sets status = OK, returns TRUE */
static bool
jpl_post_pvm_init(
    JNIEnv	*env,
    int		argc,
    char	**argv
    )
    {
    char	*msg;
    jobject	ta;
    int		i;

    /* Prolog VM is already initialised (by us or by other party) */
    /* retire default init args and set up actual init args: */
	pvm_dia = NULL; 	/* probably oughta delete (global) ref to former args... */
    if (    (ta=(*env)->NewObjectArray(env,argc,jString_c,NULL)) == NULL
		||	(pvm_aia=(*env)->NewGlobalRef(env,ta)) == NULL
	||  ( (*env)->DeleteLocalRef(env,ta), FALSE)
       )
	{
	msg = "jpl_post_pvm_init(): failed to copy actual init args";
	goto err;
	}
    for ( i=0 ; i<argc ; i++ )
	{
	jobject	    to;

	to = (*env)->NewStringUTF(env,argv[i]);
	if ( to == NULL )
	    {
	    msg = "jpl_post_pvm_init(): failed to convert actual PL init arg to String";
	    goto err;
	    }
		(*env)->SetObjectArrayElement(env,pvm_aia,i,to);
	}

    if ( create_pool_engines() != 0 )
	{
	msg = "jpl_post_pvm_init(): failed to create Prolog engine pool";
	goto err;
	}

    jpl_status = JPL_INIT_OK;
    return TRUE;

err:
    (*env)->ThrowNew( env, jJPLException_c, msg);
    jpl_status = JPL_INIT_PVM_FAILED;
    return FALSE;
    }


/* prerequisite: jpl_status != JPL_INIT_RAW */
/* outcomes: */
/*	PVM is not (already) initialised -> FALSE */
/*	PVM is (already) initialised -> TRUE */
/*	error setting up post-PVM-init JPL state -> exception */
static bool
jpl_test_pvm_init(
    JNIEnv	*env
    )
    {
    char	*msg;
    int		argc;
    char	**argv;
 /* jobject	ta; */
 /* int		i; */

    if ( jpl_status == JPL_INIT_RAW )
	{
	msg = "jpl_test_pvm_init(): called while jpl_status == JPL_INIT_RAW";
	goto err;
	}

    if ( jpl_status==JPL_INIT_JPL_FAILED || jpl_status==JPL_INIT_PVM_FAILED )
	{
	msg = "jpl_test_pvm_init(): initialisation has already failed";
	goto err;
	}

    if ( jpl_status == JPL_INIT_OK )
	{
	return TRUE;
	}

    if ( jpl_status == JPL_INIT_PVM_MAYBE )
	{
	/* we test this each time (if not already initialised) in case other foreign code inits the PVM: */
	if ( !PL_is_initialised(&argc,&argv) )	/* PVM not ready? */
	    {
	    /* jpl_status remains = JPL_INIT_PVM_MAYBE */
	    DEBUG(1, Sdprintf( "[pl_test_pvm_init(): PL is not yet initialised: returning FALSE]\n"));
	    return FALSE;   /* already-active Prolog VM not found (NB not an exceptional condition) */
	    }
	else
	    {
	    DEBUG(1, Sdprintf( "[pl_test_pvm_init(): PL is already initialised: proceeding to jpl_post_pvm_init()]\n"));
	    return jpl_post_pvm_init(env,argc,argv);  /* TRUE, FALSE or exception */
	    }
	}

    msg = "jpl_test_pvm_init(): unknown jpl_status value";
    goto err;

err:
    (*env)->ThrowNew( env, jJPLException_c, msg);
    jpl_status = JPL_INIT_PVM_FAILED;
    return FALSE;
    }


/* prerequisite: */
/*	jpl_status == JPL_INIT_PVM_MAYBE */
/* outcomes: */
/*	successful PVM initialisation and subsequent JPL state setup -> TRUE */
/*	any error -> exception */
static bool
jpl_do_pvm_init(
    JNIEnv	*env
    )
    {
    char	*msg;
    int		argc;
    char	**argv;
    int		i;
    jstring	arg;
    char	*cp;

    /* redundant prerequisites check: */
    if	( jpl_status != JPL_INIT_PVM_MAYBE )
	{
	msg = "jpl_do_pvm_init(): called while jpl_status != JPL_INIT_PVM_MAYBE";
	goto err;
	}

    /* copy current default init args into suitable form for PL_initialise(): */
	if ( pvm_dia == NULL )
	{
		msg = "jpl_do_pvm_init(): pvm_dia == NULL";
	goto err;
	}
	argc = (*env)->GetArrayLength(env,pvm_dia);
    if ( argc <= 0 )
	{
	msg = "jpl_do_pvm_init(): there are fewer than 1 default init args";
	goto err;
	}
    if ( (argv=(char**)malloc((argc+1)*sizeof(char*))) == NULL )
	{
	msg = "jpl_do_pvm_init(): malloc() failed for argv";
	goto err;
	}
    for ( i=0 ; i<argc ; i++ )
	{
		arg = (jstring)(*env)->GetObjectArrayElement(env,pvm_dia,i);
	cp = (char*)(*env)->GetStringUTFChars(env,arg,0);
	argv[i] = (char*)malloc(strlen(cp)+1);
	strcpy( argv[i], cp);
	DEBUG(1, Sdprintf( "  argv[%d] = %s\n", i, argv[i]));
	(*env)->ReleaseStringUTFChars( env, arg, cp);
	}
	DEBUG(1, Sdprintf( "	argv[%d] = NULL\n", argc));
    argv[argc] = NULL;
    if ( !PL_initialise(argc,(char**)argv) )	    /* NB not (const char**) */
	{
	msg = "jpl_do_pvm_init(): PL_initialise() failed";
	goto err;
	}
    /* *don't* free argv (must exist for lifetime of Prolog VM) */

    return jpl_post_pvm_init(env,argc,argv);  /* TRUE, FALSE or exception */

err:
    jpl_status = JPL_INIT_PVM_FAILED;
    (*env)->ThrowNew( env, jJPLException_c, msg);
    return FALSE;
    }


static bool
 jpl_ensure_jpl_init_1(
	JNIEnv	   *e
	)
	{
	bool r;

	pthread_mutex_lock( &jvm_init_mutex);
	r = jpl_do_jpl_init(e);
	pthread_mutex_unlock( &jvm_init_mutex);
	return r;
	}


static bool
 jpl_ensure_pvm_init_1(
	JNIEnv	   *e
	)
	{
	bool r;

	pthread_mutex_lock( &pvm_init_mutex);
	if ( !jpl_ensure_jpl_init(e) )
	  return FALSE;
	r = jpl_test_pvm_init(e) || jpl_do_pvm_init(e);
	pthread_mutex_unlock( &pvm_init_mutex);
	return r;
	}


/*=== initialisation-related native Java methods of jpl.fli.Prolog ================================= */

/*
 * Class:	  jpl_fli_Prolog
 * Method:    get_default_init_args
 * Signature: ()[Ljava/lang/String;
 */
/* if not yet init then return default init args as String[] */
/* if already init then return NULL */
/* if already failed to init then throw an exception */
JNIEXPORT jobject JNICALL 
Java_jpl_fli_Prolog_get_1default_1init_1args(
    JNIEnv     *env, 
    jclass	jProlog
    )
    {
    char	*msg;

    if ( !jpl_ensure_jpl_init(env) ) /* lazily do "local" initialisations iff necessary */
	return FALSE;    

    if ( jpl_status==JPL_INIT_JPL_FAILED || jpl_status==JPL_INIT_PVM_FAILED )
	{
	msg = "jpl.fli.Prolog.set_default_init_args(): initialisation has already failed";
	goto err;
	}

    return  (	jpl_test_pvm_init(env)	/* if Prolog VM is initialised */
	    ?	NULL  /* then default init args are no longer defined */
			:	pvm_dia  /* else here they are */
	    )
	;
err:
    (*env)->ThrowNew( env, jJPLException_c, msg);
    return FALSE;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:    set_default_init_args
 * Signature: ([Ljava/lang/String;)Z
 */
/* if the given jargs are null then throw an exception */
/* if already failed to init then throw an exception */
/* if not yet init then set default init args from jargs and return TRUE */
/* if already init then return FALSE */
JNIEXPORT jboolean JNICALL 
Java_jpl_fli_Prolog_set_1default_1init_1args(
    JNIEnv     *env, 
    jclass	jProlog,
    jobject	jargs	    /* oughta be proper array, perhaps zero-length */
    )
    {
    char	*msg;

    if ( !jpl_ensure_jpl_init(env) ) /* lazily do "local" initialisations iff necessary */
        return FALSE;

    if ( jargs == NULL )  /* improper call */
	{
	msg = "jpl.fli.Prolog.set_default_init_args() called with NULL arg";
	goto err;
	}

    if ( jpl_status==JPL_INIT_JPL_FAILED || jpl_status==JPL_INIT_PVM_FAILED )
	{
	msg = "jpl.fli.Prolog.set_default_init_args(): initialisation has already failed";
	goto err;
	}

    if ( jpl_test_pvm_init(env) )  /* if Prolog VM is initialised */
	{
	return FALSE;  /* unable to set default init args (too late: PVM is already initialised) */
	}
    else
	{
		pvm_dia = NULL; 		/* probably oughta delete (global) (?) ref of former args... */
		pvm_dia = (*env)->NewGlobalRef(env,jargs);
	return TRUE;  /* OK: default init args set to those provided */
	}

err:
    (*env)->ThrowNew( env, jJPLException_c, msg);
    return FALSE;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:    get_actual_init_args
 * Signature: ()[Ljava/lang/String;
 */
/* if not yet init then return null */
/* if already init then return actual init args as String[] */
/* if already failed to init then throw an exception */
JNIEXPORT jobject JNICALL 
Java_jpl_fli_Prolog_get_1actual_1init_1args(
    JNIEnv     *env, 
    jclass	jProlog
    )
    {
    char	*msg;

    if ( !jpl_ensure_jpl_init( env) ) /* lazily do "local" initialisations iff necessary */
	return NULL;

    if ( jpl_status==JPL_INIT_JPL_FAILED || jpl_status==JPL_INIT_PVM_FAILED )
	{
	msg = "jpl.fli.Prolog.get_actual_init_args(): initialisation has already failed";
	goto err;
	}

    return  (	jpl_test_pvm_init(env)	/* check PL_initialise() and update local state as appropriate */
			?	pvm_aia  /* here they are */
	    :	NULL  /* PVM not (yet) initialised */
	    );

err:
    (*env)->ThrowNew( env, jJPLException_c, msg);
    return NULL;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:    initialise
 * Signature: ()Z
 */
/* if already init then return FALSE */
/* if already failed to init then throw an exception */
/* else attempt to init and if success then return TRUE else throw an exception */
JNIEXPORT jboolean JNICALL 
Java_jpl_fli_Prolog_initialise(
    JNIEnv	*env, 
    jclass	 jProlog
    )
    {
    char	*msg;

    if ( !jpl_ensure_jpl_init( env) ) /* lazily do "local" initialisations iff necessary */
 	return FALSE;

    if ( jpl_status==JPL_INIT_JPL_FAILED || jpl_status==JPL_INIT_PVM_FAILED )
	{
	msg = "jpl.fli.Prolog.initialise(): initialisation has already failed";
	goto err;
	}

    if ( jpl_test_pvm_init(env) )
	{
	return FALSE;  /* PVM is already initialised */
	}
    else
	{
	jpl_do_pvm_init( env);
	return jpl_test_pvm_init(env);
	}

err:
    (*env)->ThrowNew( env, jJPLException_c, msg);
    return FALSE;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:    halt
 * Signature: (I)V
 */
JNIEXPORT void JNICALL 
Java_jpl_fli_Prolog_halt(
    JNIEnv     *env, 
    jclass	jProlog, 
    jint	jstatus
    )
    {

    (void)jpl_ensure_pvm_init(env);
    PL_halt( (int)jstatus);
    }


/*=== JPL utility functions ======================================================================== */

/*-----------------------------------------------------------------------
 * getLongValue
 * 
 * Retrieves the value in a jpl.fli.LongHolder (or subclass) instance
 * 
 * @param   env		  Java environment
 * @param   jlong_holder  the LongHolder class instance, or null
 * @param   lv		  address to write the retrieved (long) value
 * @return		  success? (the LongHolder was not null)
 *---------------------------------------------------------------------*/
static bool
getLongValue( 
    JNIEnv	*env, 
    jobject	jlong_holder,
    long	*lv
    )
    {
    
    if ( jlong_holder == NULL )
	{
	*lv = 0L;
	return FALSE;
	}
    else    /* Java compilation ensures it's a jpl.fli.LongHolder instance */
	{
	*lv = (long)(*env)->GetLongField(env,jlong_holder,jLongHolderValue_f);
	return TRUE;
	}
    }


/*-----------------------------------------------------------------------
 * getPointerValue
 * 
 * Retrieves the value in a jpl.fli.PointerHolder instance
 * 
 * @param   env		     Java environment
 * @param   jpointer_holder  the PointerHolder class instance, or null
 * @param   pv		     address to write the retrieved (pointer) value
 * @return		     success? (the PointerHolder was not null)
 *---------------------------------------------------------------------*/
static bool
getPointerValue( /* sets pv to jpointer_holder's .value_ (and succeeds), else sets it to NULL (and fails) */
    JNIEnv	*env, 
    jobject	jpointer_holder,
    pointer	*pv
    )
    {
    
    if	( jpointer_holder == NULL )
	{
	*pv = (pointer)NULL;
	return FALSE;
	}
    else    /* Java compilation ensures it's a jpl.fli.PointerHolder instance */
	{
	*pv = (pointer)(*env)->GetLongField(env,jpointer_holder,jPointerHolderValue_f);
	return TRUE;
	}
    }


/*-----------------------------------------------------------------------
 * setPointerValue
 * 
 * Sets the value in a jpl.fli.Pointer class instance (unless it's null)
 * to the supplied value (maybe 0L)
 * 
 * @param   env		     Java environment
 * @param   jpointer_holder  the PointerHolder class instance, or null
 * @param   pv		     the new (pointer) value
 *---------------------------------------------------------------------*/
static bool
setPointerValue( 
    JNIEnv     *env, 
    jobject	jpointer_holder,
    pointer	pv
    )
    {
    
    return  jpointer_holder != NULL
	&&  (	(*env)->SetLongField(env,jpointer_holder,jPointerHolderValue_f,(long)pv),
		TRUE
	    )
	;
    }


/*-----------------------------------------------------------------------
 * setIntValue
 * 
 * Sets the value in a Java IntHolder class instance (unless it's null)
 * to the supplied value
 * 
 * @param   env		 Java environment
 * @param   jint_holder	 the IntHolder class instance, or null
 * @param   iv		 the new (int) value
 *---------------------------------------------------------------------*/
static bool
setIntValue( 
    JNIEnv     *env, 
    jobject	jint_holder,
	jint 		iv
    )
    {
    
    return  jint_holder != NULL
	&&  (	(*env)->SetIntField(env,jint_holder,jIntHolderValue_f,iv),
		TRUE
	    )
	;
    }


#if 0
/*-----------------------------------------------------------------------
 * setInt64Value
 *
 * Sets the value in a Java Int64Holder class instance (unless it's null)
 * to the supplied value
 *
 * @param	env 		   Java environment
 * @param	jint64_holder  the Int64Holder class instance, or null
 * @param	iv			   the new (int64_t) value
 *---------------------------------------------------------------------*/
static bool
 setInt64Value(
	JNIEnv	   *env,
	jobject 	jint64_holder,
	int64_t		i64v
	)
	{
	
	return	jint64_holder != NULL
		&&	(	(*env)->SetLongField(env,jint64_holder,jInt64HolderValue_f,i64v),
				TRUE
			)
		;
	}
#endif


/*-----------------------------------------------------------------------
 * setLongValue
 * 
 * Sets the value in a Java LongHolder class instance (unless it's null)
 * to the supplied value (maybe 0L)
 * 
 * @param   env		  Java environment
 * @param   jlong_holder  the LongHolder class instance, or null
 * @param   lv		  the new (long) value
 *---------------------------------------------------------------------*/
static bool
setLongValue( 
    JNIEnv     *env, 
    jobject	jlong_holder,
	jlong		lv
    )
    {
    
    return  jlong_holder != NULL
	&&  (	(*env)->SetLongField(env,jlong_holder,jLongHolderValue_f,lv),
		TRUE
	    )
	;
    }


/*-----------------------------------------------------------------------
 * setDoubleValue
 * 
 * Sets the value in a Java DoubleHolder class instance (unless it's null)
 * to the supplied value
 * 
 * @param   env		    Java environment
 * @param   jdouble_holder  the DoubleHolder class instance, or null
 * @param   dv		    the new (double) value
 *---------------------------------------------------------------------*/
static bool
setDoubleValue( 
    JNIEnv     *env, 
    jobject	jdouble_holder,
	jdouble		dv
    )
    {
    
    return  jdouble_holder != NULL
	&&  (	(*env)->SetDoubleField(env,jdouble_holder,jDoubleHolderValue_f,dv),
		TRUE
	    )
	;
    }


/*-----------------------------------------------------------------------
 * setStringValue
 * 
 * Sets the value in a Java StringHolder class instance (unless it's null)
 * to the supplied value (maybe null)
 * 
 * @param   env		    Java environment
 * @param   jstring_holder  the StringHolder class instance, or null
 * @param   sv		    the new (jstring) value
 *---------------------------------------------------------------------*/
static bool
setStringValue( 
    JNIEnv	*env, 
    jobject	jstring_holder,
    jstring	sv
    )
    {
    
    return  jstring_holder != NULL
	&&  (	(*env)->SetObjectField(env,jstring_holder,jStringHolderValue_f,sv),
		TRUE
	    )
	;
    }


#if 0
/*-----------------------------------------------------------------------
 * setObjectValue
 * 
 * Sets the value in a Java ObjectHolder class instance (unless it's null)
 * to the supplied value (maybe null)
 * 
 * @param   env		    Java environment
 * @param   jobject_holder  the ObjectHolder class instance, or null
 * @param   ref		    the new (jobject) value
 *---------------------------------------------------------------------*/
static bool
setObjectValue( 
    JNIEnv	*env, 
    jobject	jobject_holder,
    jobject	ref
    )
    {
    
    return  jobject_holder != NULL
	&&  (	(*env)->SetObjectField(env,jobject_holder,jObjectHolderValue_f,ref),
		TRUE
	    )
	;
    }


/*-----------------------------------------------------------------------
 * setBooleanValue
 * 
 * Sets the .value field of a Java BooleanHolder class instance (unless it's null)
 * to the supplied jboolean value
 * 
 * @param   env		    Java environment
 * @param   jboolean_holder the BooleanHolder class instance, or null
 * @param   jb		     the new (jboolean) value
 *---------------------------------------------------------------------*/
static bool
setBooleanValue( 
    JNIEnv	*env, 
    jobject	jboolean_holder,
    jboolean	jb
    )
    {
    
    return  jboolean_holder != NULL
	&&  (	(*env)->SetBooleanField(env,jboolean_holder,jBooleanHolderValue_f,jb),
		TRUE
	    )
	;
    }


/*-----------------------------------------------------------------------
 * updateAtomValue
 * 
 * Updates the value in a Java atom_t class instance (unless it's null)
 * to the supplied value (maybe 0L); unregisters and registers old and new
 * atom references as appropriate.  NB atom_t extends LongHolder.
 * 
 * @param   env		  Java environment
 * @param   jatom_holder  the atom_t class instance, or null
 * @param   atom2	  the new atom reference
 *---------------------------------------------------------------------*/
static bool
updateAtomValue( 
    JNIEnv     *env, 
    jobject	jatom_holder,
    atom_t	atom2	    /* new value (perhaps 0L (?)) */
    )
    {
    atom_t	atom1;	    /* old value (perhaps 0L (?)) */

    if ( jatom_holder == NULL )
	{
	return FALSE;
	}
    else
	{
	atom1 = (atom_t)(*env)->GetLongField(env,jatom_holder,jLongHolderValue_f);
	if ( atom1 != 0L )
	    {
	    PL_unregister_atom( atom1);
	    }
	(*env)->SetLongField(env,jatom_holder,jLongHolderValue_f,(long)atom2);
	if ( atom2 != 0L )
	    {
	    PL_register_atom( atom2);
	    }
	return TRUE;
	}
    }
#endif

/*=== Java-wrapped SWI-Prolog FLI functions ======================================================== */

static int current_pool_engine_handle(PL_engine_t *e);
static int current_pool_engine();


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  action_abort
 * Signature: ()I
 */
JNIEXPORT int JNICALL
 Java_jpl_fli_Prolog_action_1abort(
    JNIEnv     *env, 
    jclass	jProlog
    )
    {
    
	if ( jpl_ensure_pvm_init(env) )
		{
		return PL_action(PL_ACTION_ABORT);
		}
	else
		{
		return -2; /* oughta throw exception? */
		}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  atom_chars
 * Signature: (Ljpl/fli/atom_t;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL									/* the local ref goes out of scope, */
 Java_jpl_fli_Prolog_atom_1chars(							/* but the string itself doesn't */
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jatom
    )
    {
	atom_t		atom;
	jstring 	lref;

    return  (	jpl_ensure_pvm_init(env)
			&&	getLongValue(env,jatom,(long*)&atom)		/* checks jatom != null */
			&&	jni_atom_to_String(env,atom,&lref)
			?	lref
	    :	NULL
	    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  attach_engine
 * Signature: (Ljpl/fli/engine_t;)I
 */
JNIEXPORT int JNICALL
 Java_jpl_fli_Prolog_attach_1engine(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	 jengine
    )
    {
	PL_engine_t  engine;
	int 		 rc;

	if	( !jpl_ensure_pvm_init(env) )
		{
		return -2; /* libpl could not be initialised (oughta throw exception) */
		}

	rc = current_pool_engine_handle(&engine);
	DEBUG(0, Sdprintf( "attach_engine(): current_engine=%p, thread_self=%d, pool_id=%d\n", engine, PL_thread_self(), rc));

	if	( !getPointerValue(env,jengine,(pointer*)&engine) ) 					/* checks jengine isn't null */
		{
		return -3; /* null engine holder */
		}

	DEBUG(0, Sdprintf( "attach_engine(): new_engine=%p\n", engine));

	if ( (rc=PL_set_engine(engine,NULL)) == PL_ENGINE_SET )
		{
		return 0; /* OK */
		}
	else
		{
		return -1; /* bad engine status: oughta throw exception */
		}
    
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  close_query
 * Signature: (Ljpl/fli/qid_t;)V
 */
JNIEXPORT void JNICALL 
 Java_jpl_fli_Prolog_close_1query(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jqid
    )
    {
	qid_t		qid;

	DEBUG(1, Sdprintf( ">close_query(env=%lu,jProlog=%lu,jquid=%u)...\n", (long)env, (long)jProlog, (long)jqid));
    if	(   jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jqid,(long*)&qid)							/* checks that jqid != NULL */
	)
		{
		PL_close_query( qid);											/* void */
		DEBUG(1, Sdprintf( "  ok: PL_close_query(%lu)\n", (long)qid));
	}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  compare
 * Signature: (Ljpl/fli/term_t;Ljpl/fli/term_t;)I
 */
JNIEXPORT jint JNICALL													/* returns -1, 0 or 1 (or -2 for error) */
 Java_jpl_fli_Prolog_compare(
    JNIEnv	*env, 
    jclass	jProlog, 
	jobject 	jterm1, 
	jobject 	jterm2
    )
    {
	term_t		term1;
	term_t		term2;
    
	DEBUG(1, Sdprintf( ">compare(term1=%lu,term2=%lu)\n", (long)jterm1, (long)jterm2));
	if	(	jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jterm1,(long*)&term1)						/* checks jterm1 isn't null */
		&&	getLongValue(env,jterm2,(long*)&term2)						/* checks jterm2 isn't null */
	    )
		{
		DEBUG(1, Sdprintf( "> PL_compare( %u, %u)", term1, term2));
		return PL_compare(term1,term2); 								/* returns -1, 0 or 1 */
		}
	else
		{
		return -2; /* oughta throw an exception... */
		}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  cons_functor_v
 * Signature: (Ljpl/fli/term_t;Ljpl/fli/functor_t;Ljpl/fli/term_t;)V
 */
JNIEXPORT void JNICALL
 Java_jpl_fli_Prolog_cons_1functor_1v(
    JNIEnv	*env, 
    jclass	jProlog, 
	jobject 	jterm,
	jobject 	jfunctor,
	jobject 	jterm0
    )
    {
	term_t		term;
	functor_t	functor;
	term_t		term0;

	if	(	jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jterm,(long*)&term)				/* checks that jterm isn't null */
		&&	getLongValue(env,jfunctor,(long*)&functor)		/* checks that jfunctor isn't null */
		&&	getLongValue(env,jterm0,(long*)&term0)			/* checks that jterm0 isn't null */
	    )
		{
		PL_cons_functor_v( term, functor, term0);
		}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  copy_term_ref
 * Signature: (Ljpl/fli/term_t;)Ljpl/fli/term_t;
 */
JNIEXPORT jobject JNICALL 
 Java_jpl_fli_Prolog_copy_1term_1ref(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jfrom
    )
    {
    jobject	rval;
	term_t		term;
	term_t		term2;

    return  (	jpl_ensure_pvm_init(env)
		 /* &&	jfrom != NULL	// redundant: getLongValue checks this */
			&&	getLongValue(env,jfrom,(long*)&term)		/* SWI RM implies must be non-null */
			&&	(rval=(*env)->AllocObject(env,jTermT_c)) != NULL
			&&	( (term2=PL_copy_term_ref(term)) , TRUE )	/* SWI RM -> always succeeds */
			&&	setLongValue(env,rval,(long)term2)
	    ?	rval
	    :	NULL							/* oughta warn of failure? */
	    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  current_engine
 * Signature: ()Ljpl/fli/engine_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_current_1engine(
    JNIEnv     *env, 
	jclass		jProlog
    )
    {
	PL_engine_t engine;
    jobject	rval;

    return  (	jpl_ensure_pvm_init(env)
			&&	PL_thread_self() != -1
			&&	( current_pool_engine_handle(&engine) , TRUE )
			&&	(rval=(*env)->AllocObject(env,jEngineT_c)) != NULL
			&&	setPointerValue(env,rval,(pointer)engine)
	    ?	rval
			:	NULL
	    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  current_engine_is_pool
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_current_1engine_1is_1pool(
    JNIEnv     *env, 
	jclass		 jProlog
    )
    {
    
	if ( jpl_ensure_pvm_init(env) )
		{
		return current_pool_engine() >= 0;
		}
	else
		{
		return FALSE; /* libpl could not be initialised: oughta throw exception */
		}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  discard_foreign_frame
 * Signature: (Ljpl/fli/fid_t;)V
 */
JNIEXPORT void JNICALL
 Java_jpl_fli_Prolog_discard_1foreign_1frame(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jfid
    )
    {
	fid_t		fid;

	if	(	jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jfid,(long*)&fid)				/* checks that jfid isn't null */
	    )
		{
		PL_discard_foreign_frame(fid);
		}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  exception
 * Signature: (Ljpl/fli/qid_t;)Ljpl/fli/term_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_exception(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jqid
    )
    {
	qid_t		qid;
    term_t	term;
	jobject 	term_t; 	/* return value */

	DEBUG(1, Sdprintf( ">exception(jqid=%lu)\n", (long)jqid));
	return	(	jpl_ensure_pvm_init(env)
			&&	( DEBUG(1, Sdprintf( "	ok: jpl_ensure_pvm_init(env)\n")), TRUE )
		 /* &&	jqid != NULL	// redundant */
			&&	( DEBUG(1, Sdprintf( "	ok: jqid != NULL\n")), TRUE )
			&&	getLongValue(env,jqid,(long*)&qid)						/* checks that jqid isn't null */
			&&	( DEBUG(1, Sdprintf( "	ok: getLongValue(env,jqid,(long*)&qid)\n")), TRUE )
			&&	( (term=PL_exception(qid)) , TRUE ) 					/* we'll build a term_t object regardless */
			&&	( DEBUG(1, Sdprintf("  ok: ( (term=PL_exception(qid)), TRUE)\n")), TRUE )
			&&	(term_t=(*env)->AllocObject(env,jTermT_c)) != NULL
			&&	( DEBUG(1, Sdprintf( "	ok: (term_t=(*env)->AllocObject(env,jTermT_c)) != NULL\n")), TRUE )
			&&	setLongValue(env,term_t,(long)term)
			&&	( DEBUG(1, Sdprintf( "	ok: setLongValue(env,term_t,(long)term)\n")), TRUE )
			?	(
					DEBUG(1, Sdprintf("  =%lu\n",(long)term_t)),
					term_t
				)
			:	NULL													/* oughta diagnose failure? */
			)
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  get_arg
 * Signature: (ILjpl/fli/term_t;Ljpl/fli/term_t;)Z
 */
JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1arg(
    JNIEnv     *env, 
    jclass	jProlog, 
	jint		jindex, 
	jobject 	jterm,
	jobject 	jarg
    )
    {
    term_t	term;
	term_t		arg;

    return  jpl_ensure_pvm_init(env)
		&&	jarg != NULL									/* don't proceed if this holder is null */
		&&	getLongValue(env,jterm,(long*)&term)				/* checks that jterm isn't null */
		&&	( arg=PL_new_term_ref() , TRUE )				/* Fred used jarg's original term ref (?) */
		&&	PL_get_arg(jindex,term,arg)
		&&	setLongValue(env,jarg,(long)arg)
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  get_atom_chars
 * Signature: (Ljpl/fli/term_t;Ljpl/fli/StringHolder;)Z
 */
JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1atom_1chars(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jterm,
	jobject 	jstring_holder
    )
    {
    term_t	term;
	atom_t		a;
	jstring 	string;

    return  jpl_ensure_pvm_init(env)
		&&	jstring_holder != NULL							/* don't call PL_get_atom_chars if this is null */
		&&	getLongValue(env,jterm,(long*)&term)			/* confirms that jterm != NULL */
		&&	PL_get_atom(term,&a)							/* confirms that term is an atom */
		&&	jni_atom_to_String(env,a,&string)				/* Unicode-aware */
		&&	setStringValue(env,jstring_holder,string)
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  get_c_lib_version
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_get_1c_1lib_1version(
    JNIEnv     *env, 
	jclass		jProlog
    )
    {

	return (*env)->NewStringUTF(env,JPL_C_LIB_VERSION); /* JPL_C_LIB_VERSION is always Latin-1 */
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  get_float
 * Signature: (Ljpl/fli/term_t;Ljpl/fli/DoubleHolder;)Z
 */
JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1float(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jterm,
	jobject 	jdouble_holder
    )
    {
    term_t	term;
	double		d;

    return  jpl_ensure_pvm_init(env)
		&&	jdouble_holder != NULL
		&&	getLongValue(env,jterm,(long*)&term)				/* confirms that jterm isn't null */
		&&	PL_get_float(term,&d)
		&&	setDoubleValue(env,jdouble_holder,d)
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  get_integer
 * Signature: (Ljpl/fli/term_t;Ljpl/fli/Int64Holder;)Z
 */
JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1integer(
    JNIEnv     *env, 
    jclass	jProlog, 
    jobject	jterm,
	jobject 	jint64_holder
    )
    {
    term_t	term;
	int64_t		i64;

    return  jpl_ensure_pvm_init(env)
		&&	jint64_holder != NULL
		&&	getLongValue(env,jterm,(long*)&term)				/* confirms that jterm isn't null */
		&&	PL_get_int64(term,&i64)
		&&	setLongValue(env,jint64_holder,i64)
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  get_name_arity
 * Signature: (Ljpl/fli/term_t;Ljpl/fli/StringHolder;Ljpl/fli/IntHolder;)Z
 */
JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1name_1arity(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jterm,
	jobject 	jname_holder,								/* we trust this is a StringHolder */
	jobject 	jarity_holder								/* we trust this is an IntHolder */
    )
    {
    term_t	term;
	atom_t		atom;
	jstring 	jname;
	int 		arity;

    return  jpl_ensure_pvm_init(env)
		&&	jname_holder != NULL							/* don't proceed if this holder is null */
		&&	jarity_holder != NULL							/* don't proceed if this holder is null */
		&&	getLongValue(env,jterm,(long*)&term)			/* confirms that jterm isn't null */
		&&	PL_get_name_arity(term,&atom,&arity)			/* proceed to register transient atom ref */
		&&	jni_atom_to_String(env,atom,&jname)				/* Unicode-aware */
		&&	setStringValue(env,jname_holder,jname)			/* stash String ref in holder */
		&&	setIntValue(env,jarity_holder,arity)			/* stash arity value in holder */
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  get_string_chars
 * Signature: (Ljpl/fli/term_t;Ljpl/fli/StringHolder;)Z
 */
JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1string_1chars(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jterm,
	jobject 	jstring_holder
    )
    {
    term_t	term;
	jstring 	string;

    return  jpl_ensure_pvm_init(env)
		&&	jstring_holder != NULL
		&&	getLongValue(env,jterm,(long*)&term)			/* checks that jterm != NULL */
		&&	jni_string_to_String(env,term,&string)			/* */
		&&	setStringValue(env,jstring_holder,string)		/* ...when sent straight back to JVM */
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  new_atom
 * Signature: (Ljava/lang/String;)Ljpl/fli/atom_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1atom(
    JNIEnv     *env, 
    jclass	jProlog, 
	jstring 	jname
    )
    {
	atom_t		atom;
	jobject 	rval;

	return	(	jpl_ensure_pvm_init(env)
			&&	jname != NULL
			&&	jni_String_to_atom(env,jname,&atom)
			&&	(rval=(*env)->AllocObject(env,jAtomT_c)) != NULL			/* doesn't call any constructor */
			&&	setLongValue(env,rval,(long)atom)
			?	rval
			:	NULL														/* oughta warn of failure? */
			)
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  new_functor
 * Signature: (Ljpl/fli/atom_t;I)Ljpl/fli/functor_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1functor(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jatom,		/* read-only */
	jint		jarity
    )
    {
	term_t		atom;
	functor_t	functor;
	jobject 	rval;

	return	(	jpl_ensure_pvm_init(env)
			&&	jarity >= 0
			&&	getLongValue(env,jatom,(long*)&atom)						/* checks jatom isn't null */
			&&	(rval=(*env)->AllocObject(env,jFunctorT_c)) != NULL
			&&	(functor=PL_new_functor(atom,(int)jarity)) != 0L
			&&	setLongValue(env,rval,(long)functor)
			?	rval
			:	NULL													/* oughta warn of failure? */
    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  new_module
 * Signature: (Ljpl/fli/atom_t;)Ljpl/fli/module_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1module(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jatom
    )
    {
	atom_t		atom;
	module_t	module;
	jobject 	rval;

	return	(	jpl_ensure_pvm_init(env)
			&&	getLongValue(env,jatom,(long*)&atom)						/* checks that jatom isn't null */
			&&	( (module=PL_new_module(atom)) , TRUE )
			&&	(rval=(*env)->AllocObject(env,jModuleT_c)) != NULL
			&&	setPointerValue(env,rval,(pointer)module)
			?	rval
			:	NULL													/* oughta warn of failure? */
    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  new_term_ref
 * Signature: ()Ljpl/fli/term_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1term_1ref(
    JNIEnv     *env, 
	jclass		jProlog
    )
    {
	jobject 	rval;

	return	(	jpl_ensure_pvm_init(env)
			&&	(rval=(*env)->AllocObject(env,jTermT_c)) != NULL
			&&	setLongValue(env,rval,(long)PL_new_term_ref())
			?	rval
			:	NULL
    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  new_term_refs
 * Signature: (I)Ljpl/fli/term_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1term_1refs(
    JNIEnv     *env, 
    jclass	jProlog, 
	jint		jn
    )
    {
	jobject 	rval;
	term_t		trefs;

	DEBUG(1, Sdprintf( ">new_term_refs(env=%lu,jProlog=%lu,jn=%lu)...\n", (long)env, (long)jProlog, (long)jn));

	return	(	jpl_ensure_pvm_init(env)
			&&	jn >= 0 									/* I hope PL_new_term_refs(0) is defined [ISSUE] */
			&&	(rval=(*env)->AllocObject(env,jTermT_c)) != NULL
			&&	( trefs=PL_new_term_refs((int)jn), TRUE )
			&&	setLongValue(env,rval,(long)trefs)
			&&	( DEBUG(1, Sdprintf("  ok: stashed trefs=%ld into new term_t object\n",(long)trefs)), TRUE )
			?	rval
			:	NULL
    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  next_solution
 * Signature: (Ljpl/fli/qid_t;)Z
 */
JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_next_1solution(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jqid		/* read */
    )
    {
	qid_t		qid;
	int 		rval;		/* for boolean return value */

	DEBUG(1, Sdprintf( ">next_solution(env=%lu,jProlog=%lu,jqid=%lu)...\n", (long)env, (long)jProlog, (long)jqid));
	return	jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jqid,(long*)&qid)						/* checks that jqid isn't null */
		&&	( DEBUG(1, Sdprintf( "	ok: getLongValue(env,jqid,(long*)&qid(%lu))\n",(long)qid)), TRUE )
		&&	( rval=PL_next_solution(qid), TRUE )					/* can call this until it returns FALSE */
		&&	( DEBUG(1, Sdprintf( "	ok: PL_next_solution(qid=%lu)=%u\n",(long)qid,rval)), TRUE )
		&&	(
				DEBUG(1, Sdprintf("  =%lu\n",(long)rval)),
				rval
	    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  object_to_tag
 * Signature: (Ljava/lang/Object;)Ljava/lang/String;
 */
JNIEXPORT jobject JNICALL 
 Java_jpl_fli_Prolog_object_1to_1tag(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jobj
    )
    {
	long		iref;
	char		abuf[23];
    
	/* empirically, unless the two 'ensure' macros are called in this order, */
	/* will crash if this is the first native method called */

	/* Sdprintf("entered object_to_tag...\n"); */

	if ( !jpl_ensure_pvm_init(env) ) {
		/* Sdprintf("jpl_ensure_pvm_init() failed\n"); */
		return NULL;
		}
	/* Sdprintf("jpl_ensure_pvm_init() ok\n"); */

	if ( !jni_ensure_jvm() ) {
		/* Sdprintf("jni_ensure_jvm() failed\n"); */
		return NULL;
		}
	/* Sdprintf("jni_ensure_jvm() ok\n"); */

	if ( jobj!=NULL && jni_object_to_iref(jobj,&iref) ) {
		/* Sdprintf("jni_object_to_iref() done\n"); */
		sprintf( abuf, "J#%020lu", iref);	/* oughta encapsulate this mapping... */
		/* Sdprintf("sprintf() done\n"); */
		return (*env)->NewStringUTF(env,abuf); /* a tag is always Latin-1 */
	} else {
		return NULL;
	}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  open_foreign_frame
 * Signature: ()Ljpl/fli/fid_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_open_1foreign_1frame(
    JNIEnv     *env, 
	jclass		jProlog
    )
    {
	jobject 	 rval;
    
	if	(	jpl_ensure_pvm_init(env)
		&&	(rval=(*env)->AllocObject(env,jFidT_c)) != NULL 		/* get a new fid_t object */
		&&	setLongValue(env,rval,(long)PL_open_foreign_frame())	/* open a frame only if alloc succeeds */
		)
		{
		return rval;
		}
	else
		{
		return NULL;
		}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:    open_query
 * Signature: (Ljpl/fli/module_t;ILjpl/fli/predicate_t;Ljpl/fli/term_t;)Ljpl/fli/qid_t;
 */
JNIEXPORT jobject JNICALL 
Java_jpl_fli_Prolog_open_1query(
    JNIEnv     *env, 
    jclass	jProlog, 
    jobject	jmodule,	/* read */
    jint	jflags,		/* read */
    jobject	jpredicate,	/* read */
    jobject	jterm0		/* read */
    )
    {
    module_t	module;
    predicate_t predicate;
    term_t	term0;
    qid_t	qid;
    jobject	jqid;		/* for returned new QidT object */
    
    DEBUG(1, Sdprintf( ">open_query(env=%lu,jProlog=%lu,jmodule=%lu,jflags=%lu,jpredicate=%lu,jterm0=%lu)...\n",
		       (long)env, (long)jProlog, (long)jmodule, (long)jflags, (long)jpredicate, (long)jterm0));
    return  (	jpl_ensure_pvm_init(env)
	    &&	( getPointerValue(env,jmodule,(pointer*)&module) , TRUE )	/* NULL module is OK below... */
	    &&	( DEBUG(1, Sdprintf("  ok: getPointerValue(env,jmodule=%lu,&(pointer)module=%lu)\n",(long)jmodule,(long)module)), TRUE )
	    &&	getPointerValue(env,jpredicate,(pointer*)&predicate)		/* checks that jpredicate != NULL */
	    &&	( DEBUG(1, Sdprintf("  ok: getPointerValue(env,jpredicate=%lu,&(pointer)predicate=%lu)\n",(long)jpredicate,(long)predicate)), TRUE )
	    &&	getLongValue(env,jterm0,(long*)&term0)			    /* jterm0!=NULL */
	    &&	( (qid=PL_open_query(module,jflags,predicate,term0)) , TRUE )	/* NULL module is OK (?) [ISSUE] */
	    &&	( DEBUG(1, Sdprintf("  ok: PL_open_query(module=%lu,jflags=%u,predicate=%lu,term0=%lu)=%lu\n",(long)module,jflags,(long)predicate,(long)term0,(long)qid)), TRUE )
	    &&	(jqid=(*env)->AllocObject(env,jQidT_c)) != NULL
	    &&	( DEBUG(1, Sdprintf("  ok: AllocObject(env,jQidT_c)=%lu\n",(long)jqid)), TRUE )
	    &&	setLongValue(env,jqid,(long)qid)
	    &&	( DEBUG(1, Sdprintf("  ok: setLongValue(env,%lu,%lu)\n",(long)jqid,(long)qid)), TRUE )
	    &&	( DEBUG(1, Sdprintf("[open_query module = %s]\n", (module==NULL?"(null)":PL_atom_chars(PL_module_name(module))))), TRUE )
	    ?	(
		    DEBUG(1, Sdprintf("  =%lu\n",(long)jqid)),
		    jqid
		)
	    :	NULL							    /* oughta diagnose failure? raise JPL exception? */
	    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  predicate
 * Signature: (Ljava/lang/String;ILjava/lang/String;)Ljpl/fli/predicate_t;
 */
JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_predicate(
    JNIEnv     *env, 
    jclass	jProlog, 
	jstring 	jname,	/* ought not be null */
	jint		jarity,	/* oughta be >= 0 */
	jstring 	jmodule	/* may be null */
    )
    {
	atom_t		pname;	/* the predicate's name, as an atom */
	atom_t		mname;	/* the predicate's module's name, as an atom */
	functor_t	func;	/* the predicate's functor */
	module_t	mod;	/* the predicate's module */
	predicate_t predicate;
	jobject 	rval;

	DEBUG(1, Sdprintf(">predicate(env=%lu,jProlog=%lu,jname=%lu,jarity=%lu,jmodule=%lu)...\n",
					  (long)env, (long)jProlog, (long)jname, (long)jarity, (long)jmodule));
	return	(	jpl_ensure_pvm_init(env)
			&&	jni_String_to_atom(env,jname,&pname)			/* checks that jname isn't NULL */
			&&	jarity >= 0
			&&	( func=PL_new_functor(pname,jarity) , TRUE )	/* "cannot fail" */
			&&	(	jmodule != NULL
				?	jni_String_to_atom(env,jmodule,&mname)		/* checks that jmodule isn't NULL */
				:	( mname=(atom_t)NULL , TRUE )
	    )
			&&	( mod=PL_new_module(mname) , TRUE)
			&&	( predicate=PL_pred(func,mod) , TRUE )
			&&	(rval=(*env)->AllocObject(env,jPredicateT_c)) != NULL
			&&	setPointerValue(env,rval,(pointer)predicate)
			?	(
				   DEBUG(1, Sdprintf("[predicate() module=%s\n",(jmodule==NULL?"(null)":PL_atom_chars(mname)))),
					rval
    )
			:	NULL													/* oughta warn of failure? */
	)
		;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  put_float
 * Signature: (Ljpl/fli/term_t;D)V
 */
JNIEXPORT void JNICALL	/* maybe oughta return jboolean (false iff given object is null) */
 Java_jpl_fli_Prolog_put_1float(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jterm,
	jdouble 	jf
    )
    {
	term_t		term;

    if	(   jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jterm,(long*)&term)					/* checks that jterm isn't null */
	)
	{
		PL_put_float( term, jf);
    }
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  put_integer
 * Signature: (Ljpl/fli/term_t;J)V
 */
JNIEXPORT void JNICALL	/* maybe oughta return jboolean (false iff given object is null) */
 Java_jpl_fli_Prolog_put_1integer(
    JNIEnv     *env, 
    jclass	jProlog, 
    jobject	jterm, 
	jlong		ji												/* why jlong? */
    )
    {
    term_t	term;

	if	(	jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jterm,(long*)&term)					/* checks that jterm isn't null */
    )
    {
		PL_put_integer( term, (int)ji); 						/* ??? */
		}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  put_term
 * Signature: (Ljpl/fli/term_t;Ljpl/fli/term_t;)V
 */
JNIEXPORT void JNICALL	/* maybe oughta return jboolean (false iff given object is null) */
 Java_jpl_fli_Prolog_put_1term(
    JNIEnv     *env, 
    jclass	jProlog, 
    jobject	jterm1, 
    jobject	jterm2
    )
    {
    term_t	term1;
    term_t	term2;
    
    if	(   jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jterm1,(long*)&term1)				/* checks that jterm1 isn't null */
		&&	getLongValue(env,jterm2,(long*)&term2)				/* checks that jterm2 isn't null */
	)
	{
		PL_put_term( term1, term2);
	}
    }


/*
 * Class:     jpl_fli_Prolog
 * Method:    put_jref
 * Signature: (Ljpl/fli/term_t;Ljava/lang/Object;)V
 */
/* added 29/1/2007 PS to support restored but now deprecated jpl.JRef for Rick Moynihan */
JNIEXPORT void JNICALL 
 Java_jpl_fli_Prolog_put_1jref(
	JNIEnv     *env, 
	jclass      jProlog, 
	jobject     jterm, 
	jobject     jref
	)
	{
	term_t		term;
	jobject		j;		// temp for JNI_jobject_to_term(+,-)
	atom_t		a;		//  "
	int			i;		//  "

	if	(	jpl_ensure_pvm_init(env)
		&&	jni_ensure_jvm()
		&&	getLongValue(env,jterm,(long*)&term)				// checks that jterm isn't null
	)
	{
		JNI_jobject_to_term(jref,term);					// assumes term is var; OK if jref == null
	}
	}


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  put_variable
 * Signature: (Ljpl/fli/term_t;)V
 */
JNIEXPORT void JNICALL	/* maybe oughta return jboolean (false iff given object is null) */
 Java_jpl_fli_Prolog_put_1variable(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jterm
    )
    {
	term_t		term;

	if	(	jpl_ensure_pvm_init(env)					/* may throw exception but cannot fail */
		&&	getLongValue(env,jterm,(long*)&term)		/* checks that jterm isn't null */
	)
	{
		PL_put_variable(term);
	}
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  term_type
 * Signature: (Ljpl/fli/term_t;)I
 */
JNIEXPORT jint JNICALL
 Java_jpl_fli_Prolog_term_1type(
    JNIEnv     *env, 
    jclass	jProlog, 
	jobject 	jterm
    )
    {
	term_t		term;
    
    return  (	jpl_ensure_pvm_init(env)
			&&	getLongValue(env,jterm,(long*)&term)					/* checks jterm isn't null */
			?	PL_term_type(term)
			:	-1													/* i.e. when jterm is null */
	    )
	;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:	  unregister_atom
 * Signature: (Ljpl/fli/atom_t;)V
 */
JNIEXPORT void JNICALL
 Java_jpl_fli_Prolog_unregister_1atom(
    JNIEnv     *env, 
	jclass		jProlog,
	jobject 	jatom
    )
    {
	atom_t		atom;

	DEBUG(1, Sdprintf( ">unregister_atom(env=%lu,jProlog=%lu,jatom=%u)...\n", (long)env, (long)jProlog, (long)jatom));

	if	(	jpl_ensure_pvm_init(env)
		&&	getLongValue(env,jatom,(long*)&atom)							/* checks that jatom isn't null */
		)
	{
		PL_unregister_atom( atom);										/* void */
		DEBUG(1, Sdprintf( "  ok: PL_unregister_atom(%lu)\n", (long)atom));
	}
    }


/*=== JPL's Prolog engine pool and thread management =============================================== */

/*
 * Class:	  jpl_fli_Prolog
 * Method:    thread_self
 * Signature: ()I
 */
JNIEXPORT jint JNICALL 
Java_jpl_fli_Prolog_thread_1self(
    JNIEnv	*env, 
    jclass	 jProlog
    )
    {

    if ( jpl_ensure_pvm_init(env) )
	{
	return PL_thread_self();
	}
    else
	{
	return -2;
	}
    }


static int
create_pool_engines()
    {
    int		i;

    DEBUG(1, Sdprintf( "JPL creating engine pool:\n"));
    if ( (engines=malloc(sizeof(PL_engine_t)*JPL_MAX_POOL_ENGINES)) == NULL )
	{
	return -1; /* malloc failed */
	}
    engines_allocated = JPL_MAX_POOL_ENGINES;
    memset(engines, 0, sizeof(PL_engine_t)*engines_allocated);

    DEBUG(1, Sdprintf( "JPL stashing default engine as [0]\n"));
    PL_set_engine( PL_ENGINE_CURRENT, &engines[0]);

    DEBUG(1, Sdprintf( "JPL detaching default engine\n"));
    /* PL_set_engine( NULL, NULL); */

    for ( i=1 ; i<JPL_INITIAL_POOL_ENGINES ; i++ )
	{
	if ( (engines[i]=PL_create_engine(NULL)) == NULL )
	    {
	    return -2; /* PL_create_engine failed */
	    }
	DEBUG(1, Sdprintf( "\tengine[%d]=%p created\n", i, engines[i]));
	}
    return 0;
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:    attach_pool_engine
 * Signature: ()Ljpl/fli/engine_t;
 */
JNIEXPORT jobject JNICALL 
Java_jpl_fli_Prolog_attach_1pool_1engine(
    JNIEnv	*env, 
    jclass	 jProlog
    )
    {
    jobject	rval;
    int		i;

    if ( !jpl_ensure_pvm_init(env) )
	{
	return NULL; /* libpl could not be initialised (oughta throw exception) */
	}

    /* Find an engine. Try setting each of the engines in the pool. */
    /* If they are all in use wait for the condition variable and try again. */
    pthread_mutex_lock( &engines_mutex);
    for ( ; ; )
	{
	try_again:
	for ( i=0 ; i<engines_allocated ; i++ )
	    {
	    int		rc;

	    if ( !engines[i] )
	        continue;

	    if ( (rc=PL_set_engine(engines[i],NULL)) == PL_ENGINE_SET )
		{
		DEBUG(1, Sdprintf( "JPL attaching engine[%d]=%p\n", i, engines[i]));
		pthread_mutex_unlock( &engines_mutex);
		return	(   (rval=(*env)->AllocObject(env,jEngineT_c)) != NULL
			&&  setPointerValue(env,rval,(pointer)engines[i])
			?   rval
			:   NULL
			);
		}
	    if ( rc != PL_ENGINE_INUSE )
		{
		DEBUG(1, Sdprintf( "JPL PL_set_engine fails with %d\n", rc));
		pthread_mutex_unlock( &engines_mutex);
		return NULL; /* bad engine status: oughta throw exception */
		}
	    }

	for ( i=0 ; i<engines_allocated ; i++ )
	{ if ( !engines[i] )
	  { DEBUG(1, Sdprintf("JPL no engines ready; creating new one\n"));
	    if ( (engines[i]=PL_create_engine(NULL)) == NULL )
	    { Sdprintf("JPL: Failed to create engine %d\n", i);
	      return NULL;
	    }
	    goto try_again;
	  }
	}

	DEBUG(1, Sdprintf("JPL no engines ready; waiting...\n"));
	while( pthread_cond_wait(&engines_cond,&engines_mutex) == EINTR )
	    {
	    ;
	    }
	}
    return NULL; /* cannot get here, but gcc -Wall isn't smart enough to realise this :-) (?) */
    }


/* returns pool_index (0+) of given engine (else -1) */
static int
pool_engine_id(
    PL_engine_t e
    )
    {
    int		i;

    for ( i=0 ; i<engines_allocated ; i++ )
	{
	if ( engines[i] && engines[i] == e )
	    {
	    DEBUG(1, Sdprintf( "JPL current  pool engine[%d] = %p (thread_self = %d)\n", i, e, PL_thread_self()));
	    return i;
	    }
	}
    DEBUG(1, Sdprintf( "JPL current non-pool engine = %p (thread_self = %d)\n", e, PL_thread_self()));
    return -1; /* no current pool engine */
    }


/* returns pool_index (0+) of attached engine (else -1), and writes its handle into e */
static int
current_pool_engine_handle(
    PL_engine_t *e
    )
    {

    PL_set_engine( PL_ENGINE_CURRENT, e);
    return pool_engine_id( *e);
    }


/* returns pool index (0+) of attached engine, else -1 */
static int
current_pool_engine()
    {
    PL_engine_t e;

    return current_pool_engine_handle(&e);
    }


/* returns pool_index (0+) of given engine (else -1) */
/*
 * Class:	  jpl_fli_Prolog
 * Method:    pool_engine_id
 * Signature: (Ljpl/fli/engine_t;)I
 */
JNIEXPORT int JNICALL 
Java_jpl_fli_Prolog_pool_1engine_1id(
    JNIEnv	*env, 
    jclass	 jProlog,
    jobject	 jengine
    )
    {
    PL_engine_t	 engine;

    if	( !jpl_ensure_pvm_init(env) )
	{
	return -2; /* libpl could not be initialised (oughta throw exception) */
	}
    if	( !getPointerValue(env,jengine,(long*)&engine) )			/* checks jengine isn't null */
	{
	return -3; /* null engine holder */
	}
    return pool_engine_id(engine);
    }


/*
 * Class:	  jpl_fli_Prolog
 * Method:    release_pool_engine
 * Signature: ()I
 */
JNIEXPORT int JNICALL 
Java_jpl_fli_Prolog_release_1pool_1engine(
    JNIEnv	*env, 
    jclass	 jProlog
    )
    {

    /* Detach our engine, making it available to the pool. */
    /* Signal the condition variable as there may be threads waiting for an engine. */

    if ( jpl_ensure_pvm_init(env) )
	{
	int	    i;
	PL_engine_t e;

	i = current_pool_engine_handle(&e);
	if ( i > 0 )
	    { DEBUG(1, Sdprintf("JPL releasing engine[%d]=%p\n", i, e));
	      PL_set_engine(NULL, NULL);
	      pthread_cond_signal(&engines_cond); /* alert waiters */
	    }
	return i;
	}
    else
	{
	return -2;
	}
    }


static foreign_t
 jni_term_to_jref_plc(
	term_t		tref1,	/* +term: AnyPrologTerm */
	term_t		tref2	/* -term: JRef to a jpl.Term instance which represents that term */
    )
    {
	jobject 	term1;
	atom_t		a;		/*	" */
	long 		i;		/*	" */
	jobject 	j;		/*	" */

	return jni_ensure_jvm()			/* untypically... */
		&& jpl_ensure_pvm_init(env)	/* ...this requires both inits */
		&& (term1=(*env)->AllocObject(env,termt_class)) != NULL
		&& setLongValue(env,term1,(long)tref1) 			/* requires jLongHolderValue_f to be initialised */
		&& JNI_jobject_to_term((*env)->CallStaticObjectMethod(env,term_class,term_getTerm,term1),tref2)
		&& jni_check_exception();
	}


/* serves jni_jref_to_term_plc() */
static bool
 jni_jobject_to_term_byval(
	jobject		jobj,	/* this must be an instance of one of jpl.Term's subclasses */
	term_t		term	/* a Prolog term, as represented by jobj, is *put* into this term ref */
	)
	{
	jobject 	termt;	/* a temporary instance of jpl.fli.term_t (i.e. a "term holder") */
	
	return jni_ensure_jvm()			/* untypically... */
		&& jpl_ensure_pvm_init(env)	/* ...this requires both inits */
		&& (termt=(*env)->AllocObject(env,termt_class)) != NULL
		&& setLongValue(env,termt,(long)term) 			/* requires jLongHolderValue_f to be initialised */
		&& ( (*env)->CallStaticVoidMethod(env,term_class,term_putTerm,jobj,termt) , TRUE )
		&& jni_check_exception()
		;
	}


/* if the first arg is a jref i.e. @(Tag) which refers to a jpl.Term instance, */
/* then the 2nd arg will be matched with a corresponding newly constructed term */
static foreign_t
 jni_jref_to_term_plc(
	term_t		jref,	/* +term: JRef to a jpl.Term instance */
	term_t		termIn	/* -term: term as represented by the JRef */
	)
	{
	functor_t	fn;
	term_t		arg = PL_new_term_ref();
	atom_t		a;
	long		iterm;
	jobject 	jterm;
	term_t		term = PL_new_term_ref();	/* jni_jobject_to_term_byval() will *put* the constructed term in here */

	return jni_ensure_jvm()			/* untypically... */
		&& jpl_ensure_pvm_init(env)	/* ...this requires both inits */
		&& PL_get_functor(jref,&fn)
		&& fn==JNI_functor_at_1
		&& PL_get_arg(1,jref,arg)
		&& PL_get_atom(arg,&a)
		&& jni_tag_to_iref(a,&iterm)
		&& (jterm = (jobject)iterm)
		&& jni_jobject_to_term_byval(jterm,term)	/* NB a bogus Tag could crash this... */
		&& PL_unify( termIn, term)	/* attempt to unify the 2nd arg with the newly constructed term */
		;
	}


static bool
 jni_get_default_jvm_opts_1(
	term_t		args,
	int		 	i,
	char		**jvm_xia
	)
	{
	term_t		tp = PL_new_term_ref();

	if ( jvm_xia[i] == NULL )
		{
		return PL_unify_nil(args);
	}
    else
	{
		return	PL_unify_list(args,tp,args)
			&&	PL_unify_term(tp,
					PL_ATOM, PL_new_atom(jvm_xia[i])
				)
			&&	jni_get_default_jvm_opts_1(args,i+1,jvm_xia)
			;
	}
    }


static foreign_t
 jni_get_jvm_opts(
	term_t		args,	/* -list(atom): the current default JVM initialisation options */
	char		**jvm_xia
    )
    {

	if ( jvm_xia==NULL )
		{
		return FALSE;
		}
	else
		{
		return jni_get_default_jvm_opts_1(args,0,jvm_xia);
		}
	}


static foreign_t
 jni_set_default_jvm_opts_plc(
	term_t		tn,		/* +integer: the qty of options */
	term_t		args	/* +list(atom): the current default JVM initialisation options */
	    )
	{
	int			n;
	int			i;
	term_t		head;
	term_t		list;
	char		*s;

	if ( jvm_dia == NULL )
		{
		return FALSE; /* presumably, JVM is already started, so default options cannot now be set */
		}
	if ( !PL_get_integer(tn,&n) )
		{
		return FALSE;
		}
	for ( i=0 ; i<100 ; i++ )
		{
		if ( jvm_dia[i] == NULL )
			{
			break;
			}
		else
			{
			free(jvm_dia[i]);
			}
		}
	if ( n != i )
		{
		if ( jvm_dia[0] != NULL ) /* definitely not first time? */
			{
			free(jvm_dia);
			}
		jvm_dia = (char**)malloc((n+1)*sizeof(char**));
		}
	head = PL_new_term_ref();      /* variable for the elements */
	list = PL_copy_term_ref(args);    /* copy as we need to write */
	i = 0;
	while ( PL_get_list(list,head,list) )
		{
		if ( PL_get_atom_chars(head,&s) )
			{
			jvm_dia[i] = (char*)malloc(strlen(s)+1);
			strcpy(jvm_dia[i],s);
			}
		else
			{
			return FALSE;
			}
		i++;
		}
	jvm_dia[i] = NULL;
	return PL_get_nil(list);              /* test end for [] */
    }


static foreign_t
 jni_get_default_jvm_opts_plc(
	term_t		args	/* -list(atom): the current default JVM initialisation options */
	)
	{

	return jni_get_jvm_opts(args,jvm_dia);
	}


static foreign_t
 jni_get_actual_jvm_opts_plc(
	term_t		args	/* -list(atom): the actual JVM initialisation options */
    )
    {

	return jni_get_jvm_opts(args,jvm_aia);
    }


/*=== FLI metadata ================================================================================= */

static
 PL_extension predspecs[] =
	{ { "jni_get_created_jvm_count",	 1, jni_get_created_jvm_count_plc,	   0 },
	  { "jni_ensure_jvm",				 0, jni_ensure_jvm_plc, 			   0 },
	  { "jni_tag_to_iref",				 2, jni_tag_to_iref_plc,			   0 },
	  { "jni_hr_info",					 4, jni_hr_info_plc,				   0 },
	  { "jni_hr_table", 				 1, jni_hr_table_plc,				   0 },
	  { "jni_byte_buf_length_to_codes",  3, jni_byte_buf_length_to_codes_plc,  0 },
	  { "jni_param_put",				 4, jni_param_put_plc,				   0 },
	  { "jni_alloc_buffer", 			 3, jni_alloc_buffer_plc,			   0 },
	  { "jni_free_buffer",				 1, jni_free_buffer_plc,			   0 },
	  { "jni_fetch_buffer_value",		 4, jni_fetch_buffer_value_plc, 	   0 },
	  { "jni_stash_buffer_value",		 4, jni_stash_buffer_value_plc, 	   0 },
	  { "jni_void", 					 1, jni_void_0_plc, 				   0 },
	  { "jni_void", 					 2, jni_void_1_plc, 				   0 },
	  { "jni_void", 					 3, jni_void_2_plc, 				   0 },
	  { "jni_void", 					 4, jni_void_3_plc, 				   0 },
	  { "jni_void", 					 5, jni_void_4_plc, 				   0 },
	  { "jni_func", 					 2, jni_func_0_plc, 				   0 },
	  { "jni_func", 					 3, jni_func_1_plc, 				   0 },
	  { "jni_func", 					 4, jni_func_2_plc, 				   0 },
	  { "jni_func", 					 5, jni_func_3_plc, 				   0 },
	  { "jni_func", 					 6, jni_func_4_plc, 				   0 },
	  { "jpl_c_lib_version",			 1, jpl_c_lib_version_1_plc,		   0 },
	  { "jpl_c_lib_version",			 4, jpl_c_lib_version_4_plc,		   0 },
	  { "jni_term_to_jref", 			 2, jni_term_to_jref_plc,			   0 },
	  { "jni_jref_to_term", 			 2, jni_jref_to_term_plc,			   0 },
	  { "jni_get_default_jvm_opts",		 1, jni_get_default_jvm_opts_plc,	   0 },
	  { "jni_set_default_jvm_opts",		 2, jni_set_default_jvm_opts_plc,	   0 },
	  { "jni_get_actual_jvm_opts",		 1, jni_get_actual_jvm_opts_plc,	   0 },
	  { NULL,							 0, NULL,							   0 }
	};


install_t
 install()
	{

	PL_register_extensions( predspecs);
	}

/*=== end of jpl.c ================================================================================= */

