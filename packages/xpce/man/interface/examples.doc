\section{Examples}				\label{sec:examples}

This section gives examples of how the above functions are used to interface
a host language to PCE.  We will use the host language declarations as used
in PCE connected to a Prolog with an Edinburgh compatible syntax.  

\subsection{Calling the virtual machine instructions}

\begin{code}
send(+@Object, +Selector, +Arguments...)
get(+@Object, +Selector, +Arguments..., -Value)
new(?@Object, +Class(+Arguments...))
\end{code}

It is stressed that the interface as described does not inforce this syntax
at all.  Details of the syntax are found in the PCE User's Guide
\cite{PCE:Prolog}.

The host language translates ``send(@foo, tweak, hello, 15, @123456)''
into something functionally equivalent to:

\begin{code}
{ PceObject receiver, selector;
  ArgVector(argv, 3);

  receiver = cToPceAssoc("foo");
  selector = cToPceName("tweak");
  argv[0]  = cToPceName("hello");
  argv[1]  = cToPceInteger(15);
  argv[2]  = cToPceReference(123456);

  pceSend(receiver, selector, 3, argv);
}
\end{code}

Note that the example program does not check for the conversions to
return a valid value (i.e. != \const{PCE_FAIL}).  This is done for the
sake of readability, the host language must obviously perform any such
checks.

Along the same lines: ``get(@123456, distance, @win, X)'' becomes:

\begin{code}
{ PceObject receiver, selector, rval;
  Name selector;
  ArgVector(argv, 1);

  receiver = cToPceReference("123456");
  selector = cToPceName("distance");
  argv[0]  = cToPceAssoc("win");

  if ( (rval = pceGet(receiver, selector, 1, argv)) != PCE_FAIL )
  { return objectToProlog(rval, X);
  }
}
\end{code}

The function \cfunc{objectToProlog()} converts a PCE object into its
Prolog representation.  An outline of this interface function is
in section~\ref{sec:pcetohost}.


Our Prolog defines:

\begin{code}
new(@fred, point(10,20)).
\end{code}

to mean the object name \arg{fred} is to be associated with the object
created from point(10,~10):

\begin{code}
{ PceObject obj, class;
  ArgVector(argv, 2);

  class   = cToPceName("point");	/* class-name rather then class */
  argv[0] = cToPceInteger(10);
  argv[1] = cToPceInteger(20);

  obj = pceNew("fred", class, 2, argv);
}
\end{code}

Calling pceNew(NULL, class, 2, argv) will creat a point object with a
PCE generated integer reference.

Most of these calls may produce PCE garbage objects.  Section~\ref{sec:gc}
defines an interface to the PCE incremental garnage collector to
deal with this problem.


\subsection{Exploiting the interface table}

When textual constants or named object-references have to be
translated from the host-language to PCE or visa-versa, using the
interface-table described in section~\ref{sec:itftable} makes the
interface much faster.  Section~\ref{sec:pcetohost} illustrates how
conversion of PCE constants to host-language constants is realised
using these tables.  Here we will illustrate how an textual constant
is converted into a PCE name.  The \arg{handle} argument is supposed
to hold the textual constant in the internal representation of the
host.  This representation should be a unique%
   \footnote{A {\em unique} representation implies that for every
             two handles, {\em iff} the handle-value is the same, the
	     represented text is the same too.}
mapping of 32 bits constants to the internal representation of the
textual constant.  The function \cfunc{stringOfTextualConstant()} is
the host-language function that translates a textual constant from its
internal value into a C char~*.

\begin{code}
PceName
hostToPceName(handle)
void *handle;
{ PceITFSymbol s;

  if ( s = pceLookupHandle(0, handle) )
    return s->name;
  else
  { PceName name = cToPceName(stringOfTextualConstant(handle));

    pceRegisterName(0, handle, name);
    return name;
  }
}
\end{code}

The conversion of an object-name in the host-language to a PCE object
is similar.  The example assumes Prolog, where a named object is
represented as `@(object_name)'.

\begin{code}
PceObject
hostToPceAssoc(assoc)		
void *assoc;			/* the atom-part of the @/1 term */
{ PceITFSymbol s;

  if ( s = pceLookupHandle(0, assoc) && s->object != PCE_FAIL )
    return s->object;
  else
  { PceObject obj;

    if ( obj = cToPceAssoc(stringOfTextualConstant(assoc)) )
    { pceRegisterObject(0, assoc, obj);
      return obj;
    }

    return PCE_FAIL;		/* No such object */
  }
}
\end{code}


\subsection{Converting PCE values to the host-language} \label{sec:pcetohost}

Both \cfunc{hostSend()}, \cfunc{hostGet()} and \cfunc{pceGet()}
convert PCE objects into a format suitable to the host-language.
Below is an outline for this in PCE/Prolog.  The example uses
a hypothetical interface definition for the Prolog system.  Type
`Term' represents an arbitrary Prolog term.

Please note how the interface exploits the interface table in
the cases \const{PCE_NAME} and \const{PCE_ASSOC}.  The `if'
branch handles the case where the constant has already passed the
PCE/Prolog interface: we can use the registered handle as the
Prolog value.  The `else' branch converts the textual constants
via C char~*.  It registers the converted value to avoid this
conversion on the next lookup.


\begin{code}
objectToProlog(obj, term)
PceObject obj;
Term term;
{ PceCValue value;

  switch( pceToC(obj, &value) )
  { case PCE_INTEGER:
      return unify(term, CtoPrologInteger(value.integer));
    case PCE_REAL:
      return unify(term, CtoPrologFloat(value.real));
    case PCE_NAME:
      if ( value.itf_symbol->handle[0] )
      { return unify(term, value.itf_symbol->handle[0]);
      } else
      { Term atom =
	     CtoPrologAtom(pceCharArrayToC(value.itf_symbol->name));

	pceRegisterName(0, atom, value.itf_symbol->name);
	return unify(term, atom);
      }
    case PCE_ASSOC:
      if ( value.itf_symbol->handle[0] )
      { Term t = CtoPrologCompound(ATOM_ref1, 1,
				   &value.itf_symbol->handle[0]);
	return unify(term, t);
      } else
      { Term atom =
	     CtoPrologAtom(pceCharArrayToC(value.itf_symbol->name));
	Term t = CtoPrologCompound(ATOM_ref1, 1, &atom);
	
	pceRegisterAssoc(0, atom, value.itf_symbol->object);
	return unify(term, t);
      }
    case PCE_REFERENCE:
      { Term ref = CtoPrologInteger(value.integer);
	Term t = CtoPrologCompound(ATOM_ref1, 1, &ref);

	return unify(term, t);
      }
  }
}

\end{code}

% keep doc2tex happy ...
