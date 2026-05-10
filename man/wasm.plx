\chapter{Using SWI-Prolog in your browser (WASM)}
\label{sec:wasm-version}

The SWI-Prolog WebAssembly (WASM) port lets you run SWI-Prolog
directly in your browser.  This is a fairly comprehensive version of
SWI-Prolog that supports the core system as well as a good selection
of \jargon{packages}, including many of the \jargon{foreign packages}.

The WASM port uses \href{https://emscripten.org/}{Emscripten} to
compile the SWI-Prolog source code to WASM, which runs on a virtual
machine that is provided by almost all modern browsers.

To build the SWI-Prolog WASM port, see the building instructions on
\href{https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm/5650}{the
wiki page}

\section{Loading and initializing Prolog}
\label{sec:wasm-loading}

The WASM SWI-Prolog distribution consists of three files:

\begin{description}
\item[\file{swipl-web.js}] This is the main file that must be loaded
  using a \verb$<script>$ element.  It defines a global function \const{SWIPL}
  that loads the other components and gives access to the SWI-Prolog system.
\item[\file{swipl-web.wasm}]
  This is the actual WASM binary containing the compiled C code of the
  core, foreign packages and required libraries.
\item[\file{swipl-web.data}]
  This data contains a \jargon{file system} that is \jargon{mounted} on
  \file{/swipl}.  It contains the Prolog startup code and libraries.
\end{description}

Below is the skeleton for getting access to the Prolog system.  We
first define the global \arg{Prolog} and \arg{Module} objects.  The
\arg{options} object provides module configuration
options. \cfuncref{SWIPL}{} returns a \ctype{Promise} that resolves
when the WASM system is loaded and initialized.  It returns the WASM
\jargon{module}, which contains \const{Module.prolog} to an instance of
the class \ctype{Prolog} that provides a high level interface from
JavaScript.

\begin{code}
let Prolog;
let Module;
const options = {
  // Provide options for customization
};

SWIPL(options).then((module) =>
{ Module = module;
  Prolog = Module.prolog;

  // Start using Prolog
};
\end{code}

The \arg{options} object defines customization properties for the
Emscripten module as well as for Prolog.  We highlight the important
properties below.

\begin{description}
    \termitem{arguments}{}
An \ctype{Array} of \ctype{String} objects that define the commandline
arguments for initializing Prolog. \exam{argv[0]} is \emph{not} part of
this array. Few arguments are useful in this context. The
\cmdlineoption{-q} may be used to suppress the Prolog banner.

    \termitem{locateFile}{}
A \ctype{Function} that is used to translate \file{swipl-web.wasm} and
\file{swipl-web.data} into a (relative) URL. Default is to find these
resources in the same directory of the server. For example, to load
\file{swipl-web.wasm} and \file{swipl-web.data} from \file{/wasm} use

\begin{code}
var Module = {
  ...,
  locateFile: (file) => '/wasm/' + file
}
\end{code}


    \termitem{on_output}{}
A \ctype{Function} that is called when Prolog writes to
\const{user_output} or \const{user_error}. It is passed two arguments: a
\ctype{String} containing the text to emit and one of the constant
strings \const{"stdout"} or \const{"stderr"} to indicate the output
stream. This uses the Emscripten \const{Module.FS.init} option to rebind
the output and error streams, providing behaviour that is similar to the
Emscripten properties \const{print} and \const{printErr}. However, our
passed string contains the newline character and the handler is called
when Prolog \jargon{flushes} the output. Normally the callback should
insert a \verb$<span>$ element that has (at least) the following style:

\begin{code}
.stderr, .stdout {
  white-space: pre-wrap;
  font-family: monospace;
  overflow-wrap: anywhere;
}
\end{code}

Here is a simple implementation of print(), assuming the document has
a \verb$<div id="output">$ node.

\begin{code}
function print(line, cls)
{ const output = document.getElementById("output");
  const node   = document.createElement('span');

  node.className = cls;
  node.textContent = line;
  output.appendChild(node);
};
\end{code}
\end{description}

\subsection{Loading Prolog files}
\label{sec:wasm-consult}

The WASM build ships with the Prolog library and thus Prolog libraries
can be loaded as normal using use_module/1, etc., for example, we can
include the \const{lists} library using this directive.  Note that the
normal \jargon{autoloading} of library code works in the WASM version.

\begin{code}
:- use_module(library(lists)).
\end{code}

When Prolog is in \jargon{asynchronous} mode, i.e., called through
\cfuncref{Prolog.forEach}{}, we can also load code from a \jargon{URL}.
For example, we can load the \const{CHAT80} demo program directly from
\href{https://github.com/JanWielemaker/chat80}{GitHub}
using\footnote{The \chr{\}c continues the quoted atom from the next
line after removing leading white space.}

\begin{code}
?- consult('https://raw.githubusercontent.com/JanWielemaker/\c
            chat80/master/prolog/chat80.pl').
\end{code}

Larger files can be loaded as \fileext{qlf} files. See \secref{qlf} and
qcompile/2. Notably we can create a single qlf file from an application
using the \term{include}{user} option. Below we create a \fileext{qlf}
file from \href{https://github.com/JanWielemaker/chat80}{CHAT80}.  The
resulting \file{chat80.qlf} can be loaded from a URL using consult/1 as
above.

\begin{code}
?- qcompile('chat80.pl', [include(user)]).
\end{code}

There are three ways to load Prolog code from JavaScript: (1) loading
from a string, (2) loading from \verb$<script>$ elements and (3) loading
from URL. Note that all the loading methods return a \ctype{Promise}
that is resolved when loading the data is completed.

\begin{description}
    \cfunction{Promise}{Prolog.load_string}{String, Id}
Load Prolog code from \arg{String}, pretending it was loaded from the
file \arg{Id}.  The \arg{Id} is optional.  When omitted it generates
\verb$/string/1$, \verb$/string/2$, \ldots.

    \cfunction{Promise}{Prolog.load_scripts}{}
Load all scripts from the current document that have their \const{type}
set to \const{text/prolog}.  The file reference for the loaded script
is \exam{/script/Id}, where \arg{Id} is derived from (1) the
\const{id} of the script, (2) the \const{name} of the script or (3)
being the nth Prolog script in the document.  When resolved, the promise
returns an array with the names of the loaded scripts.

    \cfunction{Promise}{Prolog.consult}{...Sources[, Options]}
Load the given \arg{Sources}. Each source is either a file from the
local file system, e.g., \exam{library(lists)} or a URL. The sources are
downloaded and processed sequentially. This uses
\cfuncref{Prolog.forEach}{} calling load_files/1. The returned
\ctype{Promise} returns 1 on success.  If the last argument is an
object, it is treated as options.  Options processed:

\begin{description}
  \keyitem{module}{string}
Defines the module into which a non-module file is loaded or into
which the exports of a module file are imported.  Default is
\const{"user"}.
\keyitem{engine}{Boolean}
If \const{true}, run the compilation in a temporary engine.  This
keeps the main Prolog engine available for other tasks while the
file is being loaded.
\end{description}
\end{description}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Calling Prolog from JavaScript}
\label{sec:wasm-calling}

The \ctype{Prolog} class provides several methods for calling Prolog
from JavaScript.

\begin{description}
    \cfunction{Boolean}{Prolog.call}{Goal}
Processes a Prolog goal represented as a \ctype{String} and returns
\const{true} or \const{false}. This simple calling pattern is intended
for trivial goals such as setting a Prolog flag.  For example, the call
below limits the Prolog stacks to 10Mb.

\begin{code}
Prolog.call("set_prolog_flag(stack_limit, 10 000 000)");
\end{code}

    \cfunction{Query}{Prolog.query}{Goal}
    \nodescription
    \cfunction{Query}{Prolog.query}{Goal, Input}
    \nodescription
    \cfunction{Query}{Prolog.query}{Goal, Input, Options}
Create a Prolog query from a \ctype{String}, optionally binding Prolog
variables embedded in \arg{Goal} from properties of the \ctype{Object}
\arg{Input}. The returned object is an instance of class \ctype{Query}.
This instance can be used as a JavaScript \jargon{iterator}. The value
returned in each iteration is an \ctype{Object} with properties for each
variable in \arg{Goal} that is not in \arg{Input} and does not start
with an underscore.  For example, we can iterate over the members of
a list like below.  Further details on class \ctype{Query} are
provided in \secref{wasm-query}.  The translation of data between
Prolog and JavaScript is described in \secref{wasm-data-conversion}.

\begin{code}
for(let r of Prolog.query("member(Elem,List)",
			  {List: ["aap", "noot", "mies"]}))
{ console.log(r.Elem);
}
\end{code}

This interface is also practical for calling (fast) Prolog predicates to
compute a single answer from an input using the \cfuncref{Query.once}{}
method. Assuming a Prolog predicate \nopredref{fib}{2} that computes the
nth \jargon{Fibonacci} number, we can call this using the code below.
Note that if the \nopredref{fib}{2} fails or raises an exception the
object returned by \cfuncref{Query.once}{} does not contain the
\const{Out} key and thus our function returns \const{undefined}.

\begin{code}
function fib(in, out)
{ return Prolog.query("fib(In,Out)", {In:in}).once().Out;
}
\end{code}

The \exam{.query()} method is indented for fast queries that do not
require the \jargon{yield} mechanism, i.e., the execution should not
require asynchronous operations and the browser is not responsive
during the execution.  Use \cfuncref{Prolog.forEach}{} for asynchronous
queries.

The optional \arg{Options} parameter defines the following options

\begin{description}
  \keyitem{engine}{Boolean}
  If \const{true}, run the query in a temporary engine.  Note that
  JavaScript can only interact with the \jargon{innermost query} of an
  engine.  By using a new engine we can interact with multiple queries,
  using them as \jargon{coroutines}. \cfuncref{Prolog.Engine}{} for details.
  \keyitem{string}{Type}
  Prolog type to use for JavaScript strings when converting the input.
  Default is \const{string}.  Alternatively one may use \const{atom}.
  \keyitem{nodebug}{Boolean}
  If set to \const{true}, the execution cannot be seen in the debugger.
\end{description}

    \cfunction{Promise}{Prolog.forEach}{Goal, [Input], [OnAnswer], [Options]}
This method executes \arg{Goal} asynchronously. This implies that
\arg{Goal} may execute asynchronous operations and the browser remains
responsive while executing \arg{Goal}.  \arg{Goal} and \arg{Input} are
processed as with \cfuncref{Prolog.query}{}.  If \arg{OnAnswer} is
provided, this \ctype{Function} is called with a \ctype{Object} that
holds the bindings for the output arguments of \arg{Goal} for each
answer.  \arg{Options} supports the following options

\begin{description}
  \keyitem{engine}{Boolean}
  If \const{true}, create an engine that will be destroyed when the
  query completes.
  \keyitem{heartbeat}{Integer}
  Sets the \jargon{heartbeat rate}.  This is the number of Prolog
  inferences executed before yielding.  The default is 10,000.
  Lower values improve interactive behaviour but lower Prolog
  performance.
\end{description}

All default parameters may be omitted.  A JavaScript type check is
used to discover whether the \arg{Input} or \arg{OnAnswer} parameter
is omitted.  Use an empty \arg{Input} to specify \arg{Options} without
inputs, e.g. \verb$forEach(goal, {}, {engine:true})$

The returned \ctype{Promise} is resolved when the query completes. The
value passed to the \exam{.then()} method of the \ctype{Promise} is the
number of answers if \arg{OnAnswer} is provided or an \ctype{Array} of
answers if \arg{OnAnswer} is omitted.  If \arg{Goal} raises an exception
the \ctype{Promise} is rejected.

Multiple calls to Prolog can be activated on the same engine at any
time. Prolog processes such queries in \jargon{LIFO} \jargon{(Last In,
  First Out)} mode.  If queries need to be processed sequentially use
JavaScript \const{await} or the \exam{Promise.finally()} method to
wait for completion.  Multiple \cfuncref{Prolog.forEach}{} queries
that run in separate engines act as \jargon{cooperative threads},
i.e., all make progress.  For example, a goal can be started to run
as cooperative thread using:
\begin{code}
  setTimeout(async () => {
    await Prolog.forEach(goal, input, onanswer,
			 {engine:true});
  });
\end{code}
\end{description}


\subsection{The JavaScript class Query}
\label{sec:wasm-query}

The method \cfuncref{Prolog.query}{} returns an instance of the
JavaScript class \ctype{Query} that may be used to explore the
solutions of the query.  The \ctype{Query} class implements the
JavaScript \jargon{iterator} protocol.


\begin{description}
    \cfunction{Object}{Query.next}{}
Implements the \jargon{iterator} protocol. This returns an object
holding the keys \const{done} and \const{value}.  If exception
handling is enabled it returns an object
\{\const{done}:\const{true}, \const{error}:\const{true},
\const{message}:\arg{String}\}.

    \cfunction{Object}{Query.once}{}
Close the query after the first answer. Returns the \exam{.value} of the
object returned by \exam{.next()} on success and the complete object on
failure or error. In addition, on a logical result (no error), a field
\const{success} is added with a boolean value. Thus, the return value
may contain these keys:

    \begin{description}
    \definition{\{Bindings\}}
Query succeeded.  Objects holds a key for each output variable.  In
addition the \const{success} key is set to \const{true}.

    \definition{\{\const{success}:\arg{false}\}}
Query failed.  Note that the binding keys all start with an uppercase
letter and this is thus not ambiguous.

    \definition{\{\const{error}:true, \const{message}:\arg{String}\}}
Query raised an exception.
    \end{description}

    \cfunction{Object}{Query.close}{}
Closes the query.  This can be used inside the iterator to stop
further enumeration.
\end{description}

\subsection{Using engines}
\label{sec:wasm-engines}

The WASM version of SWI-Prolog supports \jargon{engines}.  The initial
engine is called \const{main}.  Additional engines can be used to
enumerate answers of multiple open queries as well as for implementing
\jargon{coroutines}.  Combined with JavaScript \jargon{async} functions,
engines can provide \jargon{cooperative multi-threading}.  For example,
to enumerate the answers of two queries we may use the following code

\begin{code}
  const e1 = new Prolog.Engine({auto_close:true});
  const e2 = new Prolog.Engine({auto_close:true});

  const q1 = e1.query(Query1);  // see Prolog.query()
  const q2 = e2.query(Query2);

  try
  { for(;;)
    { const n1 = q1.next();
      const n2 = q2.next();
      if ( n1.done && n2.done )
        break;
      // Handle answers
    }
  } finally
  { q1.close(); // also closes e1
    q2.close();
  }
  ...
\end{code}

Engines can also be used to create a cooperative thread.  The example
below creates a Prolog task that prints the numbers 1..20 to the
console, one number every second.

\begin{code}
  ...
  setTimeout(async () => {
    const e = new Prolog.Engine({auto_close:true});
    await e.forEach("between(1,20,X),sleep(1)",
		    (a) => console.out(a.X));
  });
\end{code}

\begin{description}
  \cfunction{Engine}{Prolog.Engine}{[name], [options]}
  Create a new engine.  The \arg{name} argument names the engine.  When
  omitted, engines are named \const{engine}$N$, where $N$ is defined by
  a counter. The \arg{options} argument provides additional configuration.
  Both arguments are optional.
  \begin{description}
    \definition{Boolean \const{auto_close}}
    When \const{true} (default \const{false}, closing the last query
    associated with the engine also closes the engine.
  \end{description}

  \cfunction{undefined}{close}{}
  Terminate the engine.  This may be called safely multiple times on
  the same instance.

  \cfunction{Boolean}{Engine.call}{Goal}
  \nodescription
  \cfunction{Object}{query}{...args}
  \nodescription
  \cfunction{Object}{forEach}{goal, ...args}
  \nodescription
  \cfunction{Any}{with_frame}{function, persist}
  Same as the corresponding methods on class \class{Prolog}, using
  the specified engine for running the Prolog goals.

  \cfunction{Any}{with}{function}
  Run \arg{function} using the specified \class{Engine} instance.
\end{description}


\subsection{Translating data between JavaScript and Prolog}
\label{sec:wasm-data-conversion}

JavaScript and Prolog are both dynamically typed languages. The WASM
module defines a faithful translation between JavaScript data and Prolog
data that aims at completeness as well as keeping the data
representation clean in the common cases.  We describe the translation
in two descriptions because round tripping does not always result in
the original object.

\subsubsection{Translating JavaScript data to Prolog}
\label{sec:wasm-data-js-to-prolog}

This section describes how data from JavaScript is translated into
Prolog.  The interface is primarily designed for passing JavaScript
data as typically used to a natural Prolog representation.  In
addition a number of classes are provided to create Prolog specific
data structures such as strings (as opposed to atoms), variables,
compound terms, etc.

\begin{description}
    \definition{Number}
Translate to a Prolog integer or floating point number.
    \definition{BigInt}
Translate to a Prolog integer.
    \definition{String}
Translate to a Prolog atom. Use	\exam{new Prolog.String(text)}
to create a Prolog string.  See below.
    \definition{Boolean}
Translate to one of the Prolog atoms \const{true} or \const{false}.
    \definition{undefined}
Translate the Prolog atom \const{undefined}.
    \definition{null}
Translate the Prolog atom \const{null}.
    \definition{Array}
Translate to a Prolog list.
    \definition{Objects holding the key \const{\$t}:\arg{Type}}
Such objects are converted depending on the value for this key.
The interface defines classes to simplify creating such objects.
    \begin{description}
       \definition{s}
Represent a Prolog string.  The key \const{v} holds the text.
May be created using \exam{new Prolog.string(text)}.
May be created using \exam{new Prolog.String(text)}.
       \definition{r}
Represent a Prolog \jargon{rational number}.  The keys \const{n}
and \const{d} represent the \jargon{numerator} and \jargon{denominator}.
For example, to represent \exam{1r3}, use \{\const{\$t}:"r",
\const{n}:1, \const{d}:3\}.
May be created using \exam{new Prolog.Rational(n, d)}, where \arg{n}
and \arg{d} can be JavaScript numbers or big integers.
       \definition{t}
Represent a Prolog \jargon{compound term}.  The object should hold
exactly one key whose value is an array that holds the argument values.
For example a term \term{point}{1,2} is constructed using
\{\const{\$t}:"t", \const{point}:[1,2]\}.
May be created using \exam{new Prolog.Compound(functor, args)}
       \definition{v}
Represent a variable.  If the key \const{v} is present this identifies
the variable.  Two variables processed in the same translation with the
same identifier represent the same Prolog variable.  If the \const{v}
key is omitted the variable will be unique.
May be created using \exam{new Prolog.Var(id)}.
       \definition{l}
Represent a Prolog list.  As a JavaScript \ctype{Array} we only need
this typed object to create a \jargon{partial list}.  The \const{v}
key contains the ``normal'' elements and the key \const{tail} contains
the tail of the list.
May be created using \exam{new Prolog.List(array, tail)}.
    \end{description}
    \definition{Object of class \const{Object}}
Plain JavaScript objects are translated into a Prolog \const{dict}.
Note that JavaScript object keys are always strings and (thus) all
dict keys are atoms.  This, \{1:"one"\} is translated into
\verb$_{'1': one}$.
    \definition{ArrayBuffer}
Instances of \ctype{ArrayBuffer} are translated into a Prolog
string that consists of characters in the range $0\ldots{}255$.
    \definition{Objects of a one class not being \const{Object}}
Instances of non-plain JavaScript objects are translated into a
Prolog \jargon{blob}. Such objects are written as \verb$<js_Class(id)>$.
The Prolog interface allows for passing the objects back and calling
methods on them.  See \secref{wasm-js-call}.
\end{description}


\subsubsection{Translating Prolog data to JavaScript}
\label{sec:wasm-data-prolog-to-js}

Most of the translation from Prolog data to JavaScript is the reverse of
the translation described in \secref{wasm-data-js-to-prolog}. In some
cases however reverse translation is ambiguous. For example, both
\exam{42} and \exam{42n} (a JavaScript \ctype{BigInt}) translate to a
simple Prolog integer. The other way around, as JavaScript
\ctype{Number} is a float, both Prolog \exam{42} and \exam{42.0}
translate to \exam{42} in JavaScript.

\begin{description}
    \definition{Variable}
Translate to a JavaScript \ctype{Prolog.Variable} instance
where the identifier is a unique number of each unique variable.
    \definition{Integer}
Translate to a JavaScript \ctype{Number} when possible or
\ctype{BigInt} otherwise.  Currently JavaScript \ctype{Number}
can represent integers up to $2^{53}$ precisely.
    \definition{Rational}
Translate to a JavaScript \ctype{Prolog.Rational} instance.
    \definition{Float}
Translate to a JavaScript \ctype{Number}.
    \definition{Atom}
Translate to a JavaScript \ctype{String}.
    \definition{String}
Translate to a JavaScript \ctype{Prolog.String} instance.
    \definition{List}
When a \jargon{proper list} create a JavaScript \ctype{Array},
otherwise create a JavaScript \ctype{Prolog.List} instance.
    \definition{Compound term}
Create a JavaScript \ctype{Prolog.Compound} instance.
    \definition{Dict}
Create a plain JavaScript \ctype{Object} with the same keys.  If the
dict has a non-var \jargon{tag}, add a \const{\$tag} property.
\end{description}

\section{Accessing JavaScript from Prolog}
\label{sec:wasm-js-call}

This section describes how we can interact with JavaScript from Prolog.
The interface is captured in a single predicate \predref{:=/2}.

\begin{description}
    \infixop{:=}{Left}{Right}
Depending on \arg{Left}, this predicate implements two different
actions. If \arg{Left} is a Prolog variable, it evaluates the expression
\arg{Right} in JavaScript and unifies the result to \arg{Left}. If
\arg{Left} is a term \arg{Obj}[\arg{Key}], where \arg{Key} is an atom,
it accesses a JavaScript \jargon{setter}. The general form of an
expression is \arg{Expression}[\arg{Callable}] or simply \arg{Callable}.
If \arg{Callable} is compound it expresses a function (or method) call.
Otherwise we call JavaScript \cfuncref{eval}{}, except for these
special values:

\begin{description}
    \termitem{window}{}
The main browser window itself (\const{undefined} when not in a browser).
    \termitem{prolog}{}
The \ctype{Prolog} instance.
\end{description}

Prolog values are translated according to the rules in
\secref{wasm-data-prolog-to-js} and the result is translated back to
Prolog according to the rules in \secref{wasm-data-js-to-prolog}.
Because callables are translated to function calls, object properties or
global variables we need an escape to pass them as data. This is
achieved using the prefix operator \const{#}. Note that lists are passed
as JavaScript arrays rather than calls to the list functor. For
convenience Prolog strings are by default translated to JavaScript
\ctype{String} objects rather than \ctype{Prolog.String} instances.
Below are some examples:

\begin{code}
?- Res := myfunc([1,2,3]).
?- Max := 'Math'.max(10, 20).
?- Out := document.getElementById('output').
?- Par := document.createElement(p),
   Par.textContent := #Text.
?- Par.textContent := "aap" + " " + "noot".
\end{code}

Some JavaScript expressions are not implemented as functions.  The
following ``functions'' are handled directly by the implementation.

    \begin{description}
    \termitem{instanceof}{}
    Returns the name of the class to which the object belongs.
    Same as \exam{Obj.constructor.name}.
    \termitem{instanceof}{ClassName}
    Returns a \ctype{Boolean} indicating whether the object is an
    instance of \arg{ClassName}.  Note that the class name must be
    an atom and as JavaScript class names normally start with a
    capital, the names typically need to be quoted using \emph{single}
    quotes.  For example:
\begin{code}
?- W := window, T := W.instanceof('Window').
W = <js_Window>(1),
T = true.
\end{code}
    \termitem{-}{Any}
Numerical negation
    \termitem{!}{Any}
Logical negation.
    \termitem{+}{Any, Any}
    \nodescription
    \termitem{-}{Any, Any}
    \nodescription
    \termitem{*}{Any, Any}
    \nodescription
    \termitem{/}{Any, Any}
    \nodescription
    \termitem{&}{Any, Any}
    \nodescription
    \termitem{|}{Any, Any}
    \termitem{&&}{Any, Any}
    \nodescription
    \termitem{||}{Any, Any}
Binary operators. Note that some are not defined as Prolog operators and
thus one must write e.g. \verb$A := &&(true,false)$.  \verb$||$ is not
a Prolog atom, so logical disjunction gets \verb$A := '||'(false,false)$.
    \end{description}

    \predicate[semidet]{is_object}{1}{@Term}
True if \arg{Term} is a reference to a JavaScript object.
    \predicate[semidet]{is_object}{2}{@Term, ?Class}
True when \arg{Term} is an instance of \arg{Class}.  If \arg{Class}
is unbound it is unified with the name of the \jargon{constructor},
otherwise a JavaScript \exam{Term instanceof Class} is executed.

    \predicate{js_script}{2}{+String, +Options}
Evaluate \arg{String} as JavaScript.  This is designed to cooperate
with string \jargon{quasi quotations}, so we can write e.g.,

\begin{code}
:- use_module(library(strings)).
:- js_script({|string||
function myfunc(a)
...
|}).
\end{code}

The implementation uses \predref{=:}{2}, calling the JavaScript
function eval().

    \predicate{fetch}{3}{+URL, +Type, -Data}
Wrapper around JavaScript fetch(), conversion of the \ctype{Response}
object and waiting for the \ctype{Promise}. Type is an atom or string
that is used as method on the \ctype{Response} object. Examples are
\const{text}, \const{json}, \const{html} or \const{blob}. The
\const{blob} type returns the \arg{Data} as a string of \jargon{bytes},
i.e., character codes in the range $0\ldots{}255$.
\end{description}

\subsubsection{Asynchronous access to JavaScript from Prolog}
\label{sec:wasm-js-async-call}

While \secref{wasm-js-call} describes synchronous calls from Prolog to
JavaScript, we also need asynchronous calling to implement sleep/1, wait
for user input, downloading documents from the web, etc. Asynchronous
calling is achieved by \jargon{yielding} from the Prolog virtual
machine. This can only be done when Prolog is told to expect that the VM
may yield. This is implemented by \exam{Prolog.forEach()} as described
in \secref{wasm-calling}.

\begin{description}
    \predicate[det]{await}{2}{+Promise, -Result}
Yield the Prolog VM, returning control back to JavaScript. When this is
called from Prolog invoked using \exam{Prolog.forEach()}, execution of
await/2 completes when the \arg{Promise} resolves and \arg{Result} is
unified with the value passed to the \exam{Promise.then()} method. As an
exception to the normal conversion rules, if the result is a single
\ctype{String}, it is returned as a Prolog string rather than an atom.
When the \arg{Promise} is rejected await/2 throws an exception. Note
that await/2 allows, for example, downloading a URL from Prolog:

\begin{code}
?- FP := fetch("test.pl"), await(FP, Response),
   TP := Response.text(), await(TP, T).
FP = <js_Promise>(4),
Response = <js_Response>(5),
TP = <js_Promise>(6),
T = "% :- debug(js) ...".
\end{code}

Calls to await/2 may be asynchronously aborted by calling
\exam{Prolog.abort()} if \arg{Promise} implements \exam{.abort()}.
See \secref{wasm-promise-abort} for implementing such a promise.

    \predicate[semidet]{is_async}{0}{}
True when we can call await/2 in the current state.  This implies Prolog has
been called from JavaScript code that is prepared to deal with Prolog
yielding and Prolog is not inside a callback from C (WASM).
\end{description}


\subsubsection{JavaScript Promise that can be aborted}
\label{sec:wasm-promise-abort}

A \ctype{Promise} resolves or is rejected. As Prolog waits for a
specific promise on a call to await/2 we may want to abort long
running operations. This may be achieved using the class
\ctype{Prolog.Promise} which extends \ctype{Promise}. To make the
promise abortable the \jargon{executor} function must have an
\const{abort} property. Below is the code for
\exam{Prolog.promise_sleep()} that implements this schema.  First we
create the \jargon{executor} and use properties on the function itself
to represent the necessary state information (here, the running timer).
Next, we add an \const{abort} property the clears the timer and runs
the \const{reject} callback of the \ctype{Promise}.  Finally we return
an instance of \ctype{Prolog.Promise} which implements \exam{.abort()}.

\begin{code}
promise_sleep(time)
{ const f = function(resolve, reject)
  { f.reject = reject;
    f.timer = setTimeout(() =>
      { f.timer = undefined;
	resolve(true);
      }, time*1000);
  };

  f.abort = function()
  { if ( f.timer )
    { clearTimeout(f.timer);
      f.timer = undefined;
      f.reject("abort");
    }
  }

  return new Prolog.Promise(f);
}
\end{code}

\InputIfFileExists{lib/wasmlib}{}{}
\InputIfFileExists{lib/dom}{}{}
\InputIfFileExists{lib/taudom}{}{}
