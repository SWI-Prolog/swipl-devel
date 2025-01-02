\section{Single Sided Unification rules}
\label{sec:ssu}

\index{single sided unification}%
\index{SSU}%
\index{term subsumption}%
For the execution of a normal Prolog clause, the goal term is unified
with the head of the clause. This allows us to write facts such as below
and use this relation in all four possible \jargon{modes}.  This is the basis
of SLD resolution that turns Prolog into a logic programming language.

\begin{code}
parent('Bob', 'Susan').
\end{code}

In practice though, Prolog is both a logic programming language and a
language for expressing computations in a near \jargon{procedural}
style. The first is used to solve (notably) combinatorial problems while
the latter is used for I/O, data transformation and the many non-logical
operations that are involved in many applications.

Many Prolog programmers experience writing procedural style Prolog as
fighting non-determinism and dealing with hard to debug silent failures
because no clause matches some goal. Below are two typical queries on
library predicates that have a procedural nature, i.e., are
\jargon{single moded}.

\begin{code}
?- sum_list(a, X).
false.

?- sum_list([1|T], X).
T = [],
X = 1 ;
ERROR: Arguments are not sufficiently instantiated
\end{code}

The definition of sum_list/2 is it appears in library(lists) is below.
This implementation can be considered elegant. Note that sum_list/2 has
only one meaningful mode: (+,-). A general (logical) implementation
would allow for a partial list or a list holding one or more variables,
With a proper list that holds a single variable we can still make a
sound logical implementation. In all other cases the number of solutions
is infinite and even \jargon{uncountable} for a partial list, making the
predicate useless as a \jargon{generator} of solutions.


\begin{code}
sum_list(Xs, Sum) :-
    sum_list(Xs, 0, Sum).

sum_list([], Sum, Sum).
sum_list([X|Xs], Sum0, Sum) :-
    Sum1 is Sum0 + X,
    sum_list(Xs, Sum1, Sum).
\end{code}

If we want to avoid the above dubious behaviour we have two options.
First, we can verify that the first argument is a list before entering
the recursion, changing the first clause as below.  The disadvantage
is that we process the list twice.


\begin{code}
sum_list(Xs, Sum) :-
    must_be(list, Xs),
    sum_list(Xs, 0, Sum).
\end{code}

Alternatively, we can rewrite the second clause to verify the list on
the fly. That leads to the code below. Most likely the overhead of this
alternative compared to the above is even worse in many Prolog
implementations. Most people would also consider this code rather
inelegant.


\begin{code}
sum_list(Var, _, _) :-
    var(Var),
    instantiation_error(Var).
sum_list([], Sum, Sum) :-
    !.
sum_list([X|Xs], Sum0, Sum) :-
    !,
    Sum1 is Sum0 + X,
    sum_list(Xs, Sum1, Sum).
sum_list(NoList, _, _) :-
    type_error(list, NoList).
\end{code}

Another example is a relation \nopredref{max}{3}, expressing the maximum of two
numbers. A classical textbook definition could be as below. This code
has two drawbacks. First it leaves an open choice points in most Prolog
implementations if \arg{X} is the largest and second it compares the two
numbers twice. Some Prolog systems detect this particular case, but in
general it needs two know that one test is the strict negation of the
other.

\begin{code}
max(X,Y,X) :- X >= Y.
max(X,Y,Y) :- Y > X.
\end{code}

As a result people use a cut and might come up with the \textbf{wrong}
solution below. Consider the query \exam{?- max(5,2,2).} to see why this
code is broken.

\begin{code}
max(X,Y,X) :- X >= Y, !.
max(_,Y,Y).
\end{code}

A correct solution is below, \jargon{delaying} binding the output until after
the cut.

\begin{code}
max(X,Y,M) :- X >= Y, !, M = X.
max(_,Y,Y).
\end{code}

Some people may prefer using if-then-else as below.  This is arguable
the cleanest efficient solution in standard Prolog.

\begin{code}
max(X,Y,M) :- ( X >= Y -> M = X ; M = Y ).
\end{code}

As we have seen from these examples, writing procedural code in Prolog
requires us to follow the two basic principles below. Both principles
have been properly described in \textit{The Craft of Prolog}
\cite{Keefe:90}.

\begin{itemize}
\item
    Structure every clause as \exam{Head :- Guard, !, Body}. Every
    clause has the cut as early as possible. \arg{Guard} can be empty.
    The last clause often does not need a cut.

\item
    Avoid that the head unification binds values in the goal term.  We
    see this may lead to undesirable results such as sum_list(L,S) binding
    \arg{L} to `[]` and \arg{S} to `0` as well as loss of \jargon{steadfastness}, causing
    max(5,2,2) to succeed.  The first requires additional var/1 or nonvar/1
    tests. The second requires delaying unification until after the cut.
\end{itemize}


\href{http://picat-lang.org/}{Picat} provides the \funcref{=>}{2}
alternative for the Prolog \jargon{neck} (\funcref{:-}{2}) to force the
above practices. A Picat rule has the following shape:

\begin{code}
Head, Guard => Body.
\end{code}

This is semantically equivalent to the Prolog clause below. The
subsumes_term/2 guarantees the clause head is more \jargon{generic} than the
goal term and thus unifying the two does not affect any of the arguments
of the goal. This implies all output unification _must_ be done after
the head unification.

\begin{code}
p(V1,V2,...,Vn) :-
    Pattern = p(A1,A2,...,An),
    Args = p(V1,V2,...,Vn),
    subsumes_term(Pattern, Args),
    Pattern = Args,
    Guard,
    !,
    Body.
\end{code}

SWI-Prolog as of version 8.3.19 support \funcref{=>}{2} as an alternative to
normal Prolog clauses. The construct comes with the following
properties.

\begin{itemize}
\item
   A predicate either uses \funcref{:-}{2} for all its clauses or
   \funcref{=>}{2}. Mixing is \textbf{not} allowed and raises a
   permission error for a clause that does not use the same
   \jargon{neck} as the first clause.
\item
   Unlike Picat, it is an error if no clause matches.
\end{itemize}

Given \funcref{=>}{2} rules, we can rewrite sum_list/2 as below. The
first clause can be written using \funcref{:-}{2} or \funcref{=>}{2}. As
the head is the most general head and there is only one clause these are
fully equivalent. The \nopredref{sum_list}{3} helper needs a small
modification: we need to delay the unification against \arg{Sum} to the
body. The last clause is equivalent.


\begin{code}
sum_list(Xs, Sum) =>
    sum_list(Xs, 0, Sum).

sum_list([], Sum0, Sum) =>
    Sum = Sum0.
sum_list([X|Xs], Sum0, Sum) =>
    Sum1 is Sum0 + X,
    sum_list(Xs, Sum1, Sum).
\end{code}

Given this definition, \exam{sum_list(L,S)} no longer matches a rule
and neither does e.g., \exam{sum_list(a,S)}. Both raise an error.
Currently the error is defined as below.

\begin{code}
existence_error(matching_rule, Head)
\end{code}

Should silent failure be desired if no rule matches, this is easily
encoding by adding a rule at the end using the most general head and
fail/0 as body:

\begin{code}
sum_list(_,_,_) => fail.
\end{code}

\subsection{Single Sided Unification Guards}
\label{sec:ssu-guard}

Using the construction \exam{Head, Guard => Body}, the \arg{Guard} is
executed \emph{after} the single sided head unification. If the
\arg{Guard} succeeds the clause executes a cut (\predref{!}{0}) and
proceeds normally. There are no restrictions on the guard code. A well
behaved guard is a \jargon{test}.  Notably:

\begin{itemize}
    \item Though not enforced\footnote{We do not know about an efficient
way to enforce unification against head arguments}, guard code shall
not instantiate variables in the \arg{Head} because this breaks the
promise of SSU and may make the node non-steadfast.
    \item It is bad style (but again, not enforced) to have any type
of side effects (output, database change, etc.)
    \item Typically, guard calls are \const{semidet}.  Non-deterministic
calls are allowed.  If the guard succeeds with choicepoints these are
pruned before the body is entered.
\end{itemize}

As a special exception, explicit unification against a variable in the
head is moved into the head. See \secref{indexbody}. In the example
below, the \exam{X = f(I)} is moved into the head and (thus) is executed
using single sided unification.

\begin{code}
p(X), X = f(I), integer(I) => q(X).
\end{code}

\begin{quote}
\textbf{Warning} Moving the guard unification into the head changes the
semantics of the unification. This may be defended by the rules above
that claim one should not unify against the head arguments in the guard.
Future versions may use a dedicated operator to indicate that the
unification may be moved into the head.
\end{quote}


\subsection{Consequences of {=>} single sided unification rules}
\label{sec:ssu-consequences}

The \funcref{=>}{2} construct is handled by the low-level compiler if no
\jargon{guard} is present. If a guard is present it is currently compiled into
the construct below. The Picat \funcref{?=>}{2} \jargon{neck} operator is like
\funcref{=>}{2}, but does not \jargon{commit} to this rule. We are not
yet sure whether or not SWI-Prolog will remain supporting
\funcref{?=>}{2}.\footnote{\funcref{?=>}{2} is currently implemented but
not defined as an operator.}

\begin{code}
Head ?=> Guard, !, Body.
\end{code}

The main consequence is that clause/2 cannot distinguish between a
normal clause and a \funcref{=>}{2} clause. In the current implementation it
operates on both without distinguishing the two. This implies e.g.,
\jargon{cross referencing} still works. Meta interpretation however
does not work. In future versions clause/2 may fail on these rules. As
an alternative we provide rule/2,3.

\begin{description}
    \predicate{rule}{2}{:Head, -Rule}
\nodescription
    \predicate{rule}{3}{:Head, -Rule, -Ref}
True when \arg{Rule} is a rule/clause that implements \arg{Head}.
\arg{Rule} is a complete rule term.  For a normal clause this is
a term \exam{Head :- Body} and for a single sided unification rule
it is a term \exam{Head {=>} Body}.
\end{description}

\subsection{Single sided unification for Definite Clause Grammars}
\label{sec:ssu-dcg}

Single sided unification is attractive for \jargon{generative DCG
rules}, i.e., DCG rules that are used to \jargon{serialize} some term.
In that context they avoid unwanted matching on variables and provide
better error messages in case not all possible terms are described by
the grammar. Single sided unification has no practical use for parsing
because the arguments are typically \jargon{output} arguments.

If the head of an SSU DCG rules is a term \verb$Head, Extra$,
\arg{Extra} is interpreted as a \jargon{push back list} if it is a list
and as an SSU \jargon{guard} otherwise.  The guard is \emph{not} subject
to DCG expansion, i.e., it is interpreted as if enclosed by \verb${}$.

\subsection{SSU: Future considerations}
\label{sec:ssu-future}

The current implementation is a rather simple. Single sided unification
is achieved doing normal head unification and backtrack if this
unification bound variables in the goal term. Future versions are likely
to backtrack as soon as we find a variable in the goal that needs to be
unified.

It is likely that in due time significant parts of the libraries will be
migrated to use SSU rules, turning many silent failures on type errors
into errors.
