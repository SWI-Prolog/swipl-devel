\chapter{Tabled execution (SLG resolution)}
\label{sec:tabling}

\index{SLG,resolution}%
This chapter describes SWI-Prolog's support for \jargon{Tabled
execution} for one or more Prolog predicates, also called \jargon{SLG
resolution}. Tabling a predicate provides two properties:

\begin{enumerate}
    \item
	Re-evaluation of a tabled predicate is avoided by
	\jargon{memoizing} the answers. This can realise huge
	performance enhancements as illustrated in
	\secref{tabling-memoize}. It also comes with two downsides: the
	memoized answers are not automatically updated or invalidated if
	the world (set of predicates on which the answers depend)
	changes and the answer tables must be stored (in memory).

  \item
	\jargon{Left recursion}, a goal calling a \jargon{variant} of
	itself recursively and thus \textit{looping} under the normal
	Prolog SLD resolution is avoided by suspending the variant call
	and resuming it with answers from the table.  This is illustrated
	in \secref{tabling-non-termination}.
\end{enumerate}

Tabling is particularly suited to simplify inference over a highly
entangled set of predicates that express axioms and rules in a static
(not changing) world. When using SLD resolution for such problems, it is
hard to ensure termination and avoid frequent recomputation of
intermediate results. A solution is to use Datalog style bottom-up
evaluation, i.e., applying rules on the axioms and derived facts until a
fixed point is reached. However, bottom-up evaluation typically derives
many facts that are never used. Tabling provides a \jargon{goal
oriented} resolution strategy for such problems and is enabled simply by
adding a table/1 directive to the program.


\section{Example 1: using tabling for memoizing}
\label{sec:tabling-memoize}

As a first classical example we use tabling for \emph{memoizing}
intermediate results. We use Fibonacci numbers to illustrate the
approach.  The Fibonacci number $I$ is defined as the sum of the
Fibonacci numbers for $I-1$ and $I-2$, while the Fibonacci number
of 0 and 1 are both defined to be 1.  This can be translated naturally
into Prolog:

\begin{code}
fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
	N1 is N-1,
	N2 is N-2,
	fib(N1, F1),
	fib(N2, F2),
	F is F1+F2.
\end{code}

The complexity of executing this using SLD resolution however is $2^N$
and thus becomes prohibitively slow rather quickly, e.g., the execution
time for $N=30$ is already 0.4 seconds. Using tabling, \term{fib}{N,F}
for each value of $N$ is computed only once and the algorithm becomes
linear. Tabling effectively inverts the execution order for this case:
it suspends the final addition (F is F1+F2) until the two preceding
Fibonacci numbers have been added to the answer tables. Thus, we can
reduce the complexity from the show-stopping $2^N$ to linear by adding a
tabling directive and otherwise not changing the algorithm. The code
becomes:

\begin{code}
:- table fib/2.

fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
	N1 is N-1,
	N2 is N-2,
	fib(N1, F1),
	fib(N2, F2),
	F is F1+F2.
\end{code}

The price that we pay is that a table \term{fib}{I,F} is created for
each $I$ in $0..N$. The execution time for $N=30$ is now 1 millisecond
and computing the Fibonacci number for $N=1000$ is doable (output edited
for readability).

\begin{code}
1 ?- time(fib(1000, X)).
% 52,991 inferences, 0.013 CPU in 0.013 seconds
X = 70330367711422815821835254877183549770181269836358
    73274260490508715453711819693357974224949456261173
    34877504492417659910881863632654502236471060120533
    74121273867339111198139373125598767690091902245245
    323403501.
\end{code}

In the case of Fibonacci numbers we can still rather easily achieve
linear complexity using program transformation, where we use bottom-up
instead of top-down evaluation, i.e., we compute \term{fib}{N,F} for
growing $N$, where we pass the last two Fibonacci numbers to the next
iteration. Not having to create the tables and not having to suspend and
resume goals makes this implementation about 25 times faster than the
tabled one. However, even in this simple case the transformation is not
obvious and it is far more difficult to recognise the algorithm as an
implementation of Fibonacci numbers.

\begin{code}
fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, F) :-
	fib(1,1,1,N,F).

fib(_F, F1, N, N, F1) :- !.
fib(F0, F1, I, N, F) :-
	F2 is F0+F1,
	I2 is I + 1,
	fib(F1, F2, I2, N, F).
\end{code}


\section{Example 2: avoiding non-termination}
\label{sec:tabling-non-termination}

SLD resolution easily results in an infinite loop due to \jargon{left
recursion}, a goal that (indirectly) calls a variant of itself or cycles
in the input data. Thus, if we have a series of
\nopredref{connection}{2} statements that define railway connections
between two cities, we cannot use the most natural logical definition to
express that we can travel between two cities:

\begin{code}
% :- table connection/2.

connection(X, Y) :-
	connection(X, Z),
	connection(Z, Y).
connection(X, Y) :-
	connection(Y, X).

connection('Amsterdam', 'Schiphol').
connection('Amsterdam', 'Haarlem').
connection('Schiphol', 'Leiden').
connection('Haarlem', 'Leiden').
\end{code}

After enabling tabling however, the above works just fine as illustrated
in the session below.  Where is the magic and what is the price we
paid? The magic is, again, the fact that new goals to the tabled
predicate suspend. So, all recursive goals are suspended. Eventually, a
table for \term{connection}{'Amsterdam', X} is created with the two
direct connections from Amsterdam. Now, it resumes the first clause
using the tabled solutions, continuing the last
\nopredref{connection}{2} subgoal with \term{connection}{'Schiphol', X}
and \term{connection}{'Haarlem', X}. These two go through the same
process, creating new suspended recursive calls and creating tables for
the connections from Schiphol and Haarlem. Eventually, we end up with a
set of tables for each call variant that is involved in computing the
transitive closure of the network starting in Amsterdam. However, if the
Japanese rail network would have been in our data as well, we would not
have produced tables for that.

\begin{code}
1 ?- connection('Amsterdam', X).
X = 'Haarlem' ;
X = 'Schiphol' ;
X = 'Amsterdam' ;
X = 'Leiden'.
\end{code}

Again, the fact that a simple table/1 directive turns the pure logical
specification into a fairly efficient algorithm is a clear advantage.
Without tabling the program needs to be \jargon{stratified}, introducing
a base layer with the raw connections, a second layer that introduces
the \jargon{commutative} property of a railway (if you can travel from
$A$ to $B$ you can also travel from $B$ to $A$ and a final layer that
realises \jargon{transitivity} (if you can travel from $A$ to $B$ and
from $B$ to $C$ you can also travel from $A$ to $C$). The third and
final layer must keep track which cities you have already visited to
avoid traveling in circles. The transformed program however uses little
memory (the list of already visited cities and the still open choices)
and does not need to deal with maintaining consistency between the
tables and ground facts.

\section{Answer subsumption or mode directed tabling}
\label{sec:tabling-mode-directed}

\index{answer subsumption,tabling}%
Tabling as defined above has a serious limitation. Although the
definition of \nopredref{connection}{2} from section
\secref{tabling-non-termination} can compute the transitive closure of
connected cities, it cannot provide you with a route to travel. The
reason is that there are infinitely many routes if there are cycles in
the network and each new route found will be added to the answer table
and cause the tabled execution's completion algorithm to search for more
routes, eventually running out of memory.

The solution to this problem is called \jargon{mode directed tabling} or
\jargon{answer subsumption}.\footnote{The term \jargon{answer
subsumption} is used by XSB and \jargon{mode directed tabling} by YAP
and B-Prolog. The idea is that some arguments are considered `outputs',
where multiple values for the same `input' are combined. Possibly
\jargon{answer aggregation} would have been a better name.} In this
execution model one or more arguments are \emph{not} added to the table.
Instead, we remember a single \jargon{aggregated} value for these
arguments. The example below is derived from
\secref{tabling-non-termination} and returns the connection as a list of
cities. This argument is defined as a \jargon{moded} argument using the
\term{lattice}{PI} mode.\footnote{This mode is compatible to XSB
Prolog.} This causes the tabling engine each time that it finds an new
path to call \nopredref{shortest}{3} and keep the shortest route.

\begin{code}
:- table
    connection(_,_,lattice(shortest/3)).

shortest(P1, P2, P):-
    length(P1, L1),
    length(P2, L2),
    (   L1 < L2
    ->  P = P1
    ;   P = P2
    ).

connection(X, Y, [X,Y]) :-
    connection(X, Y).
connection(X, Y, P) :-
    connection(X, Z, P0),
    connection(Z, Y),
    append(P0, [Y], P).
\end{code}

The mode declaration scheme is equivalent to XSB with partial
compatibility support for YAP and B-Prolog. The \term{lattice}{PI} mode
is the most general mode.  The YAP \const{all} (B-Prolog \const{@}) mode
is not yet supported. The list below describes the supported modes and
indicates the portability.

\begin{description}
    \termitem{Var}{}
\nodescription
    \termitem{+}{}
\nodescription
    \termitem{index}{}
A variable (XSB), the atom \const{index} (YAP) or a \const{+}
(B-Prolog, YAP) declare that the argument is tabled normally.

    \termitem{lattice}{Pred}
\arg{Pred} denotes a predicate with arity~3.  It may be specified as
an predicate indicator (\arg{Name}/3), plain predicate name (\arg{Name})
or a head term \term{Name}{_,_,_}. On each answer, \arg{PI} is called
with three arguments: the current aggregated answer and the new answer
are inputs. The last argument must be unified with a term that
represents the new aggregated answer.

    \termitem{po}{PI}
\jargon{Partial Ordering}. The new answer is added iff
\term{call}{PI, +Old, +Answer} succeeds. For example, \verb$po('<'/2)$
accumulates the smallest result. In SWI-Prolog the arity (2) may be
omitted, resulting in \verb$po(<)$.

    \termitem{-}{}
\nodescription
    \termitem{first}{}
The atom \const{-} (B-Prolog, YAP) and \const{first} (YAP) declare to
keep the first answer for this argument.

    \termitem{last}{}
The atom \const{last} (YAP) declares to keep the last answer.

    \termitem{min}{}
The atom \const{min} (YAP) declares to keep the smallest answer
according to the standard order of terms (see \predref{@<}{2}). Note
that in SWI-Prolog the standard order of terms orders numbers by value.

    \termitem{max}{}
The atom \const{max} (YAP) declares to keep the largest answer
according to the standard order of terms (see \predref{@>}{2}). Note
that in SWI-Prolog the standard order of terms orders numbers by value.

    \termitem{sum}{}
The atom \const{sum} (YAP) declares to sum numeric answers.
\end{description}


\section{Tabling for impure programs}
\label{sec:tnotpure}

Tabling guarantees logically correct results and termination provided
the computation only involves terms of bounded size on \emph{pure}
Prolog programs, i.e., Prolog programs without side effects or pruning
of choice points (cut, \predref{->}{2}, etc.). Notably pruning choice
points of an incomplete tabled goal may cause an incomplete table and
thus cause subsequent queries for the same goal to return an incomplete
set of answers. The current SWI-Prolog implementation provides several
mechanisms to improve on this situation.

\begin{itemlist}
    \item [Dynamic Strongly Connected Components (SCC)]
Tabled goals are \jargon{completed} as soon as possible. Each fresh
tabled goal creates a scheduling component which the system attempts to
solve immediately. If a subgoal of the fresh goal refers to an
incomplete tabled goal the scheduling components for both goals are
merged such that the related goals are completed together. Dynamic
rather than static determination of strongly connected components
guarantees that the components are minimal because only actually reached
code needs to be considered rather than maximally reachable code.

Minimal SCCs imply that goals are completed as early as possible. This
implies that tabled goals may be embedded in e.g., findall/3 or be used
as a condition as long as there is no dependency (\jargon{loop}) with
goals outside the findall/3 or condition. For example, the code below
misbehaves when called as \term{p}{X} because the argument of findall/3
calls a \jargon{variant} of the goal that initiated the findall goal. A
call \term{p}{1} however is ok as \term{p}{1} is not a variant of
\term{p}{X}.

\begin{code}
p(X) :-
    findall(Y, p(Y), Ys),
    ...
\end{code}

    \item [Early completion]
Ground goals, i.e., goals without variables, are subject to early
completion. This implies they are considered completed after the first
solution.
\end{itemlist}


\section{Variant and subsumptive tabling}
\label{sec:tabling-subsumptive}

By default, SWI-Prolog (and other Prolog systems with tabling) create a
table per call \jargon{variant}. A call (term) is a variant of another
call (term) if there is a renaming of variables that makes the two terms
equal. See \predref{=@=}{2} for details. Consider the following program:

\begin{code}
:- table p/1.

p(X) :- p(Y), Y < 10 000, X is Y+1.
p(1).
\end{code}

Calling \term{p}{X} creates a table for this variant with 10,000
answers. Calling \term{p}{42} creates a new table where the recursive
call (\term{p}{Y}) uses the previously created table to enumerate all
values $1\ldots{}41$ before deriving \term{p}{42} is true. \jargon{Early
completion} (see \secref{tnotpure}) in this case prevents enumerating
all answers for \term{p}{Y} ($1\ldots{}10,000$). As a result, the query
below runs in quadratic time and creates a 10,000 additional tables.

\begin{code}
?- time(forall(between(1, 10 000, X), p(X))).
% 150,370,553 inferences, 13.256 CPU in 13.256 seconds
\end{code}

\jargon{Subsumptive} tabling answers a query using answers from a more
general table. In this case, this means it uses basically trie_gen/2 to
get the answer \term{p}{42} from the table \term{p}{_}.  This leads to
the program and results shown below.

\begin{code}
:- table p/1 as subsumptive.

p(X) :- p(Y), Y < 10 000, X is Y+1.
p(1).
\end{code}

\begin{code}
?- time(p(_)).
% 140,066 inferences, 0.015 CPU in 0.015 seconds
?- time(t).
% 170,005 inferences, 0.016 CPU in 0.016 seconds
\end{code}

\jargon{Subsumptive} tabling can be activated in two ways. Per table
assign the \exam{... as subsumptive} option and globally by setting the
\prologflag{table_subsumptive} flag to \const{true}.

One may wonder why subsumptive tabling is not the default. There are
also some drawbacks:

\begin{itemize}
    \item Subsumptive tabling only provides correct support if
instances (more specific) queries indeed provides answers that
are consistent with the more general query.  This is true for
\jargon{pure programs}, but not guaranteed for arbitrary Prolog
programs.
    \item Finding more generic tables is more complicated and
expensive than finding the call variant table and extracting the subset
of answers that match the more specific query can be expensive.
    \item Using subsumptive tables can create more dependencies
in the call graph which can slow down the table completion
process.  Larger dependent components also negatively impact the
issues discussed in \secref{tnotpure}.
\end{itemize}


\section{Well Founded Semantics}
\label{sec:WFS}

According to
\href{https://en.wikipedia.org/wiki/Well-founded_semantics}{Wikipedia},
"\jargon{Well Founded Semantics} is one definition of how we can make
conclusions from a set of logical rules". Well Founded Semantics (WFS)
defines a \jargon{three valued logic} representing \jargon{true},
\jargon{false} and something that is neither true or false. This latter
value is often referred to as \jargon{bottom}, \jargon{undefined} or
\jargon{unknown}.  SWI-Prolog uses undefined/0.

Well Founded Semantics allows for reasoning about programs with
contradictions or multiple answer sets. It allows for obtaining
true/false results for literals that do not depend on the sub program
that has no unambiguous solution, propagating the notion of
\jargon{undefined} to literals that cannot be resolved otherwise and
obtaining the \jargon{residual} program that expresses why an answer is
not unambiguous.

The notion of \jargon{Well Founded Semantics} is only relevant if the
program uses \jargon{negation} as implemented by tnot/1. The argument of
tnot/1, as the name implies, must be a goal associated with a tabled
predicate (see table/1).  In a nutshell, resolving a goal that implies
tnot/1 is implemented as follows:

Consider the following partial \jargon{body term}:

\begin{code}
	...,
	tnot(p),
        q.
\end{code}


\begin{enumerate}
    \item
If $p$ has an unconditional answer in its table, fail.
    \item
Else, \jargon{delay} the negation.  If an unconditional
answer arrives at some time, resume with failure.
    \item
If at the end of the traditional tabled evaluation we
can still not decide on $p$, execute the \jargon{continuation}
($q$ above) while maintaining the \jargon{delay list} set to
\term{tnot}{p}.  If executing the continuation results in an
answer for some tabled predicate, record this answer as a
\jargon{conditional} answer, in this case with the condition
\term{tnot}{p}.
    \item
If a conditional answer is added to a table, it is propagated to its
\jargon{followers}, say $f$, adding the pair \{$f$,answer\} to the
delay list. If this leads to an answer, the answer is conditional
on this pair.
    \item
After the continuations of all unresolved tnot/1 calls have
been executed the various tables may have conditional answers
in addition to normal answers.
    \item
If there are negative literals that have neither conditional answers
nor unconditional answers, the condition \term{tnot}{g} is true.
This conclusion is propagated by simplifying the conditions for
all answers that depend on \term{tnot}{g}.  This may result in
a definite \jargon{false} condition, in which case the answer
is removed or a definite \jargon{true} condition in which case
the answer is made unconditional.  Both events can make other
conditional answers definitely true or false, etc.
    \item
At the end of the simplifying process some answers may still be
conditional.  A final \jargon{answer completion} step analyses
the graph of depending nodes, eliminating \jargon{positive loops},
e.g., ``$p$ :- $q$.  $q$ :- $p$''.  The answers in such a loop are
removed, possibly leading to more simplification.  This process is
executed until \jargon{fixed point} is reached, i.e., no further
positive loops exist and no further simplification is possible.
\end{enumerate}

The above process may complete without any remaining conditional
answers, in which case we are back in the normal Prolog world. It is
also possible that some answers remain conditional. The most obvious
case is represented by undefined/0. The toplevel responds with
\textbf{undefined} instead of \textbf{true} if an answer is conditional.

\begin{description}
    \predicate{undefined}{0}{}
Unknown represents neither \const{true} nor \const{false} in the
well formed model.  It is implemented as

\begin{code}
:- table undefined/0.

undefined :- tnot(undefined).
\end{code}
\end{description}

\index{residual,WFS}%
Solving a set of predicates under well formed semantics results in a
\jargon{residual program}. This program contains clauses for all tabled
predicates with condition answers where each clause head represents and
answer and each clause body its condition. The condition is a
disjunction of conjunctions where each literal is either a tabled
goal or tnot/1 of a tabled goal.   The remaining model has at least a
cycle through a negative literal (tnot/1) and has no single
solution in the \jargon{stable model semantics}, i.e., it either
expresses a contradiction (as undefined/0, i.e., there is no stable
model) or a multiple stable models as in the program below, where both
\{p\} and \{q\} are stable models.

\begin{code}
:- table p/0, q/0.

p :- tnot(q).
q :- tnot(p).
\end{code}

Note that it is possible that some literals have the same truth value in
all stable models but are still \jargon{undefined} under the stable model
semantics.

The residual program is an explanation of why an answer is undefined.
SWI-Prolog offers the following predicates to access the residual
program.

\begin{description}
    \predicate{call_residual_program}{2}{:Goal, -Program}
True when \arg{Goal} is an answer according to the Well Founded
Semantics.  If \arg{Program} is the empty list, \arg{Goal} is
unconditionally true.  Otherwise this is a program as described
by delays_residual_program/2.

    \predicate{call_delays}{2}{:Goal, -Condition}
True when \arg{Goal} is an answer that is true when Condition can be
satisfied. If \arg{Condition} is \const{true}, \arg{Answer} is
unconditional. Otherwise it is a conjunction of goals, each of which is
associated with a tabled predicate.

    \predicate{delays_residual_program}{2}{:Condition, -Program}
Program is a list of clauses that represents the connected program
associated with \arg{Condition}.  Each clause head represents a
conditional answer from a table and each corresponding clause body
is the condition that must hold for this answer to be true.  The
body is a disjunction of conjunctions.  Each leaf in this condition
is either a term \term{tnot}{Goal} or a plain \arg{Goal}.  Each
\arg{Goal} is associated with a tabled predicate.  The program
always contains at least one cycle that involves tnot/1.
\end{description}


\subsection{Well founded semantics and the toplevel}
\label{sec:wfs-toplevel}

The toplevel supports two modes for reporting that it is undefined whether
the current answer is true. The mode is selected by the Prolog flag
\prologflag{toplevel_list_wfs_residual_program}. If \const{true}, the
toplevel uses call_delays/2 and delays_residual_program/2 to find the
conditional answers used and the \jargon{residual} program associated
with these answers. It then prints the residual program, followed by the
answer and the conditional answers. For undefined/0, this results in the
following output:

\begin{code}
?- undefined.
% WFS residual program
    undefined :-
        tnot(undefined).
undefined.
\end{code}

If the \prologflag{toplevel_list_wfs_residual_program} is false, any
undefined answer is a conjunction with undefined/0.  See the program and
output below.

\begin{code}
:- table p/0, q/0.

p :- tnot(q).
q :- tnot(p).
\end{code}

\begin{code}
?- p.
% WFS residual program
    p :-
        tnot(q).
    q :-
        tnot(p).
p.

?- set_prolog_flag(toplevel_list_wfs_residual_program, false).
true.

?- p.
undefined.
\end{code}

\section{Incremental tabling}
\label{sec:tabling-incremental}

Incremental tabling maintains the consistency of a set of tabled
predicates that depend on a set of dynamic predicates. Both the tabled
and dynamic predicates must have the property \const{incremental} set.
See dynamic/1 and table/1.

Incremental tabling causes the engine to connect the \jargon{answer
tries} and incremental dynamic predicates in an \jargon{Incremental
Dependency Graph} (IDG). Modifications (asserta/1, retract/1,
retractall/1 and friends) of an incremental dynamic predicate mark all
depending tables as invalid.  Subsequent usage of these tables forces
re-evaluation.

Re-evaluation of invalidated tables happens on demand, i.e., on access
to an invalid table. First the dependency graph of invalid tables that
lead to dynamic predicates is established. Next, tables are re-evaluated
in \jargon{bottom-up} order. For each re-evaluated table the system
determines whether the new table has changed. If the table has not
changed, this event is propagated to the \jargon{affected} nodes of the
IDG and no further re-evaluation may be needed. Consider the following
program:

\begin{code}
:- table (p/1, q/1) as incremental.
:- dynamic([d/1], [incremental(true)]).

p(X) :- q(X).
q(X) :- d(X), X < 10.

d(1).
\end{code}

Executing this program creates tables for $X=1$ for \nopredref{p}{1} and
\nopredref{q}{1}. After calling \exam{assert(d(100))} the tables for
\nopredref{p}{1} and \nopredref{q}{1} have an \jargon{invalid count} of
$1$. Re-running \exam{p(X)} first re-evaluates \nopredref{q}{1}
(bottom-up) which results to the same table as $X=100$ does not lead to
a new answer. Re-evaluation clears the invalid count for
\nopredref{q}{1} and, because the \nopredref{q}{1} tables is not
changed, decrements the invalid count of affected tables. This sets the
\jargon{invalid count} for \nopredref{p}{1} to zero, completing the
re-evaluation.

Note that invalidating and re-evaluation is done at the level of tables.
Notably asserting a clause invalidates all affected tables and may lead
to re-evaluating of all these tables. Incremental tabling automates
manual abolishing of invalid tables in a changing world and avoids
unnecessary re-evaluation if indirectly affected tables prove unaffected
because the answer set of dependent tables is unaffected by the change.
This is the same policy as implemented in XSB
\cite{DBLP:journals/tplp/Swift14}. Future versions may implement a more
fine grained approach.


\section{Monotonic tabling}
\label{sec:tabling-monotonic}

\jargon{Incremental tabling} (\secref{tabling-incremental}) maintains
the consistency of tables that depend directly or indirectly on
(incremental) dynamic predicates. This is done by \jargon{invalidating}
dependent tables on an assert or retract and lazily \jargon{re-evaluate}
invalid tables when their content is needed. Incremental tabling
preserves all normal tabling properties, including well founded
semantics. The downside is that re-evaluation recomputes the table from
scratch. This section deals with \jargon{monotonic tabling}, a mechanism
that propagates the consequences of assert/1 and friends without
recomputing the dependent tables from scratch. Unlike incremental
tabling though, monotonic tabling can only deal with monotonic programs,
in particular it does \emph{not} deal with negation.

The example below defines the transitive closure of a bi-directional
graph using monotonic tabling. This program builds tables for the
\nopredref{connected}{2} and maintains these tables when new facts are
added for \nopredref{link}{2}.

\begin{code}
:- table connected/2 as monotonic.
:- dynamic link/2 as monotonic.

connected(X, Y) :-
    connected(Y, X).
connected(X, Z) :-
    connected(X, Y),
    connected(Y, Z).
connected(X, Y) :-
    link(X, Y).
\end{code}

\begin{description}
    \predicate{abolish_monotonic_tables}{0}{}
Abolish all monotonic tables and their dependencies.
\end{description}

The list below describes properties of monotonic tabling and relation to
other tabling primitives:

\begin{itemize}
   \item When using retract/1 on a dynamic monotonic predicate, all
dependent tables and dependency links are invalidated and marked for
normal \jargon{incremental} update.

   \item abolish_all_tables/0 destroys all monotonic dependency relations.

   \item Dynamic predicates can be declared as both \const{monotonic} and
\const{incremental} and it allowed to have both incremental and monotonic
tabled predicates that depend on such dynamic predicates.

   \item A tabled predicate that depends on a monotonic tabled predicate
must be tabled monotonic or incremental. If the dependent predicate is
incremental a new answer invalidates the incremental table.
\end{itemize}

\subsection{Eager and lazy monotonic tabling}
\label{sec:tabling-monotonic-lazy}

There are two types of monotonic tabling. The default is \emph{eager},
which implies that an asserted clause is immediately propagated through
the dependency network.  Possibly resulting new answers can be tracked
as described in \secref{tabling-tracking}. The alternative is
\emph{lazy}.  A predicate is marked for lazy using the \const{lazy}
option as shown below, or by setting the \prologflag{table_monotonic}
flag to \const{lazy}.

\begin{code}
:- table p/1 as (monotonic,lazy).
\end{code}

If a predicate is tabled as monotonic and lazy and an answer is added to
one of the monotonic dynamic predicates, all dependent monotonic or
incremental tables are invalidated and the answer is queued together
with the dependency. A subsequent call to one of the invalidated tabled
predicates re-evaluates the tables. For a monotonic table this implies
pushing the queued answers through the dependencies. Removing a clause
from one of a monotonic dynamic predicates invalidates all dependent
tables and marks all these tables for \emph{forced re-evaluation}, which
implies they are re-evaluated using the same algorithm as used for
\jargon{incremental} tabling.

Lazy monotonic tables may depend on eager monotonic tables. There is no
point in making an eager monotonic table depend on a lazy monotonic
table as one would have to re-evaluate the lazy table to make the eager
table consistent. Therefore, a dependency of an eager table on a lazy
table is silently converted into a lazy dependency.


\subsection{Tracking new answers to monotonic tables}
\label{sec:tabling-tracking}

The prolog_listen/2 interface allows for tracking new facts that are
added to monotonic tables. For example, we can print new possible
connections from the above program using this code:

\begin{code}
:- prolog_listen(connected/2, connection_change).

connection_change(new_answer, _:connected(From, To)) :-
    format('~p and ~p are now connected~n', [From, To]).
\end{code}

Currently, \jargon{failure} of the hook are ignored. If the hook throws
an exception this is propagated. The hook is executed outside the
current tabling context.\footnote{The final behavior may be different
in both aspects.}

After loading the connected/2 program and the above declarations we can
observe the interaction below. Note that query~1 establishes the
dependencies and fills the tables using normal tabling. In the current
implementation, possibly discovered connections do not trigger the
hook.\footnote{This is likely to change in the future.}. Adding a single
\nopredref{link}{2} fact links both locations to itself and to each
other in both directions.  Adding a second fact extends the network.

\begin{code}
1 ?- connected(_,_).
false.

2 ?- assert(link('Amsterdam', 'Haarlem')).
'Amsterdam' and 'Haarlem' are now connected
'Amsterdam' and 'Amsterdam' are now connected
'Haarlem' and 'Amsterdam' are now connected
'Haarlem' and 'Haarlem' are now connected
true.

3 ?- assert(link('Leiden', 'Haarlem')).
'Leiden' and 'Haarlem' are now connected
'Haarlem' and 'Leiden' are now connected
'Amsterdam' and 'Leiden' are now connected
'Leiden' and 'Amsterdam' are now connected
'Haarlem' and 'Leiden' are now connected
'Leiden' and 'Haarlem' are now connected
'Leiden' and 'Amsterdam' are now connected
'Leiden' and 'Leiden' are now connected
'Amsterdam' and 'Leiden' are now connected
true.
\end{code}

\subsection{Monotonic tabling with external data}
\label{sec:mono-external-data}

Monotonic tables depend on monotonic dynamic predicates. In some
situations there is external dynamic data such as a database. One
solution is to maintain a shadow copy of all the external data in a
dynamic predicate. This wastes resources and introduces maintenance
problems.  The system allows to use this information directly from
the external source.  To do this, create a dynamic and monotonic
predicate that accesses the data:

\begin{code}
:- dynamic my_data/2 as monotonic.

my_data(X, Y) :-
    <access external data>.
\end{code}

Any monotonic table that depends on \nopredref{my_data}{2} will be
populated correctly and build a dependency. Next, if a new answer is
added to the external data the user must call incr_propagate_calls/1
from the Prolog library \pllib{increval}. Similarly, when an answer is
removed from the external data we use incr_invalidate_calls/1. Both
notification calls must be made \emph{after} the external data has been
updated, i.e., \nopredref{my_data}{2} must reflect the new situation
before calling incr_propagate_calls/1 or incr_invalidate_calls/1.

\begin{code}
:- use_module(library(increval)).

on_new_my_data(X, Y) :-
    incr_propagate_calls(my_data(X, Y)).

on_removed_my_data(X,Y) :-
    incr_invalidate_calls(my_data(X, Y)).
\end{code}

\begin{description}
    \predicate{incr_propagate_calls}{1}{:Answer}
Activate the monotonic answer propagation similarly to when a new
fact is asserted for a monotonic dynamic predicate.  The \arg{Answer}
term must match a monotonic dynamic predicate. See
\secref{mono-external-data} for an example.
\end{description}

\paragraph{Status}

Monotonic tabling is experimental and incomplete. Notably support for
\jargon{answer subsumption} and \jargon{call subsumption} is probably
possible and may greatly improve the application domain and resource
usage. Monotonic tabling should work with both shared and private
tables.  Concurrency issues have not yet been tested though.


\section{Shared tabling}
\label{sec:tabling-shared}

Tables can both be \jargon{private} to a thread or \jargon{shared}
between all threads. Private tables are used only by the calling threads
and are discarded as the thread terminates.  Shared tables are used by
all threads and can only be discarded explicitly. Tables are declared as
shared using, e.g.,

\begin{code}
:- table (p/1, q/2) as shared.
\end{code}

A thread may find a table for a particular variant of a shared tabled
predicate in any of the following states:

\begin{description}
    \item[Complete]
If the table is complete we can simply use its answers.
    \item[Fresh/non-existent]
If the table is still fresh, claim ownership for it and start filling
the table.  When completed, the ownership relation is terminated.
    \item[Incomplete]
If the table is incomplete and owned by the calling thread, simply
continue. If it is owned by another thread we \emph{wait} for the table
\emph{unless there is a cycle of threads waiting for each others table}.
The latter situation would cause a deadlock and therefore we raise a
\const{deadlock} exception. This exception causes the current SCC to be
abandoned and gives other threads the opportunity to claim ownership of
the tables that were owned by this thread. The thread that raised the
exception and abandoned the SCC simply restarts the leader goal of the
SCC. As other threads now have claimed more variants of the SCC it will,
in most cases, wait for these threads instead of creating a new
deadlock.
\end{description}

A thread that waits for a table may be faced with three results. If the
table is complete it can use the answers. It is also possible that the
thread that was filling the table raised an exception (either a
\const{deadlock} or any other exception), in which case we find a
\jargon{fresh} table for which we will try to claim ownership. Finally,
some thread may have abolished the table. This situation is the same as
when the owning thread raised an exception.

\subsection{Abolishing shared tables}
\label{sec:tabling-shared-abolish}

This section briefly explains the interaction between deleting shared
tables and running threads. The core rule is that \emph{abolishing a
shared table has no effect on the semantics of the tabled predicates.}
An attempt to abolish an incomplete table results in the table to be
marked for destruction on completion. The thread that is completing the
table continues to do so and continues execution with the computed table
answers. Any other thread blocks, waiting for the table to complete.
Once completed, the table is destroyed and the waiting threads see a
\jargon{fresh} table\footnote{Future versions may avoid waiting by
converting the abolished shared table to a private table.}.

The current implementation never reclaims shared tables.  Instead, they
remain part of the global variant table and only the answers of the
shared table are reclaimed. Future versions may garbage collect such
tables.  See also abolish_shared_tables/0.


\subsection{Status and future of shared tabling}
\label{sec:tabling-shared-status}

Currently, shared tabling has many restrictions. The implementation does
not verify that the limitations are met and violating these restrictions
may cause incorrect results or crashes. Future versions are expected to
resolve these issues.

\begin{itemize}
    \item Shared tabling currently only handles the basic scenario and
    cannot yet deal with well formed semantics or incremental tabling.
    \item As described in \secref{tabling-shared-abolish}, abolishing
    shared tables may cause unnecessary waiting for threads to complete
    the table.
    \item Only the answers of shared tables can be reclaimed, not the
    answer table itself.
\end{itemize}

SWI-Prolog's \jargon{continuation based} tabling offers the opportunity
to perform \jargon{completion} using multiple threads.


\section{Tabling and constraints}
\label{sec:tabling-constraints}

Starting with version 9.3.24, SWI-Prolog offers some support for
tabled execution with constraints (attributed variables, see
\secref{attvar}) All relevant data structures support attributed
variables, notably tries (see \secref{trie}).  The basic attributed
variable and tabling with attributed variables tests from XSB have
been ported and integrated in SWI-Prolog's test suite.\footnote{Thanks
to Theresa Swift and David Warren.}  Some remarks:

\begin{itemize}
\item The \jargon{variant} is defined by the attributes and their
  values.  Note however that SWI-Prolog represents multiple attributes
  in a linked list where the ordering depends on the order in which
  the attributes were added while, ideally, the order is not relevant
  for the semantics of attributes.
\item Solving a goal with attributed variables may \emph{modify}
  attributes.  As a result, enumeration of answers from the completed
  trie \emph{replaces} attributes rather than unifying with the
  attributes.  The new set of attributes is always a copy of the
  original set of attributes.  For example:

\begin{code}
:- use_module(library(clpfd)).
:- table p/1.

p(X) :- X #>= 1, X #=< 6.
p(20).
\end{code}

\begin{code}
?- X #> 0, p(X).
X in 1..6 ;
X = 20.
\end{code}

  Note that this behaviour is unlike trie_gen/2.  If an attributed
  variable is inserted into a trie, trie_gen/2 unifies the stored
  attributed term with the second argument of the call.
\end{itemize}


\section{Tabling restraints: bounded rationality and tripwires}
\label{sec:tabling-restraints}

Tabling avoids non-termination due to \jargon{self-recursion}. As Prolog
allows for infinitely nested \jargon{compound terms} (\jargon{function
symbols} in logic) and arbitrary numbers, the set of possible answers is
not finite and thus there is no guaranteed termination.

This section describes \jargon{restraints}
\cite{DBLP:conf/aaai/GrosofS13} that can be enforced to specific or all
tabled predicates. Currently there are three defined restraints,
limiting (1) the size of (the arguments to) goals, (2) the size of the
answer substitution added to a table and (3) the number of answers
allowed in any table. If any of these events occurs we can specify the
action taken. We distinguish two classes of actions. First, these events
can trap a \jargon{tripwire} which can be handled using a hook or a
predefined action such as raising an exception, printing a warning or
enter a \jargon{break level}. This can be used for limiting resources,
be notified of suspicious events (debugging) or dynamically adjust the
(tabling) strategy of the program. Second, they may continue the
computation that results in a partial answer (\jargon{bounded
rationality}). Unlike just not exploring part of the space though, we
use the third truth value of well founded semantics to keep track of
answers that have not been affected by the restraints and those that
have been affected.

The tripwire actions apply for all restraints.  If a tripwire action is
triggered, the system takes the steps below.

\begin{enumerate}
    \item Call the prolog:tripwire/2 hook.
    \item If prolog:tripwire/2 fails, take one of the predefined
    actions:
    \begin{description}
	\termitem{warning}{}
	Print a message indicating the trapped tripwire and continue
	execution as normal, i.e., the final answer is the same as
	if no restraint was active.
	\termitem{error}{}
	Throw an exception
	\verb$error(resource_error(tripwire(Wire,Context)))$.
	\termitem{suspend}{}
	Print a message and start a \jargon{break level} (see break/0).
    \end{description}
\end{enumerate}


\begin{description}
    \predicate[multifile]{prolog:tripwire}{2}{Wire, Context}
Called when tripwire \arg{Wire} is trapped. \arg{Context} provides
additional context for interpreting the tripwire.  The hook can take
one of three actions:

\begin{itemize}
    \item Succeed.  In this case the tripwire is considered handled and
    execution proceeds as if there was no tripwire.  Note that tripwires
    only trigger at the exact value, which implies that a wire on a
    count will be triggered only once.  The hook can install a new
    tripwire at a higher count.
    \item Fail.  In this case the default action is taken.
    \item Throw an exception.  Exceptions are propagated normally.
\end{itemize}
\end{description}

\jargon{Radial restraints} limit the sizes of subgoals or answers.
Abstraction of a term according to the size limit is implemented by
size_abstract_term/3.

\begin{description}
    \predicate[det]{size_abstract_term}{3}{+Size, +Term, -Abstract}
The size of a term is defined as the number of compound subterms
(\jargon{function symbols}) that appear in term. \arg{Abstract} is an
abstract copy of \arg{Term} where each argument is abstracted by
copying only the first \arg{Size} function symbols and constants.
Excess function symbols are replaced by fresh variables.

This predicate is a helper for tabling where \arg{Term} is the
\functor{ret}{N} \jargon{answer skeleton} that is added to the
\jargon{answer table}.  Examples:

\begin{center}\begin{tabular}{cll}
\textbf{Size} & \textbf{Term} & \textbf{Abstract} \\
\hline
0 & ret(f(x), a)       & ret(_, a) \\
1 & ret(f(x), a)       & ret(f(x), a) \\
1 & ret(f(A), a)       & ret(f(A), a) \\
1 & ret(f(x), x(y(Z))) & ret(f(x), x(_)) \\
\end{tabular}\end{center}

    \predicate[undefined]{radial_restraint}{0}{}
This predicate is \jargon{undefined} in the sense of well founded
semantics (see \secref{WFS} and undefined/0). Any answer that depends on
this condition is undefined because either the restraint on the subgoal
size or answer size was violated.
\end{description}


\subsection{Restraint subgoal size}
\label{sec:tabling-restraint-subgoal}

Using the \term{subgoal_abstract}{Size} attribute, a tabled subgoal that
that is too large is \jargon{abstracted} by replacing compound subterms
of the goal with variables. In a nutshell, a goal
\term{p}{s(s(s(s(s(0)))))} is converted into the semantically equivalent
subgoal if the subgoal size is limited to~3.

\begin{code}
    ...,
    p(s(s(s(X)))), X = s(s(0)),
    ...,
\end{code}

As a result of this, terms stored in the \jargon{variant trie} that maps
goal variants into \jargon{answer tables} is limited. Note that does not
limit the number of answer tables as atomic values are never abstracted
and there are, for example, an infinite number of integers. Note that
restraining the subgoal size does not affect the semantics, provided
more general queries on the predicate include all answers that more
specific queries do.  See also \jargon{call substitution} as described
in \secref{tabling-subsumptive}.  In addition to the tripwire actions,
the \prologflag{max_table_subgoal_size_action} can be set to the
value \const{abstract}:

\begin{description}
    \termitem{abstract}{}
Abstract the goal as described above and provide correctness by adding
the required unification instructions after the goal.
\end{description}


\subsection{Restraint answer size}
\label{sec:tabling-restraint-answer-size}

Using the \term{answer_abstract}{Size} attribute, a tabled subgoal that
produces answer substitutions (instances of the variables in the goal)
whose size exceed \arg{Size} are trapped. In addition to the
tripwire actions, answer abstraction defines two additional modes
for dealing with too large answers as defines by the Prolog flag
\prologflag{max_table_answer_size_action}:

\begin{description}
    \termitem{fail}{}
Ignore the too large answer.  Note that this is semantically incorrect.
    \termitem{bounded_rationality}{}
In this mode, the large answer is \jargon{abstracted} in the same way as
subgoals are abstracted (see \secref{tabling-restraint-subgoal}). This
is semantically incorrect, but our third truth value \jargon{undefined}
is used to remedy this problem. In other words, the abstracted value is
added to the table as \jargon{undefined} and all conclusions that depend
on usage of this abstracted value are thus undefined unless they can
also be proved some other way.
\end{description}

\subsection{Restraint answer count}
\label{sec:tabling-restraint-answer-count}

Finally, using ``as \term{max_answers}{Count}'' or the Prolog flag
\prologflag{max_answers_for_subgoal}, the number of answers in a table
is restrained. In addition to the tripwire actions this restraint
supports the action \const{bounded_rationality}\footnote{The action
\const{complete_soundly} is supported as a synonym for XSB
compatibility}. If the restraint is reached in the bounded rationality
mode the system takes the following actions:

\begin{itemize}
    \item Ignore the answer that triggered the restraint.
    \item Prune the choice points of the tabled goal to avoid more
          answers.
    \item Add an new answer to the table that does not bind any
          variables, i.e., an empty answer substitution.  This answer
	  is conditional on answer_count_restraint/0.
\end{itemize}

\begin{description}
    \predicate[undefined]{answer_count_restraint}{0}{}
This predicate is \jargon{undefined} in the sense of well founded
semantics (see \secref{WFS} and undefined/0). Any answer that depends on
this condition is undefined because the \const{max_answers} restraint on
some table was violated.
\end{description}

The program and subsequent query below illustrate the behavior.

\begin{code}
:- table p/2 as max_answers(3).

p(M,N) :-
    between(1,M,N).
\end{code}

\begin{code}
?- p(1 000 000, X).
X = 3 ;
X = 2 ;
X = 1 ;
% WFS residual program
    p(1000000, X) :-
        answer_count_restraint.
p(1000000, X).
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Tabling predicate reference}
\label{sec:tabling-preds}

\begin{description}
    \directive{table}{1}{:Specification}
Prepare the predicates specified by \arg{Specification} for tabled
execution.  \arg{Specification} is a \jargon{comma-list}, each member
specifying tabled execution for a specific predicate.  The individual
specification is either a \jargon{predicate indicator} (name/arity or
name//arity) or head specifying tabling with \jargon{answer subsumption}.

Although table/1 is normally used as a directive, SWI-Prolog allows
calling it as a runtime predicate to prepare an existing predicate for
tabled execution. The predicate untable/1 can be used to remove the
tabling instrumentation from a predicate.

The example below prepares the predicate edge/2 and the
non-terminal statement//1 for tabled execution.

\begin{code}
:- table edge/2, statement//1.
\end{code}

Below is an example declaring a predicate to use tabling with
\jargon{answer subsumption}. Answer subsumption or \jargon{mode directed
tabling} is discussed in \secref{tabling-mode-directed}.

\begin{code}
:- table connection(_,_,min).
\end{code}

Additional tabling options can be provided using a term \funcref{as}{2},
which can be applied to a single specification or a comma list of
specifications.  The options themselves are a comma-list of one or more
of the following atoms:

    \begin{description}
    \termitem{variant}{}
    Default.  Create a table for each call variant.
    \termitem{subsumptive}{}
    Instead of creating a new table for each call variant, check
    whether there is a completed table for a more general goal and
    if this is the case extract the answers from this table.  See
    \secref{tabling-subsumptive}.
    \termitem{shared}{}
    Declare that the table shall be shared between threads.  See
    \secref{tabling-shared}
    \termitem{private}{}
    Declare that the table shall be local to the calling thread.  See
    \secref{tabling-shared}
    \termitem{incremental}{}
    Declare that the table depends on other tables and
    \jargon{incremental} dynamic predicates. See
    \secref{tabling-incremental}.
    \termitem{dynamic}{}
    Declare that the predicate is dynamic.  Often used together
    with \const{incremental}.
    \end{description}

This syntax is closely related to the table declarations used in XSB
Prolog. Where in XSB \const{as} is an operator with priority above the
priority of the comma, it is an operator with priority below the comma
in SWI-Prolog. Therefore, multiple predicates or options must be
enclosed in parenthesis. For example:

\begin{code}
:- table p/1 as subsumptive.
:- table (q/1, r/2) as subsumptive.
\end{code}

    \predicate{tnot}{1}{:Goal}
The tnot/1 predicate implements \jargon{tabled negation}. This predicate
realises \jargon{Well Founded Semantics}. See \secref{WFS} for details.

    \predicate{not_exists}{1}{:Goal}
Handles tabled negation for non-ground (\jargon{floundering}) \arg{Goal}
as well as non tabled goals. If \arg{Goal} is ground and tabled
not_exists/1 calls tnot/1. Otherwise it used \term{tabled_call}{Goal} to
create a table and subsequently uses tnot/1 on the created table.

Logically, \term{not_exists}{p(X)} is defined as
\mbox{tnot($\exists{}$\arg{X}(p(\arg{X})))}

Note that each \arg{Goal} variant populates a table for tabled_call/1.
Applications may need to abolish such tables to limit memory usage or
guarantee consistency `after the world changed'.

    \predicate{tabled_call}{1}{:Goal}
Helper predicate for not_exists/1. Defined as below. The helper is
public because application may need to abolish its tables.

\begin{code}
:- table tabled_call/1 as variant.
tabled_call(Goal) :- call(Goal).
\end{code}

    \predicate{current_table}{2}{:Variant, -Trie}
True when \arg{Trie} is the answer table for \arg{Variant}.

    \predicate{untable}{1}{:Specification}
Remove the tables and tabling instrumentation for the specified
predicates. \arg{Specification} is compatible with table/1, although
tabling with \jargon{answer subsumption} may be removed using a
name/arity specification. The untable/1 predicate is first of all
intended for examining the effect of various tabling scenarios on a
particular program interactively from the toplevel.

Note that although using untable/1 followed by table/1 may be used to
flush all tables associated with the given predicate(s), flushing tables
should be done using one of the table abolish predicates both for
better performance and compatibility with other Prolog implementations:
abolish_all_tables/0, abolish_private_tables/0, abolish_shared_tables/0,
abolish_module_tables/1 or abolish_table_subgoals/1. For example, to
remove all tables for \nopredref{p}{3}, run the goal below. The
predicate functor/3 may be used to create a \jargon{head term} from a
given name and arity.

\begin{code}
?- abolish_table_subgoals(p(_,_,_)).
\end{code}

    \predicate{abolish_all_tables}{0}{}
Remove all tables, both \jargon{private} and \jargon{shared} (see
\secref{tabling-shared}). Since the introduction of \jargon{incremental
tabling} (see \secref{tabling-incremental}) abolishing tables is rarely
required to maintain consistency of the tables with a changed
environment. Tables may be abolished regardless of the current state of
the table. \jargon{Incomplete} tables are flagged for destruction when
they are completed. See \secref{tabling-shared-abolish} for the
semantics of destroying shared tables and the following predicates for
destroying a subset of the tables: abolish_private_tables/0,
abolish_shared_tables/0, abolish_table_subgoals/1 and
abolish_module_tables/1.

    \predicate{abolish_private_tables}{0}{}
Abolish all tables that are private to this thread.

    \predicate{abolish_shared_tables}{0}{}
Abolish all tables that are shared between threads. See also
\secref{tabling-shared-abolish}

    \predicate{abolish_table_subgoals}{1}{:Subgoal}
Abolish all tables that unify with \arg{SubGoal}.  Tables that have
undefined answers that depend of the abolished table are abolished
as well (recursively).  For example, given the program below,
\term{abolish_table_subgoals}{und} will also abolish the table
for \nopredref{p}{0} because its answer refers to \nopredref{und}{0}.

\begin{code}
p :- und.
und :- tnot(und).
\end{code}

    \predicate{abolish_module_tables}{1}{+Module}
Remove all tables that belong to predicates in \arg{Module}.

    \predicate{abolish_nonincremental_tables}{0}{}
    \nodescription
    \predicate{abolish_nonincremental_tables}{1}{+Options}
Similar to abolish_all_tables/0, but does not abolish
\jargon{incremental} tables as their consistency is maintained by
the system.  Options:

    \begin{description}
    \termitem{on_incomplete}{Action}
    \arg{Action} is one of \const{skip} or \const{error}.
    If \arg{Action} is \const{skip}, do not delete the
    table.\bug{XSB marks such tables for deletion after
    completion. That is not yet implemented.}
    \end{description}
\end{description}


\section{About the tabling implementation}
\label{sec:tabling-about}

The SWI-Prolog implementation uses \jargon{Delimited continuations} (see
\secref{delcont} to realise suspension of variant calls. The initial
version was written by Benoit Desouter and described in
\cite{DBLP:journals/tplp/DesouterDS15}. We moved the main data
structures required for tabling, the \jargon{answer tables} (see
\secref{trie}) and the \jargon{worklist} to SWI-Prolog's C~core.
\jargon{Mode directed tabling} (\secref{tabling-mode-directed}) is based
on a prototype implementation by Fabrizio Riguzzi.

The implementation of dynamic SCCs, dynamically stratified negation and
Well Founded Semantics was initiated by Benjamin Grosof from Kyndi and
was realised with a lot of help by Theresa Swift, David Warren and
Fabrizio Riguzzi, as well as publications about XSB
\cite{DBLP:journals/toplas/SagonasS98,SAGONAS20001}.

The table/1 directive causes the creation of a wrapper calling the
renamed original predicate. For example, the program in
\secref{tabling-non-termination} is translated into the following
program. We give this information to improve your understanding of the
current tabling implementation. Future versions are likely to use a more
low-level translation that is not based on wrappers.


\begin{code}
connection(A, B) :-
	start_tabling(user:connection(A, B),
		      'connection tabled'(A, B)).

'connection tabled'(X, Y) :-
	connection(X, Z),
	connection(Z, Y).
'connection tabled'(X, Y) :-
	connection(Y, X).

'connection tabled'('Amsterdam', 'Schiphol').
'connection tabled'('Amsterdam', 'Haarlem').
'connection tabled'('Schiphol', 'Leiden').
'connection tabled'('Haarlem', 'Leiden').
\end{code}


\subsubsection{Status of tabling}
\label{sec:tabling-status}

The current implementation is merely a first prototype. It needs several
enhancements before we can consider it a serious competitor to Prolog
systems with mature tabling such as XSB, YAP and B-Prolog. In
particular,

\begin{shortlist}
    \item The performance needs to be improved.
    \item Memory usage needs to be reduced.
    \item Tables must be shared between threads, both to
          reduce space and avoid recomputation.
    \item Tables must be invalidated and reclaimed automatically.
    \item Notably XSB supports incremental tabling and well-founded
          semantics under negation.
\end{shortlist}

