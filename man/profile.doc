\section{Execution profiling}			\label{sec:profile}

This section describes the hierarchical execution profiler. This
profiler is based on ideas from \program{gprof} described in
\cite{graham82gprof}. The profiler consists of two parts: the
information-gathering component built into the kernel,%
	\footnote{There are two implementations; one based on
		  setitimer() using the \const{SIGPROF} signal and one
		  using Windows Multi Media (MM) timers. On other
		  systems the profiler is not provided.}
and a presentation component which is defined in the \pllib{statistics}
library.  The latter can be hooked, which is used by the XPCE module
\pllib{swi/pce_profile} to provide an interactive graphical
frontend for the results.

\input{libprofile.tex}

\subsection{Visualizing profiling data}			\label{sec:pceprofile}

Browsing the annotated call-tree as described in \secref{profilegather}
itself is not very attractive. Therefore, the results are combined per
predicate, collecting all \emph{callers} and \emph{callees} as well
as the propagation of time and activations in both directions.
\Figref{profnode} illustrates this. The central yellowish line is the
`current' predicate with counts for time spent in the predicate
(`Self'), time spent in its children (`Siblings'), activations through
the call and redo ports. Above that are the \emph{callers}. Here, the
two time fields indicate how much time is spent serving each of the
callers. The columns sum to the time in the yellowish line. The caller
\emph{$<$recursive$>$} is the number of recursive calls. Below the
yellowish lines are the callees, with the time spent in the callee
itself for serving the current predicate and the time spent in the
callees of the callee ('Siblings'), so the whole time-block adds up to
the `Siblings' field of the current predicate. The `Access' fields show
how many times the current predicate accesses each of the callees.

The predicates have a menu that allows changing the view of the
detail window to the given caller or callee, showing the documentation
(if it is a built-in) and/or jumping to the source.

\postscriptfig[width=0.8\linewidth]{profnode}{
Execution profiler showing the activity of the
predicate chat:inv_map_list/5.}

The statistics shown in the report field of \figref{profnode} show
the following information:

\begin{itemlist}
    \item [samples]
Number of times the call-tree was sampled for collecting time
statistics.  On most hardware, the resolution of \const{SIGPROF}
is 1/100 second.  This number must be sufficiently large to get
reliable timing figures.  The {\sf Time} menu allows viewing time
as samples, relative time or absolute time.

    \item [sec]
Total user CPU time with the profiler active.

    \item [predicates]
Total count of predicates that have been called at least one time
during the profile.

    \item [nodes]
Number of nodes in the call-tree.

    \item [distortion]
How much of the time is spent building the call-tree as a percentage
of the total execution time.  Timing samples while the profiler is
building the call-tree are not added to the call-tree.
\end{itemlist}


\subsection{Information gathering}		\label{sec:profilegather}

While the program executes under the profiler, the system builds a
\emph{dynamic} call-tree. It does this using three hooks from the
kernel: one that starts a new goal (\emph{profCall}), one that tells the
system which goal is resumed after an \emph{exit} (\emph{profExit}) and
one that tells the system which goal is resumed after a \emph{fail}
(i.e., which goal is used to \emph{retry} (\emph{profRedo})). The
profCall() function finds or creates the subnode for the argument
predicate below the current node, increments the call-count of this link
and returns the sub-node which is recorded in the Prolog stack-frame.
Choice-points are marked with the current profiling node. profExit() and
profRedo() pass the profiling node where execution resumes.

Just using the above algorithm would create a much too big tree due to
recursion. For this reason the system performs detection of recursion.
In the simplest case, recursive procedures increment the `recursive'
count on the current node. Mutual recursion, however, is not easily
detected. For example, call/1 can call a predicate that uses call/1
itself. This can be viewed as a recursive invocation, but this is
generally not desirable. Recursion is currently assumed if the same
predicate \emph{with the same parent} appears higher in the call-graph.
Early experience with some non-trivial programs are
promising.

The last part of the profiler collects statistics on the CPU time
used in each node. On systems providing setitimer() with
\const{SIGPROF}, it `ticks' the current node of the call-tree each
time the timer fires.  On Windows, a MM-timer in a separate thread
checks 100 times per second how much time is spent in the profiled
thread and adds this to the current node.  See \secref{winprofile}
for details.


\subsubsection{Profiling in the Windows Implementation}
\label{sec:winprofile}

Profiling in the Windows version is similar, but as profiling is a
statistical process it is good to be aware of the implementation%
	\footnote{We hereby acknowledge Lionel Fourquaux, who
		  suggested the design described here after a
		  newsnet enquiry.}
for proper interpretation of the results.

Windows does not provide timers that fire asynchronously, frequent and
proportional to the CPU time used by the process. Windows does provide
multi-media timers that can run at high frequency. Such timers, however,
run in a separate thread of execution and they are fired on the wall
clock rather than the amount of CPU time used. The profiler installs
such a timer running, for saving CPU time, rather inaccurately at about
100 Hz. Each time it is fired, it determines the CPU time in
milliseconds used by Prolog since the last time it was fired. If this
value is non-zero, active predicates are incremented with this value.
