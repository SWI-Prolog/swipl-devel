Imperative languages like C++, Python or JavaScript execute mostly
linear code with some branching and subroutine calls. Their debuggers
support stepping through the code and pausing on each line, or running
the program until it hits a breakpoint and pauses. When paused, the user
can inspect the current program state or give the debugger commands.

Prolog has a logical execution model that involves attempting to prove
logical predicates and needs a different debugging approach. SWI-Prolog
uses the traditional Prolog "Byrd Box Model" or "4 Port Model" debugging
approach described by [@byrd:80;@Clocksin:87] with a couple of extensions
to implement its command line debugger. There are two other debuggers
available that build on this infrastructure: a [graphical
debugger](https://www.swi-prolog.org/gtrace.html) and remote debugging
in the web interface provided by [SWISH](https://swish.swi-prolog.org/).

Reference information to all predicates available for manipulating the
debugger is in [the debugger section](#debugger).

## The Byrd Box Model And Ports {#byrd-box-model}

Standard Prolog debugging tools are built around the so-called "Byrd Box
Model" or "4 Port Model" which models each predicate in a Prolog program
as a state machine ("box") that transitions through states ("ports") as
a program is evaluated. The developer can ask the engine to pause for
program inspection when it reaches specific ports or predicates.

As we go through this overview, remember that a "port" is just another
word for a "state" in the state machine that each predicate transitions
through during evaluation. The state machine is called a "box" because
it is drawn like this:

~~~
                *--------------------------------------*
         Call   |                                      |   Exit
     ---------> +  descendant(X,Y) :- offspring(X,Y).  + --------->
                |                                      |
                |  descendant(X,Z) :-                  |
     <--------- +     offspring(X,Y), descendant(Y,Z). + <---------
         Fail   |                                      |   Redo
                *--------------------------------------*
~~~

The standard ports are: `call`, `redo`, `exit` and `fail`. SWI-Prolog
extends this with two more: `unify` and `exception`. Each trace happens
at a particular phase of predicate resolution. Recall that when
resolving or "proving" a predicate, the Prolog engine:

1. Collects all rules that *might* match by having a head with the same
   name and number of arguments
   - `call` is traced, once, if *any* rules might match.
   - `redo` is also traced when the engine backtracks to find the next
     matching rule.
2. Finds the next matching rule whose head can be unified with the predicate
   - `unify` is traced with the results of unification if one is found.
   - `fail` is traced if no rule heads can be unified.
3. Applies variable assignments from unification to clauses in the rule
   body and continues at #1 with the updated clauses
6. After *all* of the body clauses of the matched rule have either succeeded,
   failed, or thrown an exception:
   - `exit` is traced if all of them succeeded (meaning this rule is true).
   - `fail` is traced if any of them failed (meaning this rule is false).
   - `exception` is traced if any of them threw an exception.

This means there can be *a lot* of traces between the initial `call` and
the end of tracing for a particular predicate.

## Trace Mode Example {#trace-example}

The trace/0 predicate turns on "trace mode", which, by default, produces
a trace and pauses at every port of every predicate to allow inspection
of the state of the program. This is normally done from the Prolog
console window, but for embedded Prolog systems or when Prolog runs as a
daemon it can also be done by getting a prompt via the
[libssh](https://www.swi-prolog.org/pack/list?p=libssh) package.

> Note: If the native graphics plugin (XPCE) is available, the commands
> gtrace/0 and gspy/1 activate the graphical debugger while tdebug/0 and
> tspy/1 allow debugging of arbitrary threads.

Each goal is printed using the Prolog predicate write_term/2. The style
is defined by the Prolog flag `debugger_write_options` and can
be modified using this flag or using the [`w`, `p` and `d` commands of
the tracer](#trace-formatting-commands).

Here's an example debugging session that shows the basic flow. The
`unify` port is off by default since it doesn't add a lot of information
in most cases for the command line debugger.

~~~
is_a(rock1, rock).
is_a(rock2, rock).
color(rock1, red).

noun(X, Type) :- is_a(X, Type).
adjective(X, color, Value) :- color(X, Value).
~~~

~~~
?- trace.
true.

[trace]  ?- noun(X, rock), adjective(X, color, red).
   Call: (11) noun(_9774, rock) ? creep
~~~

The trace/0 predicate turned on trace mode, which is now indicated at
every prompt by ``[trace] ?-``. The initial query provided by the user was
`noun(X, rock), adjective(X, color, red)` which is asking to find a "red
rock". Finally: the first port triggered was a `Call` to the first
predicate in the initial query, indicating the engine is about to look
for the first rule that matches `noun(_9774, rock)`.

Pressing `spacebar`, `c`, or `enter` caused the tracer to print `creep`
followed by the next trace. There are many additional commands available
that are described later in the overview.

~~~
is_a(rock1, rock).
is_a(rock2, rock).
color(rock1, red).

noun(X, Type) :- is_a(X, Type).
adjective(X, color, Value) :- color(X, Value).
~~~

~~~
[trace]  ?- noun(X, rock), adjective(X, color, red).
...
   Call: (12) is_a(_9774, rock) ? creep
   Exit: (12) is_a(rock1, rock) ? creep
   Exit: (11) noun(rock1, rock) ? creep
...
~~~

Next, the first clause of ``noun/2`` gets a `call` trace since the
engine is trying to find the next rule that matches `is_a(_9774, rock)`.
Since there *is* a fact that can unify: `is_a(rock1, rock)`, the trace
shows `exit` (i.e. succeeded) along with that value. Since that was the
final predicate in the body of ``noun/2``, ``noun/2`` also gets an
`exit` trace that shows the unified value of its head:
`noun(rock1, rock)`.

~~~
is_a(rock1, rock).
is_a(rock2, rock).
color(rock1, red).

noun(X, Type) :- is_a(X, Type).
adjective(X, color, Value) :- color(X, Value).
~~~

~~~
[trace]  ?- noun(X, rock), adjective(X, color, red).
...
   Call: (11) adjective(rock1, color, red) ? creep
   Call: (12) color(rock1, red) ? creep
   Exit: (12) color(rock1, red) ? creep
   Exit: (11) adjective(rock1, color, red) ? creep
   X = rock1 ;
...
~~~

Prolog then moved to the next predicate in the initial query:
``adjective/3`` and solved it in a similar way. Since that was the last
predicate in the query, an answer was returned. Pressing `;` requested
the next answer and began Prolog backtracking.

~~~
is_a(rock1, rock).
is_a(rock2, rock).
color(rock1, red).

noun(X, Type) :- is_a(X, Type).
adjective(X, color, Value) :- color(X, Value).
~~~

~~~
[trace]  ?- noun(X, rock), adjective(X, color, red).
...
   Redo: (12) is_a(_9774, rock) ? creep
   Exit: (12) is_a(rock2, rock) ? creep
   Exit: (11) noun(rock2, rock) ? creep
   Call: (11) adjective(rock2, color, red) ? creep
   Call: (12) color(rock2, red) ? creep
   Fail: (12) color(rock2, red) ? creep
   Fail: (11) adjective(rock2, color, red) ? creep
false.
~~~

The only choice point to `redo` (i.e. backtrack over) was the ``is_a/2``
clause of ``noun/2`` since there was one potential match left to attempt
to unify: `is_a(rock2, rock)`. This succeeds with an `exit` trace since
it does unify with the `redo` predicate and causes `noun(rock2, rock)`
to also succeed with `exit` just as above.

As the traces continue, you can see the `fail` port get activated for
`color(rock2, red)` since there is no way to prove that predicate and
thus the whole query returns `false`.

Tracing will continue for every query you pose until you enter
`notrace.` to turn off trace mode.

## Trace Mode Options: leash/1 and visible/1 {#trace-options}

When you enable trace mode with trace/0, the tracer will, by default,
pause and wait for a command at every port it hits on every predicate.
The leash/1 predicate can be used to modify the ports to pause at. This
is a global setting, so changes will remain until they are changed again
or SWI-Prolog is restarted. Disabling the tracer via notrace/0 doesn't
affect which ports are leashed.

The leash/1 argument must start with `+` to add, or `-` to remove,
followed by the name of a port such as `call`, `exit`, etc. There are
special terms like `all` which can be used instead of manually adding or
removing every port.

To stop only at the fail port, use leash/1 like this:

~~~
?- leash(-all).
true.

?- leash(+fail).
true.

?- trace.
true.

[trace]  ?- noun(X, rock), adjective(X, color, red).
   Call: (11) noun(_3794, rock)
   Call: (12) is_a(_3794, rock)
   Exit: (12) is_a(rock1, rock)
   Exit: (11) noun(rock1, rock)
   Call: (11) adjective(rock1, color, red)
   Call: (12) color(rock1, red)
   Exit: (12) color(rock1, red)
   Exit: (11) adjective(rock1, color, red)
X = rock1 ;
   Redo: (12) is_a(_3794, rock)
   Exit: (12) is_a(rock2, rock)
   Exit: (11) noun(rock2, rock)
   Call: (11) adjective(rock2, color, red)
   Call: (12) color(rock2, red)
   Fail: (12) color(rock2, red) ? creep
   Fail: (11) adjective(rock2, color, red) ? creep
false.
~~~

Now, only the lines that start with "Fail:" have "creep" after them
because that was the only time the tracer paused for a command. To never
pause and just see all the traces, use `leash(-all)` and don't turn any
ports back on.

The default ports are still printed out because a different setting,
visible/1, controls which ports are printed. visible/1 takes the same
form of argument as leash/1. To only stop and show the `fail` port, use
leash/1 and visible/1 like this:

~~~
?- leash(-all).
true.

?- leash(+fail).
true.

?- visible(-all).
true.

?- visible(+fail).
true.

?- trace.
true.

[trace]  ?- noun(X, rock), adjective(X, color, red).
X = rock1 ;
   Fail: (12) color(rock2, red) ? creep
   Fail: (11) adjective(rock2, color, red) ? creep
false.
~~~

## Trace Mode Commands When Paused {#trace-commands}

You can do way more than just press `spacebar` when the tracer is paused
at a port. All actions are single-character commands which are executed
*without* waiting for a return (unless the command line option
``--no-tty`` is active). Pressing `?` or `h` when paused will print out a
list of these commands as well.

### Control Flow Commands {#trace-control-flow-commands}

| **Abort** |a| Abort Prolog execution (see abort/0)|
|**Break** |b|Enter a Prolog break environment (see break/0)|
| **Creep** |c| Continue execution, stop at next port. (Also `return`, `space`)|
| **Exit** |e|Terminate Prolog (see halt/0)|
| **Fail**  |f|Force failure of the current goal|
|**Find** |/|Search for a port ([see below for the description of this command](#trace-find-command)) |
|**Ignore**  |i|Ignore the current goal, pretending it succeeded|
|**Leap**  |l|Continue execution, stop at next spy point|
|**No debug**  |n|Continue execution in 'no debug' mode|
|**Repeat find**|.|Repeat the last find command ([see 'Find'](#trace-find-command))|
|**Retry**  |r|Undo all actions (except for database and I/O actions) back to the `call` port of the current goal and resume execution at the `call` port|
|**Skip**  |s|Continue execution, stop at the next port of *this* goal (thus skipping all calls to children of this goal)|
|**Spy** |+|Set a spy point (see spy/1) on the current predicate. Spy points are described [later in the overview](#spy-points-debug-mode).|
| **No spy** |-| Remove the spy point (see nospy/1) from the current predicate. Spy points are described [later in the overview](#spy-points-debug-mode).|
|**Up**  |u|Continue execution, stop at the next port of *the parent* goal (thus skipping this goal and all calls to children of this goal). This option is useful to stop tracing a failure driven loop.|

#### Find  (`/`) Description and Examples {#trace-find-command}

The Find (`/`) command continues execution until a port matching a find
pattern is found. After the `/`, the user can enter a line to specify
the port to search for. This line consists of a set of letters
indicating the port type, followed by an optional term, that should
unify with the goal run by the port. If no term is specified it is taken
as a variable, searching for any port of the specified type. If an atom
is given, any goal whose functor has a name equal to that atom matches.
Examples:

|/f|Search for any `fail` port|
|/fe solve|Search for a `fail` or `exit` port of any goal with name  `solve`|
|/c solve(a, _)|Search for a call to `solve/2` whose first argument is a variable or the atom  `a`|
|/a member(_, _)|Search for any port on `member/2`. This is equivalent to setting a spy point on `member/2`.|

### Informational Commands {#trace-information-commands}

|**Alternatives**|A| Show all goals that have alternatives|
|**Goals**|g|Show the list of parent goals (the execution stack). Note that due to tail recursion optimization a number of parent goals might not exist any more.|
|**Help**|h| Show available options (also `?`)|
|**Listing**|L|List the current predicate with `listing/1`|

### Formatting Commands {#trace-formatting-commands}

|**Context** |C|Toggle 'Show Context'. If `on`, the context module of the goal is displayed between square brackets (see [modules section](#modules)). Default is `off`.|
|**Display**  |d|Set the `max_depth(Depth)` option of [debugger_write_options](#flags), limiting the depth to which terms are printed. See also the  `w`  and  `p`  options.|
|**Print**  |p|Set the Prolog flag `debugger_write_options` to `[quoted(true), portray(true), max_depth(10), priority(699)]`. This is the default.|
|**Write**  |w|Set the Prolog flag `debugger_write_options` to `[quoted(true), attributes(write), priority(699)]`, bypassing portray/1, etc.|


## Trace Mode vs. Trace Point {#trace-mode-vs-point}

A slight detour is useful to describe some related predicates that can
be confusing: To only trace a single or select set of predicates, the
trace/1 or trace/2 predicates can be used to set a *trace point*. Even
though they use the same base predicate name `trace`, these predicates
ignore the leash/1 and visible/1 global settings and don't pause when
they trace a port. They really are a different feature that also happens
to do tracing.

A *trace point* is set on a particular predicate and traces the ports of
that predicate *whether or not you are in trace/0 trace mode*. Each
trace point can trace different ports if the trace/2 variant is used.

~~~
?- trace(is_a/2).
%         is_a/2: [all]
true.

?- noun(X, rock), adjective(X, color, red).
 T Call: is_a(_25702, rock)
 T Exit: is_a(rock1, rock)
X = rock1 ;
 T Redo: is_a(rock1, rock)
 T Exit: is_a(rock2, rock)
false.
~~~

Notice that *trace mode* did not have to be turned on using trace/0
*and* that this only traced out the ports hit while executing ``is_a/2``
*and* that the program was not ever paused.

In fact, if trace mode is turned on while using a trace point, things
get very confusing because the trace point infrastructure itself will be
traced!

~~~
?- trace(is_a/2).
%         is_a/2: [all]
true.

?- trace.
true.

[trace]  ?- noun(X, rock), adjective(X, color, red).
   Call: (11) noun(_29318, rock) ? creep
   Call: (12) is_a(_29318, rock) ? creep
   Call: (13) print_message(debug, frame(user:is_a(_29318, rock), trace(call))) ? creep
   Call: (18) push_msg(frame(user:is_a(_29318, rock), trace(call))) ? creep
   Call: (21) exception(undefined_global_variable, '$inprint_message', _30046) ? creep
   Fail: (21) exception(undefined_global_variable, '$inprint_message', _30090) ? creep
   Exit: (18) push_msg(frame(user:is_a(_29318, rock), trace(call))) ? creep
   Call: (19) prolog:message(frame(user:is_a(_29318, rock), trace(call)), _30140, _30142) ? creep
   Fail: (19) prolog:message(frame(user:is_a(_29318, rock), trace(call)), _30140, _30142) ? creep
   Call: (19) message_property(debug, stream(_30192)) ? creep
   Fail: (19) message_property(debug, stream(_30192)) ? creep
   Call: (20) message_property(debug, prefix(_30200)) ? creep
   Fail: (20) message_property(debug, prefix(_30200)) ? creep
 T Call: is_a(_29318, rock)
   Call: (17) pop_msg ? creep
   Exit: (17) pop_msg ? creep
   ...Lots more after this...
~~~

So, trace *points* are a confusingly named and separate feature from
trace *mode*.

## Spy Points and Debug Mode {#spy-points-debug-mode}

Back to trace mode features: Because the tracing output of a Prolog
program can often be quite large, sometimes it is useful to start trace
mode at a particular point deep in the program. This is what a *spy
point* is for. It specifies a predicate that should turn on trace mode.

A spy point is enabled like this: `spy(mypredicate/2)`. After that
command, the first time ``mypredicate/2`` is encountered, trace mode
will turn on and work just like it does normally. This includes paying
attention to the global leash/1 and visible/1 settings. The spy point
can be removed using nospy/1 or nospyall/0.

~~~
is_a(rock1, rock).
is_a(rock2, rock).
color(rock1, red).

noun(X, Type) :- is_a(X, Type).
adjective(X, color, Value) :- color(X, Value).
~~~

~~~
?- spy(is_a/2).
% Spy point on is_a/2
true.

[debug]  ?- noun(X, rock), adjective(X, color, red).
 * Call: (12) is_a(_1858, rock) ? creep
 * Exit: (12) is_a(rock1, rock) ? creep
   Exit: (11) noun(rock1, rock) ? creep
   Call: (11) adjective(rock1, color, red) ? creep
   Call: (12) color(rock1, red) ? creep
   Exit: (12) color(rock1, red) ? creep
   Exit: (11) adjective(rock1, color, red) ? creep
X = rock1 ;
 * Redo: (12) is_a(_1858, rock) ? creep
 * Exit: (12) is_a(rock2, rock) ? creep
   Exit: (11) noun(rock2, rock) ? creep
   Call: (11) adjective(rock2, color, red) ? creep
   Call: (12) color(rock2, red) ? creep
   Fail: (12) color(rock2, red) ? creep
   Fail: (11) adjective(rock2, color, red) ? creep
false.
~~~

After the spy point is hit, the output above is identical to the traces
generated by running trace/0 with the initial query, but is obviously
missing all of the traces before the spy point.

Note that after spy/1 is called, there is a new tag in front of ``?-``,
the ``[debug]`` tag:

~~~
?- spy(is_a/2).
% Spy point on is_a/2
true.

[debug] ?-
~~~

This means the system is in "debug mode". Debug mode does two things: it
tells the system to watch for spy points and it turns off some
optimizations that would make the traces confusing. The ideal 4-port
model ([@byrd:80]) as described in many Prolog books ([@Clocksin:87]) is
not visible in many Prolog implementations because code optimisation
removes part of the choice and exit points. Backtrack points are not
shown if either the goal succeeded deterministically or its alternatives
were removed using the cut. When running in debug mode, choice points
are only destroyed when removed by the cut and last call optimisation is
switched off. [Note: This implies the system can run out of stack in
debug mode, while no problems arise when running in non-debug mode.]

Debug mode can be turned off again using nodebug/0, but then the spy
point will be ignored (but remembered). Turning debug mode back on via
debug/0 will hit the spy point again.

~~~
is_a(rock1, rock).
is_a(rock2, rock).
color(rock1, red).

noun(X, Type) :- is_a(X, Type).
adjective(X, color, Value) :- color(X, Value).
~~~

~~~
?-  spy(is_a/2).
% Spy point on is_a/2
true.

[debug]  ?- nodebug.
true.

?- noun(X, rock).
X = rock1 ;
X = rock2.

?- debug.
true.

[debug]  ?- noun(X, rock).
 * Call: (11) is_a(_47826, rock) ? creep
 * Exit: (11) is_a(rock1, rock) ? creep
   Exit: (10) noun(rock1, rock) ? creep
X = rock1 ;
 * Redo: (11) is_a(_47826, rock) ? creep
 * Exit: (11) is_a(rock2, rock) ? creep
   Exit: (10) noun(rock2, rock) ? creep
X = rock2.
~~~

So, debug mode allows Prolog to watch for spy points and enable trace
mode when it hits one. The tracing/0 and debugging/0 predicates will
report if the system is in either of those modes.

## Breakpoints {#trace-breakpoints}

Sometimes even spy points aren't enough. There may be a predicate that
is used in many different places and it would be helpful to turn on
tracing mode only when *one particular* call to it is made.
*Breakpoints* allow for turning on trace mode when a specific source
file, line number, and character in that line are hit. The predicates
used are set_breakpoint/4 and set_breakpoint/5. Many breakpoints can be
active at a time.

Note that the interface provided by these predicates is not intended
for end-users.  The built-in PceEmacs editor that is also embedded in
the graphical debugger allow setting break points based on the cursor
position.

Example.pl has now been modified to have multiple calls to ``noun/2``:

~~~
is_a(rock1, rock).
is_a(rock2, rock).
color(rock1, red).

noun(X, Type) :- is_a(X, Type).
adjective(X, color, Value) :- color(X, Value).
test_noun1(X, Type) :- noun(X, Type).
test_noun2(X, Type) :- noun(X, Type).
~~~

To enable tracing just when ``noun/2`` is called from ``test_noun2/2``,
`set_breakpoint/4` can be used like this:

~~~
?- set_breakpoint('/...path.../Example.pl', 8, 24, ID).
% Breakpoint 1 in 1-st clause of test_noun2/2 at Example.pl:8
ID = 1.

?- debug.
true.

[debug]  ?- noun(X, rock).
X = rock1 .

[debug]  ?- test_noun1(X, rock).
X = rock1 .

[debug]  ?- test_noun2(X, rock).
   Call: (11) noun(_44982, rock) ? creep
   Call: (12) is_a(_44982, rock) ? creep
   Exit: (12) is_a(rock1, rock) ? creep
   Exit: (11) noun(rock1, rock) ? creep
   Exit: (10) test_noun2(rock1, rock) ? creep
X = rock1 .

[trace]  ?- notrace.
true.

[debug]  ?-
~~~

The call to set_breakpoint/4 had to specify the source file
("Example.pl"), the line number (8), and the character within that line
(24) to precisely specify what clause should turn on trace mode (this is
much easier using the graphical debugger because it shows source code).

The breakpoint won't get triggered if the system isn't in debug mode
but, unlike setting a spy point, set_breakpoint/4 does *not* do this
automatically. So, it was turned on manually using debug/0.

The output shows that only the call to ``test_noun2/2`` (where the
breakpoint was set) actually turned on trace mode. Note that the
``[Trace] ?-`` at the end shows that trace mode is left on after being
triggered. It can be turned off again via notrace/0, which will leave
the system in debug mode. All debugging modes can be shut off at once by
calling nodebug/0 since shutting off debug mode automatically turns off
trace mode.

## Command Line Debugger Summary {#trace-summary}

In summary, there are really two distinct "tracing" features: trace
*mode* and trace *points*. Both write traces to the console using the
"Byrd Box Model" but that's where similarity ends.

### Trace Mode {#trace-summary-trace-mode}

Trace mode is the main Prolog command line debugger that allows for
tracing the transitions through the resolution states of predicates
represented by ports in the "Byrd Box Model" and optionally pausing for
a command when certain ports are hit.

It can be turned on manually via trace/0, or (when put into debug mode
using debug/0) when a specific predicate is encountered via spy/1, or
when a specific call to a predicate is encountered via set_breakpoint/4
or set_breakpoint/5.

When in trace mode, visible/1 controls which ports are written to the
console, and leash/1 controls which ports cause execution to pause to
allow program inspection.

When execution is paused, there are many commands that can be used to
inspect the state of the program, cause goals to fail or succeed, etc.

Trace mode is turned off via notrace/0 and debug mode is turned off via
nodebug/0.

### Trace Points {#trace-summary-trace-points}

Trace *points* are a separate feature from trace *mode* that allow
writing specified ports to the console when a predicate is being
evaluated. It does not ever pause program execution and does not need to
be in trace or debug mode to work.

They are turned on via trace/1 and trace/2.

They don't pay attention to visible/1 (because the ports shown are set
in trace/2) or leash/1 (because they don't pause execution).

They can be turned off via trace/2.
