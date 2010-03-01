
:- op(200, fy, [(?),(@)]).

/* We cannot handling this in SWI-Prolog
%% Basic ISO-modes
:- modedef '+'(A) : nonvar(A).
:- modedef '@'(A) + not_further_inst(A).
:- modedef '-'(A) : var(A).
:- modedef '?'(_).
:- modedef '*'(_).

:- use_module(engine(hiord_rt)).
:- push_prolog_flag(read_hiord,on).

%% Parametric versions of above
:- modedef +(A,X) :  X(A).
:- modedef @(A,X) :  X(A) => X(A) + not_further_inst(A).
:- modedef -(A,X) :  var(A) => X(A).
:- modedef ?(A,X) :: X(A) => X(A).
:- modedef *(A,X) :: X(A).

:- pop_prolog_flag(read_hiord).
*/

%% Version comment prompting control for this file.
%% Local Variables:
%% mode: CIAO
%% update-version-comments: "../version"
%% End:

