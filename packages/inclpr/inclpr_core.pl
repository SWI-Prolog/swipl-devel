/*  
    
    Part of INCLP(R)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, K.U. Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(inclpr_core,
	[
	    incremental/1,
	    change_incremental/1,
	    standard_domain/1,
	    change_standard_domain/1,
	    get_domain/2,
	    set_domain/2,
	    unify_trigger/1,
	    solve/0,
	    standard_domain/1,
	    {}/1
	]).

:- multifile prolog:message/3.

% toplevel printing

prolog:message(query(YesNo,Bindings)) --> !,
    { collect_output(Bindings,[],Output) },
    Output,
    '$messages':prolog_message(query(YesNo,Bindings)).

:- use_module(library(chr)).
:- use_module(inclpr_interval_arithmetic,
	[
	    accepted_solution_domain/1,
	    accepted_solution_domain_2/1,
	    subdivide/3,
	    interval_contains/2,
	    interval_intersection/3
	]).
:- use_module(inclpr_consistency,
	[
	    get_direct_full_evaluation/4,
	    check/8,
	    create_base_form/4,
	    create_partial_evaluation/5,
	    max_reduced/3
        ]).
:- use_module(inclpr_symbolic_processing,
        [
	    to_standard_form/2,
	    partial_derivative/3
	]).
:- use_module(inclpr_ordering,
        [
	    new/1,
	    get_next/2,
	    sv_try_remove/3,
	    sv_remove/3,
	    sv_remove_double/3,
	    sv_remove_number/2,
	    sv_merge/3,
	    sv_create/2
	]).
:- use_module(inclpr_inversion,
	[
	    all_occurrences/1,
	    invert/3
	]).

%%%

:- chr_type list(T) 	---> [] ; [T|list(T)].
:- chr_type ie		---> n.
:- chr_constraint
	% core

	% collect_output(Bindings,OutputIn,OutputOut)
	collect_output(?any,?any,?any),
	% constraint(Constraint,Probe)
	constraint(?any,+any),
	% prepared_constraint(Constraint,Varlist,Probe)
	prepared_constraint(?any,?any,+any),
	% new_constraint(Constraint,Varlist,Probe)
	new_constraint(?any,?any,+any),
	% bound_lower(Var,Bound)
	bound_lower(-any,+float),
	% bound_upper(Var,Bound)
	bound_upper(-any,+float),
	
	% variable scheduling

	% active_var(Phase,Var)
	active_var(+ie,-any),
	% queue(Phase,Varlist)
	queue(+ie,+list(any)), % !
	% inclusive_schedule_vars(Var)
	inclusive_schedule_vars(-any),
	% inclusive_schedule_vars(Phase,Var)
	inclusive_schedule_vars(+ie,-any),
	% exclusive_schedule_vars(Var)
	exclusive_schedule_vars(-any),
	% exclusive_schedule_vars(Phase,Var)
	exclusive_schedule_vars(+ie,-any),
	% schedule_var(Phase,Var)
	schedule_var(+ie,-any),
	% schedule_vars(Phase,Varlist)
	schedule_vars(+ie,+list(any)), %!
	% schedule_vars_excl(Phase,Varlist,Var)
	schedule_vars_excl(+ie,+list(any),-any), %!
	% schedule_constraint_vars(ID,IE)
	schedule_constraint_vars(+natural,+any),
	% schedule(Phase,Var)
	schedule(+ie,-any),
	% next_active_var(Phase)
	next_active_var(+ie),
	% get_active_var(Phase,Var)
	get_active_var(+ie,-any),
	% set_active_var(Phase,Var)
	set_active_var(+ie,-any),
		
	% trigger_list(Var,SortedVarlist)
	trigger_list(?any,?list(any)),
	% tl_connect_vars(Varlist,Varlist)
	tl_connect_vars(+list(any),?any), %!
	% tl_remove_doubles(Varlist,Var)
	tl_remove_doubles(+list(any),-any), %!
	% tl_remove_numbers(Varlist)
	tl_remove_numbers(+list(any)), %!
		
	% states
		
	% state_unify
	state_unify,
	% state_normal
	state_normal,
	% change_state(State)
	change_state(+any),
	% create_state
	create_state,
	% unify_tirgger(Var)
	unify_trigger(?any),
	% clp_core_trigger(Var)
	inclpr_core_trigger(?any),
	% unify_check_varlist_d_var(ID,IE,Var)
	unify_check_varlist_d_var(+natural,+ie,?any),
	% unify_check_varlist_d_number(ID,IE)
	unify_check_varlist_d_number(+natural,+ie),
		
	% phase scheduling
			
	% active_phase(Phase)
	active_phase(+ie),
	% phase_queue(Phaselist)
	phase_queue(+list(ie)),
	% get_phase(Phase)
	get_phase(?ie),
	% set_phase(Phase)
	set_phase(+ie),
	% next_phase
	next_phase,
	% schedule_phase(Phase)
	schedule_phase(+ie),
	% schedule_phases
	schedule_phases,
		
	% branch & prune

	% lower_domain(Var)
	lower_domain(-any),
	% upper_domain(Var)
	upper_domain(-any),
		
	% varlist(ID,IE,Varlist)
	varlist_f(+natural,+ie,?list(any)),
	% varlist_d(ID,IE,Var,Varlist)
	varlist_d(+natural,+ie,?any,?list(any)),
	
	% constraint IDs
		
	% constraint_function(ID,Function)
	constraint_function(+natural,?any),
	% new_constraint_id(ID)
	new_constraint_id(+natural),
	% max_constraint_id(ID)
	max_constraint_id(+natural),
	% constraint_relation(ID,Relation)
	constraint_relation(+natural,+any),
	
		
	%
	rem_cons(+natural,+any),
	%
	chk_sol/1,
	%
	check_id(+natural,+any,?any),
	%
	in_cons/1,
				
	% natural interval extension

	% n_i_e_create_projections(ID,Func,BaseForm,Varlist)
	n_i_e_create_projections(+natural,?any,?any,+list(any)), %!
	% n_i_e_prepare(ID,Func)
	n_i_e_prepare(+natural,?any),
	% n_i_e_p_c(ID,BaseForm,Var)
	n_i_e_p_c(+natural,?any,?any),
	% n_i_e_p_c_pd(ID,BaseForm,Var)
	n_i_e_p_c_pd(+natural,?any,?any),
		
	% inversion
		
	% inverted_constraint(ID,IE,BF,Var)
	inverted_constraint(+natural,+ie,?any,?any).
		
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_option(toplevel_show_store,off).

%%%

:- dynamic standard_domain/1.
:- dynamic incremental/1.
:- set_prolog_flag(float_format,'%.8f').

% States: the system is in unification state, in normal state or in no state.
% In unification state, the constraint store contains the state_unify/0
% constraint, in normal state, the constraint store contains the state_normal/0
% constraint. If neither are in the store, the system is in no state.
% change_state(State) changes the state to State, create_state sets the state
% to normal if the system is in no state and does nothing otherwise.

state_1 @ state_unify \ state_unify <=> true.
state_2 @ state_normal \ state_normal <=> true.
state_3 @ state_unify \ change_state(unify) <=> true.
state_4 @ change_state(unify), state_normal <=> state_unify.
state_5 @ state_normal \ change_state(normal) <=> true.
state_6 @ change_state(normal), state_unify <=> state_normal.
state_7 @ change_state(normal) <=> state_normal.
state_8 @ change_state(unify) <=> state_unify.
state_9 @ state_normal \ create_state <=> true.
state_10 @ state_unify \ create_state <=> true.
state_11 @ create_state <=> state_normal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  _______  _____   ______ _______ %%
%%  |       |     | |_____/ |______ %%
%%  |_____  |_____| |    \_ |______ %%
%%                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Unification: when unifying, 4 modules have their attribute unification hook
% called: inclpr_domain, inclpr_core, inclpr_aliases and inclpr_ordering. The
% unification handling rules in this module (rules which require the
% state_unify/0 constraint) requires that all work with respect to domains,
% variable aliases and ordering is done before the constraints are processed.
% Also, the constraint store should be internally consistent as far as the
% CHR implementation is concerned.
% For each module, we add a unify_trigger/1 constraint with as argument the
% result of unification (a variable or a number). When 4 such unify_trigger/1
% constraints are available, i.e. one per module, then all necessary checks are
% done and we can proceed to the unification handling rules. The argument of
% unify_trigger/1 is used to determine whether variables have to be scheduled.  

% Unification hook for the inclpr_domain attribute:
% If unifying a variable with a number, it is checked whether the domain of the
% variable contains that number. If unifying two variables, the domains of the
% variables are intersected.

inclpr_domain:attr_unify_hook(Domain,Y) :-
	(   number(Y)
	->  interval_contains(Domain,Y)
	;   get_attr(Y,inclpr_domain,YDomain)
	->  interval_intersection(Domain,YDomain,NewDomain),
	    put_attr(Y,inclpr_domain,NewDomain)
	;   put_attr(Y,inclpr_domain,Domain)
	),
	unify_trigger(Y).

% These rules are the only ones of inclpr_core that can be activated by
% unification. All other require the presence of the state_unify/0 constraint.
% The rules cause a unify_trigger/1 constraint to be added, denoting that the
% CHR constraint store for this module is consistent with respect to the CHR
% implementation.
% The inclpr_core_trigger/1 constraints are used to detect unification: one
% such constraint is added for each new variable and the rules can only fire
% after unification, either with another variable, or with a number.

unify_trigger_1 @ inclpr_core_trigger(V) \ inclpr_core_trigger(V) <=> 
	unify_trigger(V).
unify_trigger_2 @ inclpr_core_trigger(N) <=> 
	number(N) |
	unify_trigger(N).

% This rule collects the 4 unify_trigger/1 constraints and puts the system in
% unification state. It also schedules variables for consistency checks if
% unification of two variables occurred.

unify_trigger_3 @ unify_trigger(X), unify_trigger(X), unify_trigger(X),
    unify_trigger(X) <=> 
	true |
	change_state(unify),
	(   var(X)
	->  inclusive_schedule_vars(X)
	;   true
	),
	state_normal.

% get_domain(Variable,Domain)
%
% Returns in <Domain> the interval representing the current domain of variable
% <Variable>.

get_domain(V,D) :- get_attr(V,inclpr_domain,D).

% set_domain(Variable,Domain)
%
% Sets the current domain of variable <Variable> to the interval <Domain>.

set_domain(V,D) :- put_attr(V,inclpr_domain,D).

% Unification hook for the inclpr_aliases attribute:
% If unifying a variable with a number, both the center alias and the domain
% alias are unified with a canonical interval for that number. If unifying two
% variables, the aliases of both variables are unified.

inclpr_aliases:attr_unify_hook(cd(C,D),Y) :-
	(   number(Y)
	->  C = i(Y,Y),
	    D = i(Y,Y)
	;   get_attr(Y,inclpr_aliases,cd(C,D))
	),
	unify_trigger(Y).

% create_aliases(Variable)
%
% Creates variable aliases for the center and domain of variable <Variable>.
% These aliases are used so that we can differentiate between variable
% occurrences that have to be replaced by the domain, and occurrences that have
% to be replaced by the center of the domain (for center-value based interval
% extension). It also creates a inclpr_core_trigger/1 constraint for
% unification detection. Does nothing if the variable already has aliases.

create_aliases(X) :- 
	(   get_attr(X,inclpr_aliases,_)
	->  true
	;   put_attr(X,inclpr_aliases,cd(_,_)),
	    inclpr_core_trigger(X)
	).

% create_domain(Variable)
%
% Gives the variable <Variable> a domain attribute, instantiated with the
% standard domain and creates an empty trigger_list/2 constraint for the
% variable. Trigger lists are used to activate variables that are connected via
% constraints when the domain of one of them has changed. If the variable
% already has a domain, nothing is done.

create_domain(X) :-
	(   get_attr(X,inclpr_domain,_)
	->  true
	;   trigger_list(X,[]),
	    standard_domain(i(L1,U1)),
	    L is L1,
	    U is U1,
	    set_domain(X,i(L,U))
	).

% get_aliases(Variable,Aliases)
%
% Returns in <Aliases> a term cd(C,D) with <C> the variable alias for the
% center of the domain of <Variable> and <D> the variable alias for the domain
% of <Variable>.

get_aliases(X,Vid) :- get_attr(X,inclpr_aliases,Vid).

% create_new_var(Variable)
%
% Prepares a new variable by adding it to the ordering, giving it aliases and
% giving it an initial domain.

create_new_var(X) :- 
	new(X),
	create_aliases(X),
	create_domain(X).

% These rules generate a new unique identifier for a new constraint. Constraint
% identifiers are used to speedup matching constraints.

constraint_id_1 @ new_constraint_id(ID), max_constraint_id(IDMax) <=> 
	ID is IDMax + 1,
	max_constraint_id(ID).
constraint_id_2 @ new_constraint_id(ID) <=> 
	ID = 0,
	max_constraint_id(0).
	
%%%%%%%%%%%%%%%%%
%               %
% Var Scheduler %
%               %
%%%%%%%%%%%%%%%%%

% We use the concept of an active variable to denote the variable that is used
% in consistency checks and domain narrowing. Variables are scheduled in a
% queue and when an active variable has been processed, the next variable in
% the queue is used as the new active variable. There can be multiple queues to
% allow constraint processing in phases. We currently have a one-to-one
% relationship between phases and interval extensions.

% inclusive_schedule_vars(Var)
%
% Schedules variable <Var> and all its connected variables for consistency
% checks for all interval extensions. 

inclusive_schedule_1 @ inclusive_schedule_vars(Var) <=>
	inclusive_schedule_vars(n,Var).

% inclusive_schedule_vars(Phase,Var)
%
% Schedules variable <Var> and all its connected variables for consistency
% checks for the interval extension denoted by <Phase>.

inclusive_schedule_2 @ inclusive_schedule_vars(Phase,Var) <=>
	schedule_var(Phase,Var),
	exclusive_schedule_vars(Phase,Var).

% exclusive_schedule_vars(Var)
%
% Schedules the variables connected to <Var> for consistency checks for all
% interval extensions.

exclusive_schedule_1 @ exclusive_schedule_vars(Var) <=>
	exclusive_schedule_vars(n,Var).

% exclusive_schedule_vars(Phase,Var)
%
% Schedules the variables connected to <Var> for consistency checks for the
% interval extension denoted by <Phase>.

exclusive_schedule_2 @ exclusive_schedule_vars(n,V),
    n_i_e_p_c(ID,_,V) #P1, varlist_f(ID,n,Varlist) #P2 ==>
	schedule_vars_excl(n,Varlist,V) pragma passive(P1), passive(P2).
exclusive_schedule_3 @ exclusive_schedule_vars(n,V),
    inverted_constraint(ID,n,_,V) #P1, varlist_f(ID,n,Varlist) #P2 ==>
	schedule_vars_excl(n,Varlist,V) pragma passive(P1), passive(P2).

exclusive_schedule_6 @ exclusive_schedule_vars(_,_) <=> true.

% schedule_constraint_vars(ID,IE)
%
% Schedules all the variables in the constraint with identifier <ID> for
% consistency checks for the interval extension denoted by <IE>.

schedule_constraint_1 @ schedule_constraint_vars(ID,IE),
    varlist_f(ID,IE,Varlist) #P1 ==>
	schedule_vars(IE,Varlist) pragma passive(P1).
schedule_constraint_2 @ schedule_constraint_vars(_,_) <=> true.

% mem_app(List,Variable,NewList)
%
% Adds <Variable> at the end of <List> if it does not appear in <List> and
% returns the extended list in <NewList>. If <Variable> is a member of <List>,
% <NewList> = <List>.

mem_app([],X,[X]).
mem_app([H|T],X,[H|T2]) :- 
	(   X == H
	->  T = T2
	;   mem_app(T,X,T2)
	).

% queue/2 constraints contain a queue of variables that are scheduled for
% consistency checks for a given interval extension. 

% Rule representing an explicit functional dependency: one queue per interval
% extension.

queue_1 @ queue(Phase,_) \ queue(Phase,_) <=> true.

% schedule_var(IE,Variable)
%
% Schedules variable <Variable> for consistency checks for the interval
% extension denoted by <IE>.

queue_2 @ schedule_var(Phase,Var), queue(Phase,Q) #P <=>
	true |
	mem_app(Q,Var,NewQ),
	queue(Phase,NewQ),
	(   \+ get_active_var(Phase,_)
	->  next_active_var(Phase)
	;   true
	)
	pragma passive(P).
queue_3 @ schedule_var(Phase,Var) <=>
	queue(Phase,[]),
	schedule_var(Phase,Var).

% schedule_vars(IE,Variables)
%
% Schedules the variables in list <Variables> for consistency checks for the
% interval extension denoted by <IE>.

queue_4 @ schedule_vars(Phase,[H|T]) <=>
	schedule_var(Phase,H),
	schedule_vars(Phase,T).
queue_5 @ schedule_vars(_,[]) <=> true.

% schedule_vars_excl(IE,Variables,Variable)
%
% Schedules the variables in list <Variables> for consistency checks for the
% interval extension denoted by <IE> except for variable <Variable> which is
% not scheduled (even if it is in <Variables>).

schedule_vars_excl(Phase,[H|T],V) <=>
	(   H \== V
	->  schedule_var(Phase,H)
	;   true
	),
	schedule_vars_excl(Phase,T,V).
schedule_vars_excl(_,[],_) <=> true.

% next_active_var(IE)
%
% Sets the next variable in the queue of interval extension <IE> as the active
% variable for <IE>.

queue_6 @ next_active_var(Phase), queue(Phase,[H|T]) #P <=>
	queue(Phase,T),
	set_active_var(Phase,H)
	pragma passive(P).
queue_7 @ next_active_var(Phase), active_var(Phase,_) #P <=>
	true pragma passive(P).
queue_8 @ next_active_var(_) <=> true.

% get_active_var(IE,Variable)
%
% Returns in <Variable> the active variable for interval extension <IE>.

active_var(Phase,AV) #P \ get_active_var(Phase,GAV) <=>
	AV = GAV pragma passive(P).
get_active_var(_,_) <=> fail.

% set_active_var(IE,Variable)
%
% Sets <Variable> as the active variable for interval extension <IE>. This
% makes the current active variable passive if such exists.

set_active_var(Phase,SAV), active_var(Phase,_) #P <=>
	active_var(Phase,SAV),
	schedule_phase(Phase) pragma passive(P).
set_active_var(Phase,SAV) <=>
	active_var(Phase,SAV),
	schedule_phase(Phase).

% collect_output(Bindings,Temp,Output)
%
% Output processing for toplevel printing: <Bindings> is a list of elements
% <Name> = <Variable> where <Name> is a toplevel variable name and <Variable>
% is a variable. If <Variable> is an INCLPR variable, then it is output as
% `{<L> =< <Name> =< <U>}' with <L> its lower bound and <U> its upper bound.
% Other variables are output as `<Name> = <Variable>'. <Temp> contains the
% output for the bindings processed thusfar and is eventually unified with
% <Output>.

collect_output([],Output,FinalOutput) <=> Output = FinalOutput.
collect_output([X=Y|T],Output,FinalOutput) <=>
	get_domain(Y,i(L,U)) |
	collect_output(T,['{~w =< ~w =< ~w}'-[L,X,U],nl|Output],FinalOutput).
collect_output([X=Y|T],Output,FinalOutput) <=>
	collect_output(T,['{~w = ~w}'-[X,Y],nl|Output],FinalOutput).

%%%%%%%%%%%%%%%%%%%
%                 %
% Phase Scheduler %
%                 %
%%%%%%%%%%%%%%%%%%%

% Phases denote different constraint processing steps. For each phase there is
% a queue of variables to be scheduled. The phase_queue/1 constraint contains
% a list of phases. For the active phase, the variable queue is processed. When
% this is done, the next phase in the phase queue becomes the active phase.

% Explicit singleton declaration

phase_queue(_) \ phase_queue(_) <=> true.

% next_phase
%
% Sets the next phase in the phase queue as the active phase, making the
% current active phase passive if such exists. If the phase queue is empty, no
% phase is active.

next_phase, phase_queue([H|T]) #P <=>
	phase_queue(T),
	set_phase(H) pragma passive(P).
next_phase, active_phase(_) #P <=>
	true pragma passive(P).
next_phase <=> true.

% schedule_phase(Phase)
%
% Schedules <Phase> by adding it to the end of the phase queue if it is not
% already there and making it the active phase if no phase is currently active.

schedule_phase(SP), phase_queue(PQ) #P <=>
	true |
	mem_app(PQ,SP,NewPQ),
	phase_queue(NewPQ),
	(   \+ get_phase(_)
	->  next_phase
	;   true
	)
	pragma passive(P).
schedule_phase(SP) <=>
	phase_queue([]),
	schedule_phase(SP).

% schedule_phases
%
% Schedules all supported phases. Currently this is only the phase
% corresponding to using the natural interval extension.

schedule_phases <=>
	schedule_phase(n).

% get_active_phase(Phase)
%
% Returns in <Phase> the current active phase.

active_phase(AP) #P \ get_phase(GP) <=>
	GP = AP pragma passive(P).
get_phase(_) <=> fail. 

% set_phase(Phase)
%
% Sets <Phase> as the current active phase. If another phase was active, this
% phase becomes passive.

set_phase(SP), active_phase(_) #P <=>
	active_phase(SP) pragma passive(P).
set_phase(SP) <=> active_phase(SP).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        %
% Constraint Preparation %
%                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% {Constraints}
%
% Entry point for new constraints. <Constraints> is either a constraint or a
% conjunction of constraints.

{C} :- 
	incremental(Inc),
	cns(C,Inc),
	(   Inc == false
	->  create_state
	;   true
	).

% cns(Constraints,Incremental)
%
% Similar to {}/1 but used to differentiate between incremental constraint
% processing and processing in block. <Incremental> denotes the constraint
% processing mode and is either `true' or `false'.

cns((Cons1,Cons2),Inc) :-
	!,
	cns(Cons1,Inc),
	cns(Cons2,Inc).
cns(Cons,Inc) :-
	!,
	constraint(Cons,p),
	(   Inc == true
	->  create_state
	;   true
	).
% new_constraint(Constraint,Variables,Probe)
%
% Prepares the processing of the constraint <Constraint> without changing the
% system to normal state. (see state_normal/0 and state_unify/0). <Probe> is
% currently not used. The constraint variables are listed in <Variables> and 
% have been processed as new variables by create_new_var/1.

new_constraint(X=Y,Vars,Prb) <=>
	true |
	(   var(X)
	->  (   var(Y)
	    ->  X = Y % rescheduling via unification checks
	    ;   number(Y)
	    ->  X is Y % rescheduling via unification checks
	    ;   prepared_constraint(X=Y,Vars,Prb)
	    )
	;   number(X)
	->  (   var(Y)
	    ->  Y is X % rescheduling via unification checks
    	    ;   number(Y)
	    ->  X =:= Y
	    ;   prepared_constraint(X=Y,Vars,Prb)
	    )
	;   prepared_constraint(X=Y,Vars,Prb)
	).
new_constraint(X>=Y,Vars,Prb) <=>
	true |
	(   var(X),
	    number(Y)
	->  bound_lower(X,Y),
	    inclusive_schedule_vars(X)
	;   number(X),
	    var(Y)
	->  bound_upper(Y,X),
	    inclusive_schedule_vars(Y)
	;   number(X),
	    number(Y)
	->  X >= Y
	;   prepared_constraint(X>=Y,Vars,Prb)
	).
new_constraint(X=<Y,Vars,Prb) <=>
	true |
	(   var(X),
	    number(Y)
	->  bound_upper(X,Y),
	    inclusive_schedule_vars(X)
	;   number(X),
	    var(Y)
	->  bound_lower(Y,X),
	    inclusive_schedule_vars(Y)
	;   number(X),
	    number(Y)
	->  X =< Y
	;   prepared_constraint(X=<Y,Vars,Prb)
	).

% constraint(Constraint,Probe)
%
% Prepares the processing of the constraint <Constraint> without changing the
% system to normal state. (see state_normal/0 and state_unify/0). <Probe> is
% currently not used. The constraint variables are processed as new variables
% by create_new_var/1.

constraint(Cons,Prb) <=>
	term_variables(Cons,Vars),
	maplist(create_new_var,Vars),
	new_constraint(Cons,Vars,Prb).

% standard_domain(Domain)
%
% Dynamic predicate denoting the domain to be initially used by new variables.
% Can be changed by change_standard_domain/1.

standard_domain(i(-100.0,100.0)).

% incremental(Boolean)
%
% Dynamic predicate denoting whether constraints in one call of {}/1 are to
% be processed incrementally (i.e. after each constraint is read, consistency
% checks and narrowing is performed) or in blocks (i.e. consistency checks and
% narrowing are performed only after all constraints have been read). Can be
% changed by change_incremental/1. <Boolean> is either `true' or `false'.

incremental(true).

% change_incremental(Boolean)
%
% Changes the way constraints in one call of {}/1 are processed: incremental
% or in blocks. See incremental/1.

change_incremental(X) :-
	retract(incremental(_)),
	assert(incremental(X)).

% change_standard_domain(Domain)
%
% Changes the initial domain of new variables. See standard_domain/1.

change_standard_domain(X) :-
	retract(standard_domain(_)),
	assert(standard_domain(X)).

% prepared_constraint(Constraint,Variables,Probe)
%
% Prepares the creation of interval extensions for the constraint <Constraint>
% in variables <Variables>. This is only called for nontrivial constraints.
% <Probe> is currently not used.

prepared_constraint(Cons,_,_) <=>
	true |
	to_standard_form(Cons,Std),
	(   term_variables(Std,[])
	->  call(Std)
	;   functor(Std,R,2),
	    arg(1,Std,F),
	    new_constraint_id(ID),
	    constraint_function(ID,F),
	    constraint_relation(ID,R),
	    n_i_e_prepare(ID,F)
	).

%%%%%%%%%%%%%%%%%%%%%%
%                    %
% Unification Checks %
%                    %
%%%%%%%%%%%%%%%%%%%%%%

% Trigger lists: the trigger_list/2 constraints relate a variable to a list of
% variables that is connected to it via constraints. This list is an ordered
% list of variables, making some operations like merging lists cheaper. The
% lists are used for variable scheduling.

% Unification rules for trigger lists:
% Rule unify_trigger_list_1 works for the unification of two variables: the two
% 	related lists are merged and double occurrences of the unified
%	variables are removed in other lists.
% Rule unify_trigger_list_2 works for the unification of a variable with a
% 	number: the trigger list for the unified variable is removed and the
%	number is removed in other lists. 

unify_trigger_list_1 @ state_unify \ trigger_list(V,L1) #P1,
    trigger_list(V,L2) #P2 <=>
	sv_try_remove(L1,V,NewL1),
	sv_try_remove(L2,V,NewL2),
	sv_merge(NewL1,NewL2,L3),
	trigger_list(V,L3),
	tl_remove_doubles(L3,V)
	pragma passive(P1), passive(P2).
unify_trigger_list_2 @ state_unify \ trigger_list(N,L) #P1 <=>
	number(N) |
	tl_remove_numbers(L)
	pragma passive(P1).

% using trigger lists for variable scheduling
    
trigger_list(Var,List) #P1 \ schedule(Phase,Var) <=>
	schedule_vars(Phase,List)
	pragma passive(P1). 

% tl_remove_doubles(Variables,Variable)
%
% Removes double occurrences of variable <Variable> from the trigger lists of
% the variables in <Variables>

tl_remove_doubles_1 @ tl_remove_doubles([],_) <=> true.
tl_remove_doubles_2 @ tl_remove_doubles([H|T],Var), trigger_list(H,L) #P1 <=>
	sv_remove_double(L,Var,NewL),
	trigger_list(H,NewL),
	tl_remove_doubles(T,Var)
	pragma passive(P1).

% tl_remove_numbers(Variables)
%
% Removes a number from the trigger lists of the variables in <Variables>. Used
% by unification handling, so each list may contain up to one variable.

tl_remove_numbers_1 @ tl_remove_numbers([]) <=> true.
tl_remove_numbers_2 @ tl_remove_numbers([H|T]), trigger_list(H,L) #P1 <=>
	sv_remove_number(L,NewL),
	trigger_list(H,NewL),
	tl_remove_numbers(T)
	pragma passive(P1).

% tl_connect_vars(Variables)
%
% Connect the variables in <Variables> with eachother by adding all other
% variables to the trigger list of each variable.

tl_connect_vars(Varlist) :- 
	sv_create(Varlist,SortedVarlist),
	tl_connect_vars(SortedVarlist,SortedVarlist).

% tl_connect_vars(List,Variables)
%
% Adds the variables in <Variables> to each variable <Variable> in <List>
% except for variable <Variable>. Initially, <List> and <Variables> are equal,
% but <List> is consumed and <Variables> remains. 

tl_connect_1 @ tl_connect_vars([],_) <=> true.
tl_connect_2 @ tl_connect_vars([H|T],L1), trigger_list(H,L2) #P1 <=>
	sv_remove(L1,H,L3),
	sv_merge(L3,L2,L4),
	trigger_list(H,L4),
	tl_connect_vars(T,L1)
	pragma passive(P1).

% Unification handling for the natural interval extension: 
%
% - Variable list of the constraint function has to be updated (duplicates or
%	number removed)
% - Variable lists of the derivatives have to be updated (duplicates of number
%	removed)
% - Unification of two variables: merging of partial derivatives
% - Unification with a number: removing partial derivative
% - Unification with a number: schedule consistency checks for involved
%	constraints
% - Inverted constraints and unification of two variables: create projection
%	constraints

unify_n_i_e_var @ state_unify, n_i_e_p_c(ID,_,V) #passive \ 
    n_i_e_p_c(ID,_,V) #passive, varlist_f(ID,n,BFV) #passive <=>
	term_variables(BFV,NewBFV),
	varlist_f(ID,n,NewBFV),
	unify_check_varlist_d_var(ID,n,V).
unify_n_i_e_number @ state_unify \ n_i_e_p_c(ID,_,N) #passive,
    varlist_f(ID,n,BFV) #passive <=>
	number(N) | 
	term_variables(BFV,NewBFV),
	varlist_f(ID,n,NewBFV),
	unify_check_varlist_d_number(ID,n),
	schedule_constraint_vars(ID,n).

% Updating variable lists of the derivatives: unification of two variables

unify_n_i_e_d_var_1 @ unify_check_varlist_d_var(ID,n,V) \
    varlist_d(ID,n,V,Varlist1) #passive,
    varlist_d(ID,n,V,Varlist2) #passive <=>
	term_variables(Varlist1+Varlist2,NewVarlist),
	varlist_d(ID,n,V,NewVarlist).
unify_n_i_e_d_var_2 @ unify_check_varlist_d_var(ID,n,V1) \
    varlist_d(ID,n,V2,Varlist) #passive <=>
	V1 \== V2 |
	term_variables(Varlist,NewVarlist),
	varlist_d(ID,n,V2,NewVarlist).
unify_n_i_e_d_var_3 @ unify_check_varlist_d_var(_,_,_) <=> true.

% Updating variable lists of the derivatives: unification with a number

unify_n_i_e_d_num_1 @ unify_check_varlist_d_number(ID,n) \
    varlist_d(ID,n,V,_) #passive <=>
	number(V) | true.
unify_n_i_e_d_num_2 @ unify_check_varlist_d_number(ID,n) \
    varlist_d(ID,n,V,Varlist) #passive <=>
	var(V) |
	term_variables(Varlist,NewVarlist),
	varlist_d(ID,n,V,NewVarlist).
unify_n_i_e_d_num_3 @ unify_check_varlist_d_number(_,_) <=> true.

% Merging partial derivatives (var=var) / deleting partial derivative (number)

unify_n_i_e_pd_var @ state_unify \ n_i_e_p_c_pd(ID,DBF1,V) #passive,
    n_i_e_p_c_pd(ID,DBF2,V) #passive <=> 
	n_i_e_p_c_pd(ID,(DBF1)+(DBF2),V).
unify_n_i_e_pd_num @ state_unify \ n_i_e_p_c_pd(_,_,N) #passive <=>
	number(N) | true.

% Inverted constraints: F = 0 -> X = Fx and Y = Fy and X = Y
% After unification, the number of occurrences of the unified variable has
% increased from 1 to 2. 
unify_n_i_e_inv_inv @ state_unify \ inverted_constraint(ID,n,_,V) #passive,
    inverted_constraint(ID,n,_,V) #passive, varlist_f(ID,n,_) #passive,
    constraint_function(ID,F) #passive <=>
	create_base_form(n,F,BF,BFV),
	varlist_f(ID,n,BFV),
	n_i_e_p_c(ID,BF,V),
	partial_derivative(F,V,PD),
	create_base_form(n,PD,DBF,DBFV),
	varlist_d(ID,n,V,DBFV),
	n_i_e_p_c_pd(ID,DBF,V).
unify_n_i_e_inv_num @ state_unify \ inverted_constraint(ID,n,_,N) #passive,
    varlist_f(ID,n,Varlist) #passive <=>
	number(N) |
	term_variables(Varlist,NewVarlist),
	varlist_f(ID,n,NewVarlist),
	schedule_constraint_vars(ID,n).
% mixed forms

% Projection and inversion: remove inversion and update pd
unify_n_i_e_inv_proj @ state_unify, n_i_e_p_c(ID,_,V) #passive \
    n_i_e_p_c_pd(ID,_,V) #passive, inverted_constraint(ID,n,_,V) #passive,
    varlist_f(ID,n,BFV) #passive, varlist_d(ID,n,V,_) #passive,
    constraint_function(ID,F) #passive <=>
	term_variables(BFV,NewBFV),
	varlist_f(ID,n,NewBFV),
	partial_derivative(F,V,PD),
	create_base_form(n,PD,DBF,DBFV),
	unify_check_varlist_d_var(ID,n,V),
	varlist_d(ID,n,V,DBFV),
	n_i_e_p_c_pd(ID,DBF,V).

state_unify <=> true.


%%%%%%%%%%%%%%%%%%%%
%                  %
% Variable Domains %
%                  %
%%%%%%%%%%%%%%%%%%%%

% bound_upper(Variable,Bound)
%
% Reflects the effects of adding a constraint <Variable> =< <Bound> on the
% inclpr_domain attribute.

bound_upper(Var,Upper) <=>
	true |
	get_domain(Var,i(L,U)),
	(   Upper >= U
	->  true
	;   Upper =:= L
	->  Var = L
	;   Upper < U,
	    Upper > L
	->  set_domain(Var,i(L,Upper))
	).

% bound_lower(Variable,Bound)
%
% Reflects the effects of adding a constraint <Variable> >= <Bound> on the
% inclpr_domain attribute.

bound_lower(Var,Lower) <=>
	true |
	get_domain(Var,i(L,U)),
	(   Lower =< L
	->  true
	;   Lower =:= U
	->  Var = U
	;   Lower > L,
	    Lower < U
	->  set_domain(Var,i(Lower,U))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  _______  _____         _    _ _____ __   _  ______ %%
%%  |______ |     | |       \  /    |   | \  | |  ____ %%
%%  ______| |_____| |_____   \/   __|__ |  \_| |_____| %%
%%                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve
%
% Uses a branch and prune algorithm to find quasi-point solutions. These
% solutions have a domain of very small (relative) width for all variables.

solve :-
	chk_sol(Fail),
	(   var(Fail)
	->  true
	;   get_next(bottom,First),
	    branch(First,_)
	).

% branch(Variable,Loop)
%
% If the variable <Variable> has a domain of acceptable small width, then the
% next variable according to variable ordering is recursively checked.
% Otherwise the domain of the variable is split in two halves and the lower
% half is chosen as the new domain for <Variable>. On backtracking, the upper
% half is tried. The variable <Loop> is instantiated to `false' when splitting
% is done. It is used to prevent infinite looping if all domains satisfy the
% precision criterion. 

branch(Var,Loop) :-
	(   var(Var)
	->  get_next(Var,Next),
	    (   in_cons(Var),
		get_domain(Var,Domain),
		\+ accepted_solution_domain_2(Domain)
	    ->  Loop = false,	
		(   lower_domain(Var),
		    create_state,
		    chk_sol(Fail),
		    (   var(Fail)
		    ->	true
		    ;	branch(Next,Loop)
		    )
		;   upper_domain(Var),
		    create_state,
		    chk_sol(Fail),
		    (   var(Fail)
		    ->	true
		    ;   branch(Next,Loop)
		    )
		)
	    ;	branch(Next,Loop)
	    )
	;   Var == top
	->  (   var(Loop)
	    ->	true
	    ;	get_next(bottom,First),
		branch(First,_)
	    )
	).

% lower_domain(Variable)
%
% Replaces the domain of variable <Variable> by its lower half and schedules
% the variable and its connected variables for consistency checks.

lower_domain @ lower_domain(Var) <=>
	get_domain(Var,Domain),
	subdivide(lower,Domain,NewDomain) |
	set_domain(Var,NewDomain),
	inclusive_schedule_vars(Var),
	schedule_phases.

% upper_domain(Variable)
%
% Replaces the domain of variable <Variable> by its upper half and schedules
% the variable and its connected variables for consistency checks.

upper_domain @ upper_domain(Var) <=>
	get_domain(Var,Domain),
	subdivide(upper,Domain,NewDomain) |
	set_domain(Var,NewDomain),
	inclusive_schedule_vars(Var),
	schedule_phases.

% chk_sol(Fail)
%
% Checks whether we have reached a solution because the constraints cannot
% cause more domain narrowing. If this is the case, <Fail> will be variable,
% otherwise <Fail> will be unified with the atom `fail'. Constraints that
% cannot cause more domain narrowing are removed from the constraint store.

chk_sol(Fail), constraint_function(ID,_) ==> 
	check_id(ID,n,Fail).
chk_sol(_) <=> true.

% in_cons(Variable)
%
% Check whether the variable <Variable> belongs to a constraint. This might
% change by using chk_sol/1.

n_i_e_p_c(_,_,V) \ in_cons(V) <=> true.
in_cons(_) <=> fail. 

% check_id(ID,IE,Fail)
%
% Checks whether the constraint with identifier <ID> can still cause domain
% reductions with respect to interval extension <IE>. This is the case when the
% range of the constraint function for the given interval extension and the
% current domains of the constraint variables is of large enough relative
% width. If it is the case, variable <Fail> remains a variable and the
% constraint is removed from the constraint store. Otherwise, <Fail> is unified
% with the atom `fail'.

n_i_e_p_c(ID,BF,_), varlist_f(ID,n,BFV) \ check_id(ID,n,Fail) <=>
	    true |
	    (   max_reduced(n,BF,BFV)
	    ->	rem_cons(ID,n)
	    ;	Fail = fail
	    ).
check_id(_,_,_) <=> true.
	    
% rem_cons(ID,IE)
%
% Removes the CHR constraints related to the constraint with identifier <ID>
% for interval extension <IE>.

rem_cons(ID,n) \ n_i_e_p_c(ID,_,_) <=> true.
rem_cons(ID,n) \ n_i_e_p_c_pd(ID,_,_) <=> true.
rem_cons(ID,n) \ varlist_f(ID,n,_) <=> true.
rem_cons(ID,n) \ varlist_d(ID,n,_,_) <=> true.

rem_cons(_,_) <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% _____ __   _ _______ _______  ______ _    _ _______                         %%
%%   |   | \  |    |    |______ |_____/  \  /  |_____| |                       %%
%% __|__ |  \_|    |    |______ |    \_   \/   |     | |_____                  %%
%%                                                                             %%
%% _______ _     _ _______ _______ __   _ _______ _____  _____  __   _ _______ %%
%% |______  \___/     |    |______ | \  | |______   |   |     | | \  | |______ %%
%% |______ _/   \_    |    |______ |  \_| ______| __|__ |_____| |  \_| ______| %%
%%                                                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            %
% Natural Interval Extension %
%                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rules for performing consistency checks/domain narrowing:
% performed with respect to an active variable and active phase, only in
% normal state

% inverted constraints (are equality constraints)
check_inverted @ state_normal, active_phase(n), active_var(n,V),
    inverted_constraint(ID,n,BF,V) #passive, varlist_f(ID,n,BFV) #passive ==>
	true |
	%writeln(ID:V),
	get_direct_full_evaluation(n,BF,BFV,PE),
	get_domain(V,Dom),
	interval_intersection(Dom,PE,NewDom),
	(   Dom == NewDom
	->  true
	;   set_domain(V,NewDom),
	    schedule(n,V)
	).

% projection constraint: we need projection and partial derivative and
% relation (=<, = or >=)
check_n_i_e @ state_normal, active_phase(n), active_var(n,V),
    n_i_e_p_c(ID,BF,V) #passive, varlist_f(ID,n,BFV) #passive, 
    n_i_e_p_c_pd(ID,DBF,V) #passive, varlist_d(ID,n,V,DBFV) #passive,
    constraint_relation(ID,R) #passive ==>
	true |
	(   create_partial_evaluation(n,BF,BFV,PE,V),
	    create_partial_evaluation(n,DBF,DBFV,DPE,V),
	    get_domain(V,I1),
	    check(n,PE,DPE,R,V,I1,lower,I2),
	    check(n,PE,DPE,R,V,I2,upper,I3),
	    (   I1 \== I3
	    ->  nb_setval(inclpr_result,sd(I3))
	    ;	nb_setval(inclpr_result,nothing)
	    ),
	    fail
	;   catch(nb_getval(inclpr_result,Result),_,fail),
	    nb_delete(inclpr_result),
	    (   Result = sd(Domain)
	    ->  set_domain(V,Domain),    
		schedule(n,V)
	    ;   true
	    )
	).
	    
% n_i_e_prepare(ID,Function)
%
% Prepares the projection constraints (>1 occurrence of a variable) and
% inverted constraints (1 occurrence of a variable) for the natural interval
% extension fo the constraint with function <Function> and identifier <ID>.

prepare_n_i_e_1 @ n_i_e_prepare(ID,F) <=>
	schedule_phase(n),
	create_base_form(n,F,BF,BFV),
	varlist_f(ID,n,BFV),
	tl_connect_vars(BFV),
	all_occurrences(F),
	n_i_e_create_projections(ID,F,BF,BFV).

% n_i_e_create_projections(ID,Function,BaseForm,Variables)
%
% Creates a projection constraint or inverted constraint (depending on the
% number of occurrences of the variable at hand) for each variable for the
% constraint with identifier <ID>, constraint function <Function> and natural
% interval extension base form <BaseForm>.

prepare_n_i_e_2 @ constraint_relation(ID,R) \
    n_i_e_create_projections(ID,F,BF,[H|T]) <=>
	true |
	schedule_var(n,H),
	(   R == (=),
	    get_attr(H,inclpr_occurrence_count,one(_))
	->  invert(F,H,Inv),
	    create_base_form(n,Inv,InvBF,_),
	    inverted_constraint(ID,n,InvBF,H)
	;   n_i_e_p_c(ID,BF,H),
	    partial_derivative(F,H,PD),
	    create_base_form(n,PD,DBF,DBFV),
	    varlist_d(ID,n,H,DBFV),
	    n_i_e_p_c_pd(ID,DBF,H)
	),
	del_attr(H,inclpr_occurrence_count),
	n_i_e_create_projections(ID,F,BF,T).
prepare_n_i_e_3 @ n_i_e_create_projections(_,_,_,[]) <=> true.

% next active var, next phase and exiting normal state.

state_normal, active_phase(AP) \ active_var(AP,_) <=> next_active_var(AP).
state_normal \ active_phase(_) <=> next_phase.
state_normal <=> true.

% explicit functional dependencies
varlist_f(ID,P,_) \ varlist_f(ID,P,_) <=> true.
constraint_function(ID,_) \ constraint_function(ID,_) <=> true.
constraint_relation(ID,_) \ constraint_relation(ID,_) <=> true.
