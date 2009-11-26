/*  Part of SWI-Prolog

    Author:        Nicos Angelopoulos
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, Nicos Angelopoulos
*/

:- module( r_session,
          [
               r_open/0, r_open/1,
               r_close/0, r_close/1,
               r_in/1, r_in/2,
               r_push/1, r_push/2,
               r_out/2, r_out/3,
               r_err/3, r_err/4,
               r_print/1, r_print/2,
               r_lines_print/1, r_lines_print/2, r_lines_print/3,
               r_lib/1, r_lib/2,
               r_flush/0, r_flush/1,
               r_flush_onto/2, r_flush_onto/3,
               current_r_session/1, current_r_session/3,
               default_r_session/1,
               r_session_data/3, r_streams_data/3,
               r_history/0, r_history/1, r_history/2,
               r_session_version/1,
	   op( 950, xfx, (<-) )
          ] ).

:- use_module( library(lists) ).
:- use_module( library(readutil) ). % read_line_to_codes/2.

% Swi declaration:
:- use_module( library(process) ).   % process_create/3.
:- at_halt( r_close(all) ).
% end of Swi declaration.


:- dynamic( r_session/3 ).
:- dynamic( r_session_history/2 ).


/** <module> R session

This library facilitates interaction with an R session. On the Yap
system it depends on library(System) and on SWI on library(process)-
part of the clib package. Currently it only works on Linux systems. It
assumes an R executable in $PATH or can be given a location to a
functioning R executable. R is run as a slave with Prolog writing and
reading on/off the associated streams.

Multiple session can be managed simultaneously. Each has 3 main
components: a name or alias, a term structure holding the communicating
streams and a number of associated data items.

The library attempts to ease the translation between prolog terms and R
inputs. Thus, Prolog term =|x <- c(1,2,3)|= is translated to atomic =|'x
<- c(1,2,3)'|= which is then passed on to R. That is, =|<-|= is a
defined/recognised operator. =|X <- c(1,2,3)|=, where X is a variable,
instantiates X to the list =|[1,2,3]|=. Currently only vectors can be
translated in this fashion.

@author 	Nicos Angelopoulos
@version	0:0:1
@copyright	Nicos Angelopoulos
@license	YAP: Artistic
@see		examples/R/r_demo.pl, http://www.r-project.org/
@tbd		Fix starting the R process on Windows.
*/

%%% Section: Interface predicates

%%	r_open
%
%	Open a new R session. Same as r_open( [] ).
%
r_open :-
     r_open( [] ).

/* Opts = [alias/1,assert/1,at_r_halt/1,copy/2,rbin/1,with/1] */

%% r_open( +Opts )
%
%   Open a new R session with optional list of arguments. Opts should be
%   a list of the following
%
%       * alias(Alias)
%	Name for the session. If absent or  a variable an opaque term is
%	generated.
%
%       * assert(A)
%	Assert token. By default session  opened   last  is  the default
%	session (see default_r_session/1). Using A =   =z= will push the
%	session to the bottom of the pile.
%
%	* at_r_halt(RHAction)
%	R slaves often halt when they   encounter  an error. This option
%	provides a handle to changing the  behaviour of the session when
%	this happens. RHAction should be one of =abort=, =fail=, call/1,
%	call_ground/1, =reinstate= or =restart=. Default is =fail=. When
%	RHAction is =reinstate=, the history of   the session is used to
%	roll-back all the commands sent so far. At `restart' the session
%	is restarted with same name  and   options,  but  history is not
%	replayed.
%
%       * copy(CopyTo,CopyWhat)
%	Records interaction with R to a   file/stream.  CopyTo should be
%	one   of   =null=,   stream(Stream),   OpenStream,   AtomicFile,
%	once(File) or many(File). In the  case   of  many(File), file is
%	opened and closed at each write   operation.  CopyWhat should be
%	one of =both=, =in=, =out= or =none=.   In  all cases apart from
%	when CopyTo is =null=, error stream is copied to CopyTo. Default
%	is no recording (CopyTo = =null=).
%
%       * ssh(Host)
%       * ssh(Host,Dir)
%       Run R on Host with start directory Dir. Dir defaults to /tmp.
%
%       * rbin(Rbin)
%       R executable location. Default is 'R'.
%
%       * with(With)
%	With is in [environ,restore,save]. The   default behaviour is to
%	start the R executable  is   started  with  flags =|--no-environ
%	--no-restore --no-save|=. For each With value  found in Opts the
%	corresponding =|--no-|= flag is removed.
%
r_open( Opts ) :-
     r_open_1( Opts, _R, false ).

%%   r_close
%
%         Close the default R session.
%
r_close :-
     ( default_r_session( Alias ) ->
               r_close( Alias )
               ;
               fail_term( no_default_open_r_session_could_be_found_to_close )
     ).

%%   r_close(+R)
%
%         Close the named R session.
%
r_close( All ) :-
     All == all,
     !,
     findall( Alias, ( retract( r_session(Alias,Streams,Data) ),
                       r_close_session( Alias, Streams, Data ) ), _All ).
     % write( closed_all(All) ), nl.
r_close( Alias ) :-
     ( retract( r_session(Alias,Streams,Data) ) ->
          r_close_session( Alias, Streams, Data )
          ;
          fail_term( no_open_r_session_could_be_found_to_close_at:Alias )
     ).

%%   r_in(+Rcmd)
%
%         Push Rcmd to the default R session. Output and Errors  will be
%         printed to the terminal.
%
r_in( This ) :-
     default_r_session( R ),
     r_in( R, This, _ ).

%%   r_in(+R,+Rcmd)
%
%         As r_in/1 but for session R.
%
r_in( R, PrvThis ) :-
     r_in( R, PrvThis, _ ).

%%   r_push(+Rcmd)
%
%         As r_in/1 but does not consume error or output streams.
%
r_push( This ) :-
     default_r_session( R ),
     r_push( R, This ).

%%   r_push(+R,+Rcmd)
%
%         As r_push/1 but for named session.
%
r_push( R, RCmd ) :-
     current_r_session( R, Streams, Data ),
     r_session_data( copy_to, Data, CopyTo ),
     r_session_data( copy_this, Data, CopyThis ),
     r_streams( input, Streams, Ri ),
     r_input_normative( RCmd, RNrm ),
     write( Ri, RNrm ), nl( Ri ),
     r_record_term( CopyThis, CopyTo, RNrm ).

%%   r_out(+Rcmd,-Lines)
%
%         Push Rcmd to default R session and grab output lines Lines as
%         a list of code lists.
%
r_out( This, Read ) :-
     default_r_session( R ),
     r_out( R, This, Read ).

%%   r_out(+R,+Rcmd,-Lines)
%
%         As r_out/2 but for named session R.
%
r_out( R, RCmd, RoLns ) :-
     r_push( R, RCmd, Rplc, RoLns, ReLns, Halt, HCall ),
     r_lines_print( ReLns, error, user_error ),
     r_record_history( Halt, R, RCmd ),
     r_out_halted_record( Halt, R, RoLns ),
     replace_variables( Rplc ),
     call( HCall ).

%%   r_err(+Rcmd,-Lines,-ErrLines)
%
%         Push Rcmd to default R session and grab output lines Lines as
%         a list of code lists. Error lines are in ErrLines.
%
r_err( This, Read, ErrRead ) :-
     default_r_session( R ),
     r_err( R, This, Read, ErrRead ).

%%   r_err(+R,+Rcmd,-Lines, -ErrLines)
%
%         As r_err/3 but for named session R.
%
r_err( R, RCmd, RoLns, ReLns ) :-
     r_push( R, RCmd, Rplc, RoLns, ReLns, Halt, HCall ),
     r_lines_print( ReLns, error, user_error ),
     r_record_history( Halt, R, RCmd ),
     r_out_halted_record( Halt, R, RoLns ),
     replace_variables( Rplc ),
     call( HCall ).

%%   r_print(+X)
%
%         A shortcut for r_in( print(X) ).
%
r_print( This ) :-
     default_r_session( R ),
     r_print( R, This ).

%%   r_print(+R,+X)
%
%         As r_print/1 but for named session R.
%
r_print( R, This ) :-
     r_out( R, This, Read ),
     r_lines_print( Read, output ).

%%   r_lines_print( +Lines )
%
%         Print a list of code lists (Lines) to the user_output.
%         Lines would normally be read of an R stream.
%
r_lines_print( Lines ) :-
     r_lines_print( Lines, output, user_output ).

%%   r_lines_print( +Lines, +Type )
%
%         As r_lines_print/1 but Type declares whether to treat lines
%         as output or error response. In the latter case they are written
%         on user_error and prefixed with '!'.
%
r_lines_print( Lines, Type ) :-
     r_lines_print_type_stream( Type, Stream ),
     r_lines_print( Lines, Type, Stream ).

%%   r_lines_print( +Lines, +Type, +Stream )
%
%         As r_lines_print/3 but Lines are written on Stream.
%
r_lines_print( [], _Type, _Stream ).
r_lines_print( [H|T], Type, Stream ) :-
     atom_codes( Atm, H ),
     r_lines_print_prefix( Type, Stream ),
     write( Stream, Atm ), nl( Stream ),
     r_lines_print( T, Type, Stream ).

%%   r_lib(+L)
%
%         A shortcut for r_in( library(X) ).
%
r_lib( Lib ) :-
     default_r_session( R ),
     r_lib( R, Lib ).

%%   r_lib(+R,+L)
%
%            As r_lib/1 but for named session R.
%
r_lib( R, Lib ) :-
     r_in( R, library(Lib) ).

%%   r_flush
%
%         Flush default R's output and error on to the terminal.
%
r_flush :-
     default_r_session( R ),
     r_flush( R ).

%%   r_flush(+R)
%
%         As r_flush/0 but for session R.
%
r_flush( R ) :-
     r_flush_onto( R, [output,error], [Li,Le] ),
     r_lines_print( Li, output ),
     r_lines_print( Le, error ).

%%   r_flush_onto(+SAliases,-Onto)
%
%         Flush stream aliases to code lists Onto. SAliases
%         should be one of, or a list of, [output,error].
%
r_flush_onto( RinStreamS, OntoS ) :-
     default_r_session( R ),
     r_flush_onto( R, RinStreamS, OntoS ).

%% r_flush_onto(+R,+SAliases,-Onto)
%
%         As r_flush_onto/2 for specified session R.
%
r_flush_onto( R, RinStreamS, OntoS ) :-
     ( is_list(RinStreamS) -> RinStreams = RinStreamS; RinStreams=[RinStreams] ),
     % to_list( RinStreamS, RinStreams ),
     r_input_streams_list( RinStreams ),
     r_flush_onto_1( RinStreams, R, Ontos ),
     ( is_list(RinStreamS) -> OntoS = Ontos; Ontos=[OntoS] ).

%%   current_r_session(?R)
%         True if R is the name of current R session.
%         Can be used to enumerate all open sessions.
%
current_r_session( R ) :-
     var( R ),
     !,
     r_session( R, _Session, _Data ).
current_r_session( R ) :-
     r_session( R, _Session, _Data ),
     !.
current_r_session( R ) :-
     fail_term( 'Could not find session':R ).

%%   current_r_session(?R,?S,?D)
%
%         True if R is an open session with streams S
%         and data D (see introduction to the library).
%
current_r_session( Alias, R, Data ) :-
     r_session( Alias, R, Data ).

%% default_r_session(?R)
%
%         True if R is the default session.
%
default_r_session( R ) :-
     ( var(R) ->
          ( r_session(R,_Cp1,_Wh1) ->
               true
               ;
               fail_term( no_default_open_r_session_was_found )
          )
          ;
          ( r_session(R,_Cp2,_Wh2) ->
               true
               ;
               fail_term( no_open_r_session_at(R) )
          )
     ).

%%   r_streams_data(+SId,+Streams,-S)
%         True if Streams is an R session streams
%         structure and S is its stream corresponding
%         to identifier SId, which should be one of
%         [input,output,error].
%
r_streams_data( input,  r(Ri,_,_), Ri ).
r_streams_data( output, r(_,Ro,_), Ro ).
r_streams_data( error,  r(_,_,Re), Re ).

%% r_session_data(+DId,+Data,-Datum)
%
%         True if Data is a structure representing
%         R session associated data and Datum is its
%         data item corresponding to data identifier
%         DId. DId should be in
%         [copy_to,copy_this,at_r_halt,opts].
%
r_session_data( copy_to, rsdata(Copy,_,_,_), Copy ).
r_session_data( copy_this, rsdata(_,This,_,_), This ).
r_session_data( at_r_halt, rsdata(_,_,RHalt,_), RHalt ).
r_session_data( opts, rsdata(_,_,_,Opts), Opts ).

%%   r_history
%
%         Print on user_output the history of the default session.
%
r_history :-
     default_r_session( R ),
     r_session_history( R, History ),
     reverse( History, Hicory ),
     write( history(R) ), nl, write('---' ), nl,
     ( (member(H,Hicory),write(H),nl,fail) -> true; true ),
     write( '---' ), nl.

%%   r_history(-H)
%
%         H unifies to the history list of the Rcmds fed into the default
%         session. Most recent command appears at the head of the list.
%
r_history( History ) :-
     default_r_session( R ),
     r_session_history( R, History ).

%%   r_history(?R,-H)
%         As r_history/1 but for named session R.
%         It can be used to enumerate all histories.
%         It fails when no session is open.
%
r_history( R, History ) :-
     r_session_history( R, History ).

%%   r_session_version(-Version)
%         Installed version. Version is of the form Major:Minor:Fix,
%         where all three are integers.
%
r_session_version( 0:0:1 ).

%%% Section: Auxiliary predicates

% Rcv == true iff r_open_1/3 is called from recovery.
%
r_open_1( Opts, Alias, Rcv ) :-
     ( memberchk(rbin(RBin),Opts)->
          ( options_have_ssh(Opts,Host,Dir) ->
               atoms_concat( ['ssh ', Host,' "cd ',Dir,'; ',RBin], RPfx ),
               RPsf = '"'
               ;
               RPfx = RBin,
               RPsf = ''
          )
          ;
          ( options_have_ssh(Opts,Host,Dir) ->
               atoms_concat( ['ssh ', Host,' "cd ',Dir,'; R '], RPfx ),
               RPsf = '"'
               ;
               ( shell('sh -c "which R &> /dev/null"  ') ->
                    RPfx = 'R',
                    RPsf = ''
                    ;
                    fail_term( cannot_locate_r_executable )
               )
          )
     ),
     r_opt_with_exec( RPfx, Opts, PrvExec ),
     atom_concat( PrvExec, RPsf, Exec ),
     r_process( Exec, Ri, Ro, Re ),
     R = r(Ri,Ro,Re),
     r_streams_set( Ri, Ro, Re ),
     r_process_was_successful( Ri, Ro, Re ),
     r_open_opt_copy( Opts, CpOn, CpWh, Rcv ),
     r_open_opt_at_r_halt( Opts, RHalt ),
     ( memberchk(alias(Alias),Opts) ->
          ( var(Alias) ->
               r_session_skolem( Alias, 1 )
               ;
               ( r_session(Alias,_,_) ->
                    fail_term( 'Session already exists for alias':Alias )
                    ;
                    true
               )
          )
          ;
          r_session_skolem( Alias, 1 )
     ),
     RData = rsdata(CpOn,CpWh,RHalt,Opts),
     ( memberchk(assert(Assert),Opts) ->
          ( Assert == a ->
               asserta( r_session(Alias,R,RData) )
               ;
               ( Assert == z ->
                    assertz( r_session(Alias,R,RData) )
                    ;
                    fail_term( 'Cannot decipher argument to assert/1 option':Assert )
               )
          )
          ;
          asserta( r_session(Alias,R,RData) )
     ),
     AtRH = at_r_halt(reinstate),
     ( (memberchk(history(false),Opts),\+memberchk(AtRH,Opts)) ->
               true
               ;
               retractall( r_session_history(Alias,_) ),
               assert( r_session_history(Alias,[]) )
     ).

r_close_session( Alias, Streams, Data ) :-
     r_streams_data( input, Streams, Ri ),
     r_streams_data( output,Streams, Ro ),
     r_streams_data( error, Streams, Re ),
     r_session_data( copy_to, Data, CopyTo ),
     r_session_data( copy_this, Data, CopyThis ),
     write( Ri, 'q()' ), nl( Ri ),
     r_record_term( CopyThis, CopyTo, 'q()' ),
     ( (CopyTo=stream(CopyS),stream_property(CopyS,file_name(CopyF)),CopyF\==user)->
          close(CopyS)
          ;
          true
     ),
     close( Ri ),
     close( Ro ),
     close( Re ),
     retractall( r_session_history(Alias,_) ).

r_in( R, RCmd, Halt ) :-
     r_push( R, RCmd, Rplc, RoLns, ReLns, Halt, HCall ),
     r_out_halted_record( Halt, R, RoLns ),
     r_lines_print( RoLns, output, user_output ),
     r_lines_print( ReLns, error, user_error ),
     r_record_history( Halt, R, RCmd ),
     replace_variables( Rplc ),
     call( HCall ).

r_push( R, RCmd, Rplc, RoLns, ReLns, Halt, HCall ) :-
     current_r_session( R, Streams, Data ),
     r_session_data( copy_to, Data, CopyTo ),
     r_session_data( copy_this, Data, CopyThis ),
     r_streams( input, Streams, Ri ),
     r_input_normative( RCmd, R, 0, RNrm, Rplc, _ ),
     write( Ri, RNrm ), nl( Ri ),
     r_record_term( CopyThis, CopyTo, RNrm ),
     r_lines( Streams, error, ReLns ),
     r_halted( ReLns, R, Halt, HCall ),
     ( Halt == true ->
          r_streams( output, Streams, Ro ),
          r_read_lines( Ro, [], RoLns )
          ;
          r_lines( Streams, output, RoLns )
     ),
     r_record_lines( RoLns, output, CopyTo ),
     r_record_lines( ReLns, error, CopyTo ).

r_out_halted_record( true, _Alias, [] ).
r_out_halted_record( false, _Alias, Lines ) :-
     r_session_data( copy_this, Data, CopyThis ),
     r_session_data( copy_to, Data, CopyTo ),
     ( (CopyThis==out;CopyThis==both) ->
          r_record_lines( Lines, output, CopyTo )
          ;
          true
     ).

r_flush_onto_1( [], _R, [] ).
r_flush_onto_1( [H|T], R, [HOn|TOns] ) :-
     current_r_session( R, Streams, _ ),
     r_lines( Streams, H, HOn ),
     r_flush_onto_1( T, R, TOns ).

replace_variables( [] ).
replace_variables( [arp(R,Pv,Rv)|T] ) :-
     r_out( R, Rv, Lines ),
     r_read_list( Lines, Pv ),
     % r_lines_to_pl_var( Lines, Pv ),
     replace_variables( T ).

% r_lines_to_pl_var( [], [] ).
% r_lines_to_pl_var( [H|T], [] ) :-
     % r_line_to_pl_var( [H|T], [] ) :-
     % r_lines_to_pl_var( T, TPv ).

r_input_streams_list( Rins ) :-
     ( select(output,Rins,NoInpIns) -> true; NoInpIns=Rins ),
     ( select(error,NoInpIns,NoErrIns) -> true; NoErrIns=NoInpIns ),
     ( NoErrIns = [] ->
          true
          ;
          ( (memberchk(input,NoErrIns);memberchk(error,NoErrIns)) ->
                    fail_term( 'duplicate entries in input streams list':Rins )
                    ;
                    fail_term( 'superfluous entries in input streams list':Rins )
          )
     ).

% succeds if Rcmd produces empty output, otherwise it fails
ro_empty( R, Rcmd ) :-
     r_out( R, Rcmd, [] ).

r_input_normative( (A;B), R, I, This, Rplc, OutI ) :-
     !,
     r_input_normative( A, R, I, ThisA, RplcA, NxI ),
     r_input_normative( B, R, NxI, ThisB, RplcB, OutI ),
     atoms_concat( [ThisA,'; ',ThisB], This ),
     append( RplcA, RplcB, Rplc ).

r_input_normative( Obj<-Call, R, I, This, Rplc, NxI ) :-
     !,
     ( var(Obj) ->
          Rplc = [arp(R,Obj,ThisObj)],
          number_codes( I, ICs ),
          append( "pl_Rv_", ICs, RvCs ),
          atom_codes( ThisObj, RvCs ),
          NxI is I + 1
          ;
          Rplc = [],
          r_input_normative( Obj, ThisObj ),
          NxI is I
     ),
     r_input_normative( Call, ThisCall ),
     atoms_concat( [ThisObj,' <- ',ThisCall], This ).
r_input_normative( PrvThis, _R, I, This, [], I ) :-
     r_input_normative( PrvThis, This ).

r_input_normative( Var, This ) :-
     var(Var),
     !,
     This = Var.
r_input_normative( Opt=Val, This ) :-
     !,
     r_input_normative( Opt, ThisOpt ),
     r_input_normative( Val, ThisVal ),
     atoms_concat( [ThisOpt,'=',ThisVal], This ).
% 2008ac06, careful! we are changing behaviour here
r_input_normative( PrvThis, This ) :-
     ( (\+ var(PrvThis),(PrvThis = [_|_];PrvThis=[])) ->
          append( PrvThis, [0'"], ThisRight ),
          atom_codes( This, [0'"|ThisRight] )
          ;
          ( compound(PrvThis) ->
               PrvThis =.. [Name|Args],
               ( (current_op(_Pres,Asc,Name),
                  atom_codes(Asc,[_,0'f,_]),
                  Args = [Arg1,Arg2]
               ) ->
                    r_input_normative( Arg1, Arg1Nrm ),
                    r_input_normative( Arg2, Arg2Nrm ),
                    atoms_concat( [Arg1Nrm,Name,Arg2Nrm], This )
                    ;
                    r_input_normative_tuple( Args, Tuple ),
                    atoms_concat( [Name,'(',Tuple,')'], This )
               )
               ;
               ( number(PrvThis) ->
                    number_codes( PrvThis, ThisCs ),
                    atom_codes( This, ThisCs )
                    ;
                    This = PrvThis
               )
          )
     ).

r_input_normative_tuple( [], '' ).
r_input_normative_tuple( [H|T], Tuple ) :-
     r_input_normative_tuple( T, Psf ),
     r_input_normative( H, HNorm ),
     ( Psf == '' -> Tuple = HNorm
        ; atoms_concat([HNorm,',',Psf], Tuple) ).

r_read_lines( Ro, TermLine, Lines ) :-
     read_line_to_codes( Ro, Line ),
     r_read_lines_1( Line, TermLine, Ro, Lines ).

r_halted( Lines, R, Halted, HCall ) :-
     last( Lines, "Execution halted" ),
     !,
     Halted = true,
     findall( rs(Alias,Streams,Data), retract(r_session(Alias,Streams,Data)), Sessions),
     \+ var(R),
     r_halted_recovery( Sessions, R, HCall ).
r_halted( _, _R, false, true ).

r_halted_recovery( [], R, Which ) :-
     ( var(Which) ->
          fail_term( internal_error_in_recovering_from_halt(R) )
          ;
          true
     ).
r_halted_recovery( [rs(AliasH,StreamsH,DataH)|T], R, Which ) :-
     ( R == AliasH ->
          r_session_data( at_r_halt, DataH, AtHalt ),
          r_halted_recovery_action( AtHalt, AliasH, StreamsH, DataH, Which )
          ;
          assertz(r_session(AliasH,StreamsH,DataH))
     ),
     r_halted_recovery( T, R, Which ).

r_halted_recovery_action( restart, Alias, _Streams, Data, true ) :-
     write( user_error, 'at_r_halt(restart): restarting r_session ':Alias ), nl( user_error ),
     r_session_data( opts, Data, Opts ),
     ( memberchk(copy(CopyTo,_),Opts) -> r_halted_restart_copy(CopyTo); true ),
     r_open_1( Opts, Alias, true ),
     current_r_session( Alias, Streams, _ ),
     r_lines( Streams, output, _ReLines ).
r_halted_recovery_action( reinstate, Alias, _Streams, Data, true ) :-
     ( r_session_history(Alias,History) ->
          r_session_data( opts, Data, Opts ),
          r_open_1( Opts, Alias, true ),
          reverse( History, Hicory ),
          r_halted_recovery_rollback( Hicory, Alias )
          ;
          fail_term( 'at_r_halt(reinstate): cannnot locate history for':Alias )
     ).
r_halted_recovery_action( abort, _Alias, _Streams, _Data, abort ) :-
     write( user_error, 'at_r_halt(abort): R session halted by slave' ), nl( user_error ).
r_halted_recovery_action( fail, Alias, _Streams, _Data, Call ) :-
     retractall( r_session_history(Alias,_) ),
     Call = fail_term( 'at_r_halt(fail): failure due to execution halted by slave on r_session':Alias ).
r_halted_recovery_action( call(Call), _Alias, Streams, _Data, Call ) :-
     Call = call( Call, Streams ).
r_halted_recovery_action( call_ground(Call), _Alias, _Streams, _Data, Call) :-
     Call = call( Call ).

r_halted_restart_copy( CopyTo ) :-
     ((atomic(CopyTo),File=CopyTo);CopyTo=once(File)),
     File \== user,      % you never known
     !,
     open( File, read, Dummy ),
     stream_property( Dummy, file_name(Full) ),
     close( Dummy ),
     ( stream_property(OpenStream,file_name(Full)) ->
          write( close( OpenStream ) ), nl,
          close( OpenStream )
          ;
          true
     ).
r_halted_restart_copy( _CopyTo ).

r_halted_recovery_rollback( [], _Alias ).
r_halted_recovery_rollback( [H|T], Alias ) :-
     r_in( Alias, H, _Halted ),
     r_halted_recovery_rollback( T, Alias ).


r_record_history( true, _Alias, _This ).
r_record_history( false, Alias, This ) :-
     r_session_history( Alias, Old ),
     !,
     retractall( r_session_history(Alias,_) ),
     assert( r_session_history(Alias,[This|Old]) ).
r_record_history( false, _, _ ). % fold with true if assumption is correct

r_read_lines_1( eof, _TermLine, _Ro, Lines ) :- !, Lines = [].
r_read_lines_1( end_of_file, _TermLine, _Ro, Lines ) :- !, Lines = [].
r_read_lines_1( [255], _TermLine, _Ro, Lines ) :- !, Lines = [].
     % yap idiosyncrasy
r_read_lines_1( TermLine, TermLine, _Ro, Lines ) :- !, Lines = [].
r_read_lines_1( Line, TermLine, Ro, [Line|Lines] ) :-
     read_line_to_codes( Ro, NewLine ),
     r_read_lines_1( NewLine, TermLine, Ro, Lines ).

r_boolean( Boo, Rboo ) :-
     ( memberchk(Boo,[t,true,'TRUE']) ->
          Rboo = 'TRUE'
          ;
          memberchk(Boo,[f,false,'FALSE']),
          Rboo = 'FALSE'
     ).

r_read_list( [], [] ).
r_read_list( [PreH|T], List ) :-
     delete_leading( PreH, 0' , H ),
     ( H = [0'[|Hrm] ->
          break_list_on( Hrm, 0'], _, Hprv ),
          delete_leading( Hprv, 0' , Hproper )
          ;
          Hproper = H
     ),
     r_read_list_line( Hproper, List, ConTail ),
     r_read_list( T, ConTail ).

r_read_list_line( [], List, List ).
r_read_list_line( [0' |RRead], List, ConTail ) :-
     !,
     r_read_list_line( RRead, List, ConTail ).
r_read_list_line( [Fst|RRead], [H|List], ConTail ) :-
     break_list_on( RRead, 0' , RemCs, RemNumCs ),
     !,
     number_codes( H, [Fst|RemCs] ),
     r_read_list_line( RemNumCs, List, ConTail ).
r_read_list_line( [Fst|RemCs], [H|List], List ) :-
     number_codes( H, [Fst|RemCs] ).

r_streams( [], _R, [] ).
r_streams( [H|T], R, [SH|ST] ) :-
     !,
     r_stream( H, R, SH ),
     r_streams( T, R, ST ).

r_streams( Id, R, Stream ) :-
     r_stream( Id, R, Stream ).

r_stream( H, R, SH ) :-
     % current_r_session( R ),
     ( var(H) ->
          fail_term( variable_stream_identifier )
          ;
          true
     ),
     ( r_streams_data( H, R, SH ) ->
          true
          ;
          fail_term( invalid_r_stream:H )
     ).

/*
r_terminator( r(Ri,Ro,_Re), Lines ) :-
     write( Ri, 'print(\"prolog_eoc\")' ),
     nl( Ri ),
     r_read_lines_till( Ro, "[1] \"prolog_eoc\"", Lines ).

r_read_lines_till( Ro, Terminator, Lines ) :-
     fget_line( Ro, Line ),
     r_read_lines_till_1( Line, Terminator, Ro, Lines ).

r_read_lines_till_1( Line, Line, _Ro, Lines ) :-
     !,
     Lines = [].
r_read_lines_till_1( Line, Terminator, Ro, [Line|Lines] ) :-
     fget_line( Ro, NxLine ),
     NxLine \== eof,
     r_read_lines_till_1( NxLine, Terminator, Ro, Lines ).
*/

r_open_opt_copy( Opts, CpTerm, What, Rcv ) :-
     ( (memberchk(copy(Cp,CpWh),Opts),Cp \== null) ->
          % heere
          ( ((catch(is_stream(Cp),_,fail),CpS=Cp);Cp=stream(CpS)) ->  % catch = yap bug
               CpTerm = stream(CpS)
               ;
               ( atomic(Cp) ->
                    ( Rcv==true -> Mode = append; Mode = write ),
                    open( Cp, Mode, CpStream ),
                    CpTerm = stream(CpStream)
                    ;
                    ( Cp = once(CpFile) ->
                         ( Rcv==true -> Mode = append; Mode = write ),
                         open( CpFile, Mode, CpStream ),
                         CpTerm = stream(CpStream)
                         ;
                         ( Cp = many(CpFile) ->
                              CpTerm = file(CpFile)
                              ;
                              fail_term( 'I cannot decipher 1st argument of copy/2 option':Cp )
                         )
                    )
               )
          ),
          ( memberchk(CpWh,[both,none,in,out])->
               What = CpWh
               ;
               fail_term( 'I cannot decipher 2nd arg. to copy/2 option':CpWh )
          )
          ;
          CpTerm = null, What = none
     ).

r_open_opt_at_r_halt( Opts, RHalt ) :-
     ( memberchk(at_r_halt(RHalt),Opts) ->
          ( memberchk(RHalt,[restart,reinstate,fail,abort,call(_),call_ground(_)]) ->
               true
               ;
               fail_term( 'Cannot decipher argument to at_r_halt option':RHalt )
          )
          ;
          RHalt = fail
     ).

r_opt_with_exec( _RBin, Opts, _Exec ) :-
     member( with(With), Opts ),
     \+ memberchk(With, [environ,restore,save] ),
     !,
     fail_term( 'Cannot decipher argument to option with/1': With ).
r_opt_with_exec( RBin, Opts, Exec ) :-
     findall( W, member(with(W),Opts), Ws ),
     sort( Ws, Sr ),
     length( Ws, WsL ),
     length( Sr, SrL ),
     ( WsL =:= SrL ->
          atom_concat( RBin, ' --slave ', Pfx ),
          r_opt_with_concat( [environ,restore,save], Ws, Pfx, Exec )
          ;
          fail_term( 'Multiple identical args in with/1 option': Ws )
     ).

% r_opt_exec_no( [environ,restore,save], Ws, Pfx, Exec ) :-
r_opt_exec_no( [], _Ws, [] ).
r_opt_exec_no( [H|T], Ws, Exec ) :-
     ( memberchk(H,Ws) ->
          TExec=Exec
          ;
          atom_concat( ' --no-', H, NoH ),
          Exec=[NoH|TExec]
     ),
     r_opt_exec_no( T, Ws, TExec ).

r_opt_with_concat( [], _Ws, Exec, Exec ).
r_opt_with_concat( [H|T], Ws, Pfx, Exec ) :-
     ( memberchk(H,Ws) ->
          Nxt = Pfx
          ;
          atom_concat( ' --no-', H, NoH ),
          atom_concat( Pfx, NoH, Nxt )
     ),
     r_opt_with_concat( T, Ws, Nxt, Exec ).

r_record_lines( [], _Type, _CopyTo ) :- !.
r_record_lines( Lines, Type, CopyTo ) :-
     ( CopyTo == null ->
          true
          ;
          copy_stream_open( CopyTo, CopyStream ),
          r_lines_print( Lines, Type, CopyStream )
     ).

r_record_term( CopyThis, CopyTo, This ) :-
     ( CopyThis == in; CopyThis == both),
     CopyTo \== null,
     !,
     copy_stream_open( CopyTo, CopyOn ),
     write( CopyOn, This ),
     nl( CopyOn ),
     copy_stream_close( CopyTo ).
r_record_term( _CopyThis, _CopyTo, _This ).

copy_stream_open( stream(CopyStream), CopyStream ).
copy_stream_open( file(File), CopyStream ) :-
     open( File, append, CopyStream ).

copy_stream_close( file(CopyTo) ) :- close( CopyTo ).
copy_stream_close( stream(_) ).

/*
write_list_to_comma_separated( [], _Sep, _Out ).
write_list_to_comma_separated( [H|T], Sep, Out ) :-
     write( Out, Sep ),
     write( Out, H ),
     write_list_to_comma_separated( T, ',', Out ).
     */

fail_term( Term ) :-
     ( Term = What:Which ->
          write( user_error, What ),
          write( user_error, ': ' ),
          write( user_error, Which )
          ;
          write( user_error, Term )
     ),
     nl( user_error ), fail.

r_lines( Streams, ROstream, Lines ) :-
     r_streams_data( input,  Streams, Ri ),
     r_streams_data( ROstream,  Streams, Ro ),
     ( ROstream == error ->
          Mess = 'message("prolog_eoc")',
          Trmn = "prolog_eoc"
          ;
          Mess = 'print("prolog_eoc")',
          Trmn = "[1] \"prolog_eoc\""
     ),
     Excp = error(io_error(write, _), context(_,_)),
     catch( (write(Ri,Mess),nl(Ri)), Excp, true ),
     r_read_lines( Ro, Trmn, Lines ).

r_lines_print_type_stream( output, user_output ).
r_lines_print_type_stream( error, user_error ).

r_lines_print_prefix( error, Stream ) :- write( Stream, '!  ' ).
r_lines_print_prefix( output, _Stream ).

r_session_skolem( Alias, I ) :-
     Alias = '$rsalias'(I),
     \+ r_session( Alias, _, _ ),
     !.
r_session_skolem( Alias, I ) :-
     NxI is I + 1,
     r_session_skolem( Alias, NxI ).

r_process_was_successful( Ri, Ro, Re ) :-
     Mess = 'message("prolog_eoc")',
     Trmn = "prolog_eoc",
     catch( (write(Ri,Mess),nl(Ri)), Excp, true ),
     r_read_lines( Re, Trmn, Lines ),
     r_lines_print( Lines, error, user_error ),
     ( (var(Excp),Lines==[]) ->
          true
          ;
          ( Excp = error(io_error(write, _), context(_,_)) ->
               true
               ;
               print_message( error, Excp )
          ),
          close( Ri ), close( Ro ), close( Re ),
          fail_term( failed_to_open_session )
     ).

%%%%%%%%
% break_list_on( +List, +Element, ?LeftPartition, ?RightPartition ).
% Element does not appear in either the end of LeftPartition,
% or as first element of RightPartition.
% Only finds first partition so Element should be ground
% | ?- break_list_on( L, El, [a], [c,b,d,b,e] ).
%  = [a,El,c,b,d,b,e] ? ; no
%
break_list_on( [X|Xs], X, [], Xs ) :-
	!.
break_list_on( [X|Xs], Xa, [X|XLa], XRa ) :-
	break_list_on( Xs, Xa, XLa, XRa ).

delete_leading( [], _Chop, [] ).
delete_leading( [H|T], Chop, Clean ) :-
     ( H == Chop ->
          R = T,
          Clean = TClean
          ;
          R = [],
          Clean = [H|T]
     ),
     delete_leading( R, Chop, TClean ).

options_have_ssh( Opts, Host, Dir ) :-
     ( memberchk(ssh(Host),Opts) ->
          Dir = '/tmp'
          ;
          memberchk( ssh(Host,Dir), Opts )
     ).

% Section: Swi Specifics.

/*
r_lines( Streams, ROstream, Lines ) :-
     r_streams_data( input,  Streams, Ri ),
     r_streams_data( ROstream,  Streams, Ro ),
     ( ROstream == error ->
          Mess = 'message("prolog_eoc")',
          Trmn = "prolog_eoc"
          ;
          Mess = 'print("prolog_eoc")',
          Trmn = "[1] \"prolog_eoc\""
     ),
     Excp = error(io_error(write, _), context(_,_)),
     catch( (write(Ri,Mess),nl(Ri)), Excp, true ),
     r_read_lines( Ro, Trmn, Lines ).
     */

atoms_concat( Atoms, Concat ) :-
     atomic_list_concat( Atoms, Concat ).

r_streams_set( Ri, Ro, Re ) :-
     set_stream( Ri, buffer(false) ), set_stream( Ri, close_on_abort(true) ),
     set_stream( Ro, buffer(false) ), set_stream( Ro, close_on_abort(true) ),
     set_stream( Re, buffer(false) ), set_stream( Re, close_on_abort(true) ).

r_process( Exec, Ri, Ro, Re ) :-
     Streams = [stdin(pipe(Ri)),stdout(pipe(Ro)),stderr(pipe(Re))],
     process_create( '/bin/sh', ['-c',Exec], Streams ).

