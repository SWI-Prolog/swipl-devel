
% :- load_compilation_module(library('regtypes/regtypes_tr')).
% :- add_sentence_trans(expand_regtypes/2).

:- new_declaration(regtype/1).
:- new_declaration(regtype/2).

:- op(1150, fx,(regtype)).
:- op(1150,xfx,(regtype)).
:- op(500, yfx,#).			% JW: Added

% :- meta_predicate regtype(goal).

%% Control version comment prompting for the file.
%% Local Variables:
%% mode: CIAO
%% update-version-comments: "off"
%% End:

