:- module('$file', []).

:- register_iri_scheme(file, file_iri_hook, []).

%!  file_iri_hook(+Action, +IRI, -Result) is semidet.
%
%   Hooks IRI-style file system access using   the "file" schema. Useful
%   when dragging files in macOS, for   example,  where Finder drops the
%   file reference using a file://some-file atom. Maps file-scheme hooks
%   directly to their corresponding built-in file-oriented predicates.
%
%   Has  potential  self-recursion  since    the   delegated  predicates
%   themselves re-invoke the registered IRI hooks if they receive an IRI
%   scheme. The implementation therefore   relies  on file_name_to_url/2
%   for translating file-IRI references to  non-IRI file references, and
%   thereby avoids re-invoking the hooks.
%
%   @see register_iri_scheme/3
%   @see open/4

file_iri_hook(open(Mode, Options), IRI, Stream) :-
    file_name_to_url(File, IRI),
    open(File, Mode, Stream, Options).
file_iri_hook(access(Mode), IRI, Bool) :-
    file_name_to_url(File, IRI),
    (   access_file(File, Mode)
    ->  Bool = true
    ;   Bool = false
    ).
file_iri_hook(time, IRI, Time) :-
    file_name_to_url(File, IRI),
    time_file(File, Time).
file_iri_hook(size, IRI, Size) :-
    file_name_to_url(File, IRI),
    size_file(File, Size).
