% Load the extended "van Roy"  benchmarks   from  GitHub.  After loading
% completes, you may run the suite using e.g.
%
%     ?- run(0.01).
%
% According to this set, the browser  version   is  a  bit over 10 times
% slower than the  naive  version.  This  is   in  part  caused  by  the
% Emscripten WASM compiler that is based   on  Clang. GCC produces about
% 30% better code. Another reason is the   suboptimal support for 32 bit
% systems of SWI-Prolog.

:- set_prolog_flag(optimise, true).
:- ['https://raw.githubusercontent.com/SWI-Prolog/bench/refs/heads/master/run.pl'].
