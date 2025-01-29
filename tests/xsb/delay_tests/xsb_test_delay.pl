/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(xsb_test_delay,
          [ xsb_test_delay/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_delay :-
    run_tests([ delay_dynstrat,
                delay_old_failures,
                delay_old_segv,
                delay_interp,
                delay_non_strat_neg,
                delay_wfs_neg,
                delay_pos_simpl,
                delay_pot_pouri,
                delay_ac,
                delay_misc
              ]).

term_expansion(xsb_test(Test),
               (   test(Test) :-
                       xsb_test(delay_tests, Test, test)
               )).

:- begin_tests(delay_dynstrat, [sto(rational_trees)]).

xsb_test(dynstrat1).                    % Needs n simplification
xsb_test(dynstrat2).                    % No simplification
xsb_test(dynstrat3).                    % Needs p+n simplification
xsb_test(dynstrat4).                    % Needs n simplification
xsb_test(dynstrat5).                    % Needs n simplification
xsb_test(dynstrat6).                    % Needs n simplification
xsb_test(dynstrat7).                    % Needs n simplification
xsb_test(ross1).                        % No simplification
xsb_test(sel_unsusp).                   % Tests selective unsuspension
xsb_test(dl_dupl).                      % Needs p+n simplification
xsb_test(asl_dupl).                     % Needs n simplification
xsb_test(gfp).                          % Needs cascading simplifications

:- end_tests(delay_dynstrat).

:- begin_tests(delay_old_failures, [sto(rational_trees)]).

xsb_test(fr1).
xsb_test(fr2).
xsb_test(fr3).
xsb_test(fr4).
xsb_test(fr5).
xsb_test(fr6).
xsb_test(fr7).
xsb_test(fr8).
xsb_test(fr9).
xsb_test(fr19).
xsb_test(fr20).
xsb_test(fr21).
xsb_test(fr22).
xsb_test(fr23).
xsb_test(fr24).
xsb_test(fr25).
xsb_test(fr26).
xsb_test(fr27).
xsb_test(fr28).
xsb_test(fr29).                         % shows need for undeleting answers
xsb_test(fr30).                         % shows need for junking answers

:- end_tests(delay_old_failures).

:- begin_tests(delay_old_segv, [sto(rational_trees)]).

xsb_test(seg1).
xsb_test(seg2).
xsb_test(seg3).
xsb_test(seg4).
xsb_test(seg5).
xsb_test(fr24).

:- end_tests(delay_old_segv).

:- begin_tests(delay_interp, [sto(rational_trees)]).

xsb_test(interp0).
xsb_test(interp1).
xsb_test(interp2).                      % was giving wrong results
%xsb_test(interp3).                     % was causing an infinite loop
xsb_test(interp4).                      % showed need for ptcpreg in Prolog CPs
xsb_test(interp5).
xsb_test(interp6).
xsb_test(interp7).
xsb_test(interp8).
xsb_test(interp9).
xsb_test(interp10).
xsb_test(interp11).                     % was giving wrong results

:- end_tests(delay_interp).

:- begin_tests(delay_non_strat_neg, [sto(rational_trees)]).

xsb_test(two_ary).
xsb_test(abol_susp1).                   % Tests abolishing suspensions
xsb_test(abol_susp2).                   % Tests abolishing suspensions
xsb_test(przy1).                        % No simplification
xsb_test(przy1_simp).                   % Needs n simplification
xsb_test(nonstrat1).                    % No simplification
xsb_test(nonstrat2).                    % Needs n simplification

:- end_tests(delay_non_strat_neg).

:- begin_tests(delay_wfs_neg, [sto(rational_trees)]).

xsb_test(p1).                           % No simplification
xsb_test(p2).                           % No simplification
xsb_test(p3).                           % No simplification
xsb_test(p4).                           % No simplification
xsb_test(p5).                           % Needs n simplification
xsb_test(simpl_win).                    % No simplification
xsb_test(win).                          % Tests cond vs uncond. answers
xsb_test(cond_uncond).                  % Tests cond vs uncond. answers
xsb_test(ullman3).                      % Requires answers to be returned
xsb_test(undef1).                       % To test printing of delay lists
xsb_test(undef2).                       % To test printing of delay lists

:- end_tests(delay_wfs_neg).

:- begin_tests(delay_pos_simpl, [sto(rational_trees)]).

xsb_test(pos_simpl1).

:- end_tests(delay_pos_simpl).

:- begin_tests(delay_pot_pouri, [sto(rational_trees)]).

% xsb_test(pot_pouri).

:- end_tests(delay_pot_pouri).

:- begin_tests(delay_ac, [sto(rational_trees)]).

xsb_test(weidong2).
xsb_test(weidong3).
xsb_test(weidong4).
xsb_test(weidong5).
xsb_test(weidong6).
xsb_test(weidong7).
xsb_test(weidong8).
xsb_test(interp12).
xsb_test(interp13).
xsb_test(interp14).
xsb_test(interp15).

:- end_tests(delay_ac).

:- begin_tests(delay_misc, [sto(rational_trees)]).

xsb_test(avoid_flounder).
xsb_test(residual1).
xsb_test(fa).
xsb_test(delay_var).
xsb_test(tabsimp_seq).
xsb_test(wmay_winbug).
xsb_test(ac_tests).

:- end_tests(delay_misc).
