% Load the CHAT80 natural language system   from  GitHub. After loading,
% try these goals:
%
%    ?- test_chat.
%       Run the test suite, holding 23 questions.
%    ?- chat80:hi.
%       Run CHAT80 interactively. You may run   `?- edit(ed)` to see the
%       test  questions  and  get  some   idea.  Questions  are  entered
%       naturally, i.e., as a sentence ending with `?`.

:- use_module('https://raw.githubusercontent.com/JanWielemaker/chat80/refs/heads/master/prolog/chat80.pl').
