%  A Prolog test suite.
%  Load this file as usual (e.g. [test001]) and then
%  `:- run_tests.`
%
%  See the SWI-Prolog documentation for Prolog Unit Tests

:- begin_tests(prolog2gpt).
:- use_module('../prolog2gpt.pro').

% check that the GPT key is obtainable from the environment
% Note: run this test before all others, so that the key is initialized
test(init_key):-
    format('~ntest "init_key"~n',[]),
    init_gptkey,
    current_prolog_flag(gptkey,Key),
    format('   Key: ~w~n',[Key]).

% check the availability of GPT models by checking for "text-davinci-003"
test(models,[nondet]):-
    format('test "models"~n',[]),
    gpt_models(Models),
    Data=Models.data,
    member(M,Data),
    M.id="text-davinci-003".

:- end_tests(prolog2gpt).