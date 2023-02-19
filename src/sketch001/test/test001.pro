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
    gpt_models(json([_,data=Models|_])),
    member(json([id='text-davinci-003'|_]),Models),
    format('  text-davinci-003 is a model.~n',[]).

% basic check of text completion
test(completion01,[nondet]):-
    format('test basic completion~n',[]),
    gpt_completions('text-davinci-003','My favourite animal is ',Text,_,[]),
    format('Resulting text: ~w~n',[Text]).

:- end_tests(prolog2gpt).