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

% check the availability of GPT models
test(models,[nondet]):-
    format('test "models"~n',[]),
    gpt_models(Models),
    format('  Models: ~w~n',[Models]).

% get the details of a named model
test(a_model):-
    format('test getting model details~n',[]),
    gpt_models('text-davinci-003',Details),
    format('text-davinci-003 details:~n~w~n',[Details]).

% basic check of text completion
test(completion01,[nondet]):-
    format('test basic completion~n',[]),
    gpt_completions('text-davinci-003','My favourite animal is ',Text,[]),
    format('Resulting text: ~w~n',Text).

% basic check of text edit
test(edits01,[nondet]):-
    format('test basic edits~n',[]),
    gpt_edits('text-davinci-edit-001','Fix spelling mistakes',Text,
        [   input='What day of the wek is it?'
        ]),
    format('Resulting text: ~w~n',Text).

% basic check of image generation
test(image_create01,[nondet]):-
    format('test image creation~n',[]),
    gpt_images_create('A cute baby sea otter',Result,[]),
    format('Image url: ~w~n',Result).

:- end_tests(prolog2gpt).