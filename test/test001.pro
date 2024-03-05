%  A Prolog test suite.
%  Load this file as usual (e.g. ['test001.pro']) and then
%  `:- run_tests.`
%
%  See the SWI-Prolog documentation for Prolog Unit Tests

:- begin_tests(prolog2gpt).
:- use_module('../src/prolog/prolog2gpt.pro').

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
    gpt_models_detail('gpt-4',Details),
    format('gpt-4 details:~n~w~n',[Details]).

% basic check of text completion, no response parsing
test(completion01,[nondet]):-
    format('test basic completion without response parsing~n',[]),
    gpt_completions('gpt-3.5-turbo','My favourite animal is ',Text, true, []),
    format('Resulting text: ~w~n',Text).

% basic check of text completion
test(completion02,[nondet]):-
    format('test basic completion~n',[]),
    gpt_completions('gpt-3.5-turbo','My favourite animal is ',Text, true, []),
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

% basic check of image edit
test(image_edit01,[nondet]):-
    format('test image edit~n',[]),
    gpt_images_edits('A cartoon otter with a hat','./otter.png',Result,[]),
    format('Image url: ~w~n',Result).

% basic check of image variation
test(image_variation01,[nondet]):-
    format('test image variation~n',[]),
    gpt_images_variations('./otter.png',Result,[]),
    format('Image url: ~w~n',Result).

% basic check of text embeddings
test(edits01,[nondet]):-
    format('test basic embeddings~n',[]),
    gpt_embeddings('text-embedding-ada-002','The food was delicious',Text,[]),
    format('Resulting text: ~w~n',Text).

% basic check of file upload, list, details, and delete
test(upload01,[nondet]):-
    format('test file upload~n',[]),
    gpt_files_upload('./tune_answer.jsonl','fine-tune',[ID],[]),
    format('File ID: ~w~n',[ID]),
    gpt_files_retrieve(ID,R,true),
    format('File details: ~w~n',[R]),
    gpt_files(List),
    format('File list: ~w~n',[List]),
    gpt_files_delete(ID,RDel),
    format('File deleted: ~w~n',RDel).

% basic check of moderations
test(moderations,[nondet]):-
    format('test moderations~n',[]),
    gpt_moderations('I want to kill them',R,[]),
    format('Moderation result: ~w~n',[R]).

:- end_tests(prolog2gpt).