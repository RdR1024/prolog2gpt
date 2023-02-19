/** <module> Prolog interface to GPT

# Introduction

This module provides prolog predicates to call the GPT API.  

Large Language Models (LLM) like GPT essentially predict what text comes next, based on
learning the (latent) probabilistic relationships between text tokens.  By training the
model on massive samples of text, the natural language capabilities have improved
dramatically in recent years.

However, such language models can benefit from interaction with other types of modules,
such as logic engines.  To aid in developing such interactions, this library aims
to make it easy to interact with GPT directly from Prolog, using predicates that call
the GPT API.

The Prolog predicates are based on the OpenAI API Reference: https://platform.openai.com/docs/api-reference

# Usage

1. Create your GPT account at https://platform.openai.com
2. Create an API key at https://platform.openai.com/account/api-keys and save that key
   somewhere (e.g. in a text file in a secure folder) as instructed.
3. Set an environment variable called `GPTKEY` to the key value
4. Use the `prolog2gpt.pro` Prolog module as usual


*/
:- module(prolog2gpt,[
   init_gptkey/0,
   gpt_models/1,
   gpt_completions/5
    
]).
:- use_module('file_path_name_ext.pro').
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

%% init_gptkey is semidet.
%  Get the GPT API Key from the environment variable and create
%  a prolog flag (`gptkey`) with the key value.
%
%  Example use:
%  ~~~
%  :- init_gptkey, current_prolog_flag(gptkey,Key), writeln(Key).
%  Key = sk-manycharactersoftheacturalkeyvalue
%  ~~~
%
init_gptkey:-
   getenv('GPTKEY',Key),
   create_prolog_flag(gptkey,Key,[type(atom)]).

%% gpt_models(-Models:json) is semidet.
%  Get a list of the available GPT models
%
%  @arg Models    The list of models as a JSON term
%
%  Example use:
%  ~~~
%  :- gpt_models(Models).
%  Models = ...  % the JSON term
%  ~~~
%
gpt_models(Models):-
   current_prolog_flag(gptkey,Key),
   http_get('https://api.openai.com/v1/models',Models,
            [authorization(bearer(Key)),application/json]).

%% gpt_completions(+Model:atom, +Prompt:atom, -Result:text, +Options:list) is semidet.
%% gpt_completions(+Model:atom, +Prompt:atom, -Result:term, ?Raw:boolean,+Options:list) is semidet.
%  Get a prompted text completion from a GPT model
%
%  @arg Model        The GPT model name, e.g. "text-davinci-003"
%  @arg Prompt       The prompt that GPT will complete
%  @arg Result       The text result, or json term with the result from GPT
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be the text result
%  @arg Options      The model completion options as list of json pair values (see below)
%
%
%  Options:
%  * temperature=N     
%    Controls "randomness" of output, with `0<=N<=1`.
%    Higher temperature means text will be more diverse, but
%    also risks more grammar mistakes and nonsense.
%  * max_tokens=M    
%    The size of output, where `M` is a natural number (incl. 0). 
%    GPT-3 can theoretically return up to 4096 tokens, but in practice less than half that. 
%    One token is about 4 characters or 0.75 average word length.
%
%  Example use:
%  ~~~
%  :- gpt_completions('text-davinci-003','My favourite animal is ',Result,_,[]),
%  Result = "a dog"
%  ~~~
%
gpt_completions(Model,Prompt,Result,Options):- gpt_completions(Model,Prompt,Result,false,Options).
gpt_completions(Model,Prompt,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key),
   atom_json_term(D,json([model=Model,prompt=Prompt|Options]),[]),
   Data = atom(application/json,D),
   http_post('https://api.openai.com/v1/completions',Data,json(ReturnData),
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> member((choices=[json([text=Result|_])|_]),ReturnData)
   ;  Result= json(ReturnData)
   ).