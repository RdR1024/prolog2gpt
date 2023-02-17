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

# Usage

1. Create your GPT account at https://platform.openai.com
2. Create an API key at https://platform.openai.com/account/api-keys and save that key
   somewhere (e.g. in a text file in a secure folder) as instructed.
3. Set an environment variable called `GPTKEY` to the key value
4. Use the `prolog2gpt.pro` Prolog module as usual

*/
:- module(prolog2gpt,[
   init_gptkey/0,
   gpt_models/1
    
]).
:- use_module('file_path_name_ext.pro').
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_json)).
%:- use_module(library(http/json_convert)).

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
%  @arg Models    The list of models as a JSON dict
%
%  Example use:
%  ~~~
%  :- gpt_models(Models).
%  Models = ...  % the JSON dict
%  ~~~
%
gpt_models(Models):-
   current_prolog_flag(gptkey,Key),
   http_get('https://api.openai.com/v1/models',Models,
            [authorization(bearer(Key)),application/json,json_object(dict)]).