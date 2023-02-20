:- module(prolog2gpt,[
   init_gptkey/0,
   gpt_models/1,
   gpt_models/2,
   gpt_completions/4,
   gpt_completions/5
    
]).
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
2. Create an API key at https://platform.openai.com/account/api-keys and, as instructed, save that key
   somewhere (e.g. in a text file in a secure folder).
3. Set an environment variable called `GPTKEY` to the key value (don't forget in Linux that if you added the environment variable to your bash startup script, e.g. `~/.bashrc`, then you need to `source` your `~/.bashrc` script to activate the new environment variable).
4. Use the `prolog2gpt.pro` Prolog module as usual

@author Richard de Rozario
@license MIT
*/

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

%% init_gptkey is semidet.
%  Get the GPT API Key from the environment variable named `GPTKEY` and create
%  a prolog flag (`gptkey`) with the key value. Note: execute this predicate
%  before using any of the others, because the gpt key is needed for authorization
%  with each gpt api call.
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
%  Example use:
%  ~~~
%  :- gpt_models(Models).
%  Models = ...  % the JSON term
%  ~~~
%
%  @arg Models    The list of models as a JSON term
%
gpt_models(Models):-
   current_prolog_flag(gptkey,Key),
   http_get('https://api.openai.com/v1/models',Models,
            [authorization(bearer(Key)),application/json]).

%% gpt_models(+Model:atom, -ModelDetails:json) is semidet.
%  Get the details of a particular model
%
%  Example use:
%  ~~~
%  :- gpt_models('text-davinci-003',Details).
%  Details = ...  % the JSON term
%  ~~~
%
%  @arg Model     The model name, Note: put names that end with numeric suffixes in 
%                 single quotes, to avoid the numeric being treated as a number. 
%                 For example, use `'text-davinci-003'`
%  @arg Details   The details of the model as a JSON term
%
gpt_models(Model,Details):-
   current_prolog_flag(gptkey,Key),
   atomic_concat('https://api.openai.com/v1/models/',Model,URL),
   http_get(URL,Details,[authorization(bearer(Key)),application/json]).


%% gpt_completions(+Model:atom, +Prompt:atom, -Result:text, +Options:list) is semidet.
%% gpt_completions(+Model:atom, +Prompt:atom, -Result:term, ?Raw:boolean,+Options:list) is semidet.
%  Get a prompted text completion from a GPT model.
%
%  Example use:
%  ~~~
%  :- gpt_completions('text-davinci-003','My favourite animal is ',Result,_,[]),
%  Result = "a dog"
%  ~~~
%
%  @arg Model        The GPT model name, Note: put names that end with numeric suffixes in 
%                    single quotes, to avoid the numeric being treated as a number. 
%                    For example, use `'text-davinci-003'`
%  @arg Prompt       The prompt that GPT will complete
%  @arg Result       The text result, or json term with the result from GPT
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be the text completion result
%  @arg Options      The model completion options as list of json pair values (see below)
%
%
%  Options (Note option descriptions are mostly from the GPT API reference -- see the https://platform.openai.com/docs/api-reference for up-to-date and further details):
%  * suffix=S
%    A string (S) that is inserted after the completion
%  * max_tokens=M    
%    The size of output, where `M` is a natural number (incl. 0).
%    GPT-3 can theoretically return up to 4096 tokens, but in practice less than half that. 
%    One token is about 4 characters or 0.75 average word length. Defaults to 16.
%  * temperature=N     
%    Controls "randomness" of output, with `0<=N<=2`. Defaults to 1.
%    Higher temperature means text will be more diverse, but
%    also risks more grammar mistakes and nonsense. Recommended to
%    change either this or `top_p`, but not both.
%  * top_p
%    An alternative to sampling with `temperature`, called
%    nucleus sampling, where the model considers the results
%    of the tokens with `top_p` probability mass. So 0.1 means
%    only the tokens comprising the top 10% probability mass are
%    considered.  Use this, or `temperature`, but not both.
%    Defaults to 1.
%  * n=N
%    The number of completions (e.g. Results) to generate for each
%    prompt. Defaults to 1.
%  * stream=TF
%    If `true` then tokens will be sent as data-only 
%    `server-sent events` as they become available, with the
%    stream terminated by a `data: [DONE]` message. 
%    Defaults to `false`
%  * logprobs=N
%    Include the log probabilities on most likely tokens. For
%    example, if `logprobs=5`, the API will return a list of
%    the 5 most likely tokens. The API will always return the
%    log probability of the sampled token, so there may be up 
%    to `logprobs+1` elements in the response. Defaults to 0.
%  * echo=TF
%    If `true`, echo back the prompt in addition to 
%    the completion. Defaults to `false`
%  * stop=S
%    (string or list of strings). Up to 4 strings ("sequences")
%    where the API will stop generating further tokens. The 
%    returned text will not contain the stop sequences. Defaults
%    to `null`
%  * presence_penalty=N
%    Number between -2.0 and 2.0.  Positive values penalize new
%    tokens based on whether they appear in the text so far, 
%    increase the model's likelihood to talk about new topics.
%    Defaults to 0.
%  * frequency_penalty=N
%    Number between -2.0 and 2.0. Positive values penalize new
%    tokens based on their existing frequency in the text so far,
%    decreasing the model's likelihood to repeat the same line
%    verbatim. Defaults to 0.
%  * best_of=N
%    Generates best_of completions server-side and returns the "best" 
%    (the one with the highest log probability per token). Results cannot be streamed.
%
%    When used with `n`, `best_of` controls the number of candidate completions 
%    and `n` specifies how many to return – `best_of` must be greater than `n`.
%
%    Note: Because this parameter generates many completions, it can quickly consume 
%    your token quota. Use carefully and ensure that you have reasonable settings for 
%    `max_tokens` and `stop`.
%  * logit_bias=JSON_TERM
%    Modify the likelihood of specified tokens appearing in the completion.
%
%    Accepts a json object that maps tokens (specified by their token ID in the 
%    GPT tokenizer) to an associated bias value from -100 to 100. You can use this 
%    tokenizer tool (which works for both GPT-2 and GPT-3) to convert text to token IDs. 
%    Mathematically, the bias is added to the logits generated by the model prior to sampling. 
%    The exact effect will vary per model, but values between -1 and 1 should decrease or 
%    increase likelihood of selection; values like -100 or 100 should result in a ban or 
%    exclusive selection of the relevant token.
%
%    As an example, you can pass `json("50256": -100)` to prevent the `<|endoftext|>` token 
%    from being generated.
%  * user=S
%    A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
%
gpt_completions(Model,Prompt,Result,Options):- 
   gpt_completions(Model,Prompt,Result,false,Options).

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

