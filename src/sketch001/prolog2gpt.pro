:- module(prolog2gpt,[
   init_gptkey/0,
   gpt_models/1, gpt_models/2,
   gpt_models_detail/2, 
   gpt_extract_data/4,
   gpt_extract_fields/3,
   gpt_completions/4, gpt_completions/5,
   gpt_edits/4, gpt_edits/5,
   gpt_images_create/3, gpt_images_create/4,
   gpt_images_edits/4, gpt_images_edits/5,
   gpt_images_variations/3, gpt_images_variations/4,
   gpt_embeddings/4, gpt_embeddings/5,
   gpt_files/1, gpt_files/2
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

%% gpt_models(-Models:list) is semidet.
%% gpt_models(-Models:json,+Raw:boolean) is semidet.
%  Get a list of the available GPT models
%
%  Example use:
%  ~~~
%  :- gpt_models(Models).
%  Models = [babbage,davinci,...]
%  ~~~
%
%  @arg Models    The list of model names, or JSON term with model details
%  @arg Raw       If `true` then Models is the raw json result, else Models is a list of model names
%
gpt_models(Models):- gpt_models(Models,false).
gpt_models(Models,Raw):-
   current_prolog_flag(gptkey,Key),
   http_get('https://api.openai.com/v1/models',Ms,
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> gpt_extract_data(data,id,Ms,Models)
   ;  Models=Ms
   ).


%% gpt_models_detail(+Model:atom, -ModelDetails:json) is semidet.
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
gpt_models_detail(Model,Details):-
   current_prolog_flag(gptkey,Key),
   atomic_concat('https://api.openai.com/v1/models/',Model,URL),
   http_get(URL,Details,[authorization(bearer(Key)),application/json]).


%% gpt_extract_data(+Group:atom,+Fielname:atom,+Data:json,-Result:list) is semidet.
%  Extract a list of field data from a gpt json structure.  Note: this predicate
%  makes some simple assumptions about how GPT API result data is structured.
%
%  Example use:
%  ~~~
%  :- gpt_models(Ms,true), gpt_extract_data(data,id,Ms,Models).
%  Models = ["babbage","text-davinci-001",...]
%  ~~~
%
%  @arg Group     The GPT data group name. e.g. `data`, `choices`,...
%  @arg Fieldname The name of the field whose data we want
%  @arg Data      The json data list from the GPT API, that contains one or more field values
%  @arg Result    The resulting list of data values
gpt_extract_data(Group,Fieldname,json(Data),Result):-
   member(Group=Fieldlist,Data),
   gpt_extract_fields(Fieldname,Fieldlist,Result).

%% gpt_extract_fields(+Fieldname:atom,+Data:json,-Result:list) is semidet.
%  Extract a list of field data from a gpt json structure.  Note: this predicate
%  makes some simple assumptions about how GPT API result data is structured.
%
%  Example use:
%  ~~~
%  :- Data=[json([id="babbage",object="model"]),json([id='text-davinci-001',object="model"])], gpt_extract_data(data,id,Ms,Models).
%  Models = ["babbage","text-davinci-001"]
%  ~~~
%
%  @arg Fieldname The name of the field whose data we want
%  @arg Data      The list with json data from the GPT API, that contains one or more field values
%  @arg Result    The resulting list of data values
gpt_extract_fields(_,[],[]):-!.
gpt_extract_fields(Fieldname,[json(Fields)|Fs],[R|Results]):-
   (member(Fieldname=R,Fields);true),!,
   gpt_extract_fields(Fieldname,Fs,Results).

            
%% gpt_completions(+Model:atom, +Prompt:atom, -Result:text, +Options:list) is semidet.
%% gpt_completions(+Model:atom, +Prompt:atom, -Result:term, ?Raw:boolean,+Options:list) is semidet.
%  Get a prompted text completion from a GPT model.
%
%  Example use:
%  ~~~
%  :- gpt_completions('text-davinci-003','My favourite animal is ',Result,_,[]),
%  Result = ['a dog']
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
   gpt_completions(Model,Prompt,Result,false,Options),!.

gpt_completions(Model,Prompt,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key),
   atom_json_term(D,json([model=Model,prompt=Prompt|Options]),[]),
   Data = atom(application/json,D),
   http_post('https://api.openai.com/v1/completions',Data,ReturnData,
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> gpt_extract_data(choices,text,ReturnData,Result)
   ;  Result= ReturnData
   ).

%% gpt_edits(+Model:atom, +Instruction:atom, -Result:text, +Options:list) is semidet.
%% gpt_edits(+Model:atom, +Instruction:atom, -Result:term, ?Raw:boolean,+Options:list) is semidet.
%  Get a new edit for a given model, input and instruction.  
%  Note: Only for the 'text-davinci-edit-001' or 'code-davinci-edit-001' models.
%
%  Example use:
%  ~~~
%  :- gpt_edit('text-davinci-001','Fix the spelling mistakes',Result,_,
%              [  input='What day of the wek is it?'
%              ]),
%  Result = "What day of the week is it?"
%  ~~~
%
%  @arg Model        The GPT model name, Note: put names that end with numeric suffixes in 
%                    single quotes, to avoid the numeric being treated as a number. 
%                    For example, use `'text-davinci-003'`
%  @arg Instruction  The natural language editing instruction.
%  @arg Result       The text result, or json term with the result from GPT
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be the (first) text result
%  @arg Options      The edit options as list of json pair values (see below)
%
%
%  Options (Note option descriptions are mostly from the GPT API reference -- see the https://platform.openai.com/docs/api-reference for up-to-date and further details):
%  * input=S
%    An atom of text (S) that the model needs to edit. Default=''
%  * max_tokens=M    
%    The size of output, where `M` is a natural number (incl. 0).
%    GPT-3 can theoretically return up to 4096 tokens, but in practice less than half that. 
%    One token is about 4 characters or 0.75 average word length. Defaults to 16.
%  * n=N
%    The number of completions (e.g. Results) to generate for each
%    prompt. Defaults to 1.
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
gpt_edits(Model,Instruction,Result,Options):- 
   gpt_edits(Model,Instruction,Result,false,Options),!.

gpt_edits(Model,Instruction,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key),
   atom_json_term(D,json([model=Model,instruction=Instruction|Options]),[]),
   Data = atom(application/json,D),
   http_post('https://api.openai.com/v1/edits',Data,ReturnData,
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> gpt_extract_data(choices,text,ReturnData,Result)
   ;  Result= ReturnData
   ).

%% gpt_images_create(+Prompt:atom, -Result:term, +Options:list) is semidet.
%% gpt_images_create(+Prompt:atom, -Result:term, ?Raw:boolean,+Options:list) is semidet.
%  Create an image from a text prompt.
%
%  Example use:
%  ~~~
%  :- gpt_images_create('A cute baby sea otter',Result,_,[]),
%  Result = ['https://...'] % url of the resulting image
%  ~~~
%
%  @arg Prompt       The prompt that GPT will complete
%  @arg Result       The text result, or json term with the result from GPT
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be the (first) url or b64 result
%  @arg Options      The edit options as list of json pair values (see below)
%
%
%  Options (Note option descriptions are mostly from the GPT API reference -- see the https://platform.openai.com/docs/api-reference for up-to-date and further details):
%  * n=N
%    The number of images to generate. Defaults to 1.
%  * size=Z
%    The size of the image. Must be one of `'256x256'`, `'512x512'`, or `'1024x1024'`. 
%    Default is `'1024x1024'`
%  * response_format=S
%    The format of the generated images. Must be one of `url` or `b64_json`. Default is `url`
%  * user=S
%    A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
%
gpt_images_create(Prompt,Result,Options):-
   gpt_images_create(Prompt,Result,false,Options).
gpt_images_create(Prompt,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key),
   atom_json_term(D,json([prompt=Prompt|Options]),[]),
   Data = atom(application/json,D),
   http_post('https://api.openai.com/v1/images/generations',Data,ReturnData,
            [authorization(bearer(Key)),application/json]),
   ( member(response_format=Format,Options) -> true ; Format=url ),
   (  Raw=false
   -> gpt_extract_data(data,Format,ReturnData,Result)
   ;  Result= ReturnData
   ).

%% gpt_images_edits(+Prompt:atom, -Result:term,+Options:list) is semidet.
%% gpt_images_edits(+Prompt:atom, -Result:term, ?Raw:boolean,+Options:list) is semidet.
%  Modify an image from a text prompt.
%
%  Example use:
%  ~~~
%  :- gpt_images_edits('A cute baby sea otter with a hat','./test/otter.png',Result,_,[]),
%  Result = ['https://...'] % url of the resulting image
%  ~~~
%
%  @arg Prompt       The prompt that GPT will complete
%  @arg File         The path/filename of the image to edit
%  @arg Result       The text result, or json term with the result from GPT
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be the (first) url or b64 result
%  @arg Options      The edit options as list of pair values (see below)
%
%
%  Options (Note option descriptions are mostly from the GPT API reference -- see the https://platform.openai.com/docs/api-reference for up-to-date and further details):
%  * n=N
%    The number of images to generate. Defaults to 1.
%  * size=Z
%    The size of the image. Must be one of `'256x256'`, `'512x512'`, or `'1024x1024'`. 
%    Default is `'1024x1024'`
%  * mask
%    An additional image whose fully transparent areas (e.g. where alpha is zero) 
%    indicate where image should be edited. Must be a valid 
%    PNG file, less than 4MB, and have the same dimensions as image.
%  * response_format=S
%    The format of the generated images. Must be one of `url` or `b64_json`. Default is `url`
%  * user=S
%    A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
%
gpt_images_edits(Prompt,Image,Result,Options):-
   gpt_images_edits(Prompt,Image,Result,false,Options).
gpt_images_edits(Prompt,Image,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key),
   Data = form_data([prompt=Prompt,image=file(Image)|Options]),
   http_post('https://api.openai.com/v1/images/edits',Data,ReturnData,
            [authorization(bearer(Key)),application/json]),
   ( member(response_format=Format,Options) -> true ; Format=url ),
   (  Raw=false
   -> gpt_extract_data(data,Format,ReturnData,Result)
   ;  Result= ReturnData
   ).

%% gpt_images_variations(+File:atom, -Result:term,+Options:list) is semidet.
%% gpt_images_variations(+File:atom, -Result:term, ?Raw:boolean,+Options:list) is semidet.
%  Produce variation(s) of an image.
%
%  Example use:
%  ~~~
%  :- gpt_images_variations('./test/otter.png',Result,_,[]),
%  Result = ['https://...'] % url of the resulting image
%  ~~~
%
%  @arg Image        The path/filename of image to vary
%  @arg Result       The text result, or json term with the result from GPT
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be the (first) url or b64 result
%  @arg Options      The edit options as list of pair values (see below)
%
%
%  Options (Note option descriptions are mostly from the GPT API reference -- see the https://platform.openai.com/docs/api-reference for up-to-date and further details):
%  * n=N
%    The number of images to generate. Defaults to 1.
%  * size=Z
%    The size of the image. Must be one of `'256x256'`, `'512x512'`, or `'1024x1024'`. 
%    Default is `'1024x1024'`
%  * response_format=S
%    The format of the generated images. Must be one of `url` or `b64_json`. Default is `url`
%  * user=S
%    A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
%
gpt_images_variations(Image,Result,Options):-
   gpt_images_variations(Image,Result,false,Options).

gpt_images_variations(Image,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key),
   Data = form_data([image=file(Image)|Options]),
   http_post('https://api.openai.com/v1/images/variations',Data,ReturnData,
            [authorization(bearer(Key)),application/json]),
   ( member(response_format=Format,Options) -> true ; Format=url ),
   (  Raw=false
   -> gpt_extract_data(data,Format,ReturnData,Result)
   ;  Result= ReturnData
   ).

%% gpt_embeddings(+Input:text,-Result:list) is semidet.
%% gpt_embeddings(+Input:text,-Result:list,+Raw:boolean) is semidet.
%  Get a vector representation of a given input that can be easily consumed by machine learning models and algorithms.
%
%  Example use:
%  ~~~
%  :- gpt_embeddings('text-embedding-ada-002','The food was delicious',Result),
%  Result = [0.0023064255,-0.009327292,...]
%  ~~~
%
%  @arg Input        Atom, string, or list of such
%  @arg Result       List of file names, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
%  Options (Note option descriptions are mostly from the GPT API reference -- see the https://platform.openai.com/docs/api-reference for up-to-date and further details):
%  * user=S
%    A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
%
gpt_embeddings(Model,Input,Result,Options):-
   gpt_embeddings(Model,Input,Result,false,Options),!.
gpt_embeddings(Model,Input,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key),
   atom_json_term(D,json([model=Model,input=Input|Options]),[]),
   Data = atom(application/json,D),
   http_post('https://api.openai.com/v1/embeddings',Data,ReturnData,
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> gpt_extract_data(data,embedding,ReturnData,Result)
   ;  Result= ReturnData
   ).


%% gpt_files(-Result:list) is semidet.
%% gpt_files(-Result:list,+Raw:boolean) is semidet.
%  List all files that belong to the user's organization.
%
%  Example use:
%  ~~~
%  :- gpt_files(Result),
%  Result = ['puppy.png','hat.png']
%  ~~~
%
%  @arg Result       List of file names, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
gpt_files(Result):-
   gpt_files(Result,false).
gpt_files(Result,Raw):-
   current_prolog_flag(gptkey,Key),
   http_get('https://api.openai.com/v1/files',ReturnData,
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> gpt_extract_data(data,filename,ReturnData,Result)
   ;  Result= ReturnData
   ).



% TODO: gpt_embeddings 
% TODO: gpt_fine_tunes
% TODO: gpt_moderations


