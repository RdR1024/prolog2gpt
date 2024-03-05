:- module(prolog2gpt,[
   init_gptkey/0,
   gpt_models/1, gpt_models/2,
   gpt_models_detail/2, 
   gpt_extract_data/4,
   gpt_extract_fields/3,
   gpt_extract_field_pairs/4,
   gpt_completions/4, gpt_completions/5,
   gpt_images_create/3, gpt_images_create/4,
   gpt_images_edits/4, gpt_images_edits/5,
   gpt_images_variations/3, gpt_images_variations/4,
   gpt_embeddings/4, gpt_embeddings/5,
   gpt_files/1, gpt_files/2,
   gpt_files_upload/4, gpt_files_upload/5,
   gpt_files_delete/2, gpt_files_delete/3,
   gpt_files_retrieve/2, gpt_files_retrieve/3,
   gpt_files_retrieve_content/2, gpt_files_retrieve_content/3,
   gpt_fine_tunes/1,gpt_fine_tunes/2,gpt_fine_tunes/3, gpt_fine_tunes/4,
   gpt_fine_tunes_detail/2, gpt_fine_tunes_detail/3,
   gpt_fine_tunes_cancel/2, gpt_fine_tunes_cancel/3,
   gpt_fine_tunes_events/2, gpt_fine_tunes_events/3,
   gpt_fine_tunes_delete/2, gpt_fine_tunes_delete/3,
   gpt_moderations/3, gpt_moderations/4
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
:- use_module(library(http/json_convert)).

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
%  Models = ['babbage','text-davinci-001',...]
%  ~~~
%
%  @arg Group     The GPT data group name. e.g. `data`, `choices`,...
%  @arg Fieldname The name of the field whose data we want
%  @arg Data      The json data list from the GPT API, that contains one or more field values
%  @arg Result    The resulting list of data values
gpt_extract_data(Group, Fieldname, json(Data), Result):-
   member(Group=Fieldlist, Data),
   gpt_extract_fields(Fieldname, Fieldlist, Result).

%% gpt_extract_fields(+Fieldname:atom,+Data:json,-Result:list) is semidet.
%  Extract a list of field data from a gpt json structure.  Note: this predicate
%  makes some simple assumptions about how GPT API result data is structured.
%
%  Example use:
%  ~~~
%  :- Data=[json([id='babbage',object='model']),json([id='text-davinci-001',object='model'])], gpt_extract_data(data,id,Data,Models).
%  Models = ['babbage','text-davinci-001']
%  ~~~
%
%  @arg Fieldname The name of the field whose data we want
%  @arg Data      The list with json data from the GPT API, that contains one or more field values
%  @arg Result    The resulting list of data values
gpt_extract_fields(_,[],[]):-!.
gpt_extract_fields(Fieldname,[json(Fields)|Fs],Results):-
   (  member(Fieldname=R,Fields)
   -> Results=[R|Res]
   ;  Results=Res
   ),
   gpt_extract_fields(Fieldname,Fs,Res).

%% gpt_extract_field_pairs(+Field1:atom,+Field2:atom,+Data:json,-Result:list) is semidet.
%  Extract a list of field pairs from a gpt json structure.  Note: this predicate
%  makes some simple assumptions about how GPT API result data is structured.
%
%  Example use:
%  ~~~
%  :- Data=[json([id='123',filename=file1]),json([id='345',filename=file2])], gpt_extract_field_pairs(filename,id,Data,FieldPairs).
%  FieldPairs = [file1-'123',file2-'345']
%  ~~~
%
%  @arg Fieldname The name of the field whose data we want
%  @arg Data      The list with json data from the GPT API, that contains one or more field values
%  @arg Result    The resulting list of data values
gpt_extract_field_pairs(_,_,[],[]):-!.
gpt_extract_field_pairs(Field1,Field2,[json(Fields)|Fs],Results):-
   (  member(Field1=F1,Fields)
      -> (  member(Field2=F2,Fields)
         -> Results = [F1-F2|Res]
         ;  Results = Res
         )
      ;  Results = Res
   ),!,
  gpt_extract_field_pairs(Field1,Field2,Fs,Res).

            
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
%    As an example, you can pass `json('50256': -100)` to prevent the `<|endoftext|>` token 
%    from being generated.
%  * user=S
%    A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
%
gpt_completions(Model, Prompt, Result, Options):- 
   gpt_completions(Model, Prompt, Result, false, Options),!.

gpt_completions(Model, Prompt, Result, Raw, Options):-
   current_prolog_flag(gptkey,Key),
   
   atom_json_term(D,json([model = Model, messages = [json([role = user, content = Prompt])] | Options]),[]),
   Data = atom(application/json, D),

   http_post('https://api.openai.com/v1/chat/completions', Data, ReturnData,
            [authorization(bearer(Key)), application/json]),
   (  Raw = false
   -> (  gpt_extract_data(choices, message, ReturnData, [json(Message)]),
         member(content = Result, Message))
   ;  Result = ReturnData
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
%  Result = ['file1.jsonl'-'file-12345','file2.jsonl'-'file-56789']
%  ~~~
%
%  @arg Result       List of Filename-ID pairs, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
gpt_files(Result):-
   gpt_files(Result,false).
gpt_files(Result,Raw):-
   current_prolog_flag(gptkey,Key),
   http_get('https://api.openai.com/v1/files',json(ReturnData),
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (  member(data=Files,ReturnData),
         gpt_extract_field_pairs(filename,id,Files,Result)
      )
   ;  Result= json(ReturnData)
   ).

%% gpt_files_upload(+File:atom,+Purpose:text,-Result:list) is semidet.
%% gpt_files_upload(+File:atom,+Purpose:text,-Result:list,+Raw:boolean) is semidet.
%  Upload a JSON Lines file (typically for fine-tuning)
%
%  Example use:
%  ~~~
%  :- gpt_files_upload('./test/tune_answer.jsonl','fine-tune',Result),
%  Result = ['file-XjGxS3KTG0uNmNOK362iJua3']
%  ~~~
%
%  @arg File         Filename to upload
%  @arg Purpose      Purpose of the file. Currently only 'fine-tune'
%  @arg Result       List of file names, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
gpt_files_upload(File,Purpose,Result,Options):-
   gpt_files_upload(File,Purpose,Result,false,Options),!.
gpt_files_upload(File,Purpose,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key), 
   Data = form_data([file=file(File),purpose=Purpose|Options]),
   http_post('https://api.openai.com/v1/files',Data,json(ReturnData),
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (member(id=ID,ReturnData),Result=[ID])
   ;  Result= json(ReturnData)
   ).

%% gpt_files_delete(+FileID:atom,+Purpose:text,-Result:list) is semidet.
%% gpt_files_delete(+FileID:atom,+Purpose:text,-Result:list,+Raw:boolean) is semidet.
%  Delete a (user) file from GPT storage
%
%  Example use:
%  ~~~
%  :- gpt_files_delete('file-XjGxS3KTG0uNmNOK362iJua3',Result),
%  Result = ['file-XjGxS3KTG0uNmNOK362iJua3']
%  ~~~
%
%  @arg FileID       File ID of file in GPT storage to delete
%  @arg Purpose      Purpose of the file. Currently only 'fine-tune'
%  @arg Result       List of file names, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
gpt_files_delete(FileID,Result):-
   gpt_files_delete(FileID,Result,false),!.
gpt_files_delete(FileID,Result,Raw):-
   current_prolog_flag(gptkey,Key),
   atomic_concat('https://api.openai.com/v1/files/',FileID,URL),
   http_delete(URL,json(ReturnData),
      [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (member(id=ID,ReturnData), Result=[ID])
   ;  Result= json(ReturnData)
   ).

%% gpt_files_retrieve(+FileID:atom,-Result:list) is semidet.
%% gpt_files_retrieve(+FileID:atom,-Result:list,+Raw:boolean) is semidet.
%  Retrieve a (user) file details
%
%  Example use:
%  ~~~
%  :- gpt_files_retrieve('file-XjGxS3KTG0uNmNOK362iJua3',Result),
%  Result = ['myfile.jsonl']
%  ~~~
%
%  @arg FileID       File ID of file in GPT storage to retrieve
%  @arg Result       List with file name, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
gpt_files_retrieve(FileID,Result):-
   gpt_files_retrieve(FileID,Result,false),!.
gpt_files_retrieve(FileID,Result,Raw):-
   current_prolog_flag(gptkey,Key),
   atomic_concat('https://api.openai.com/v1/files/',FileID,URL),
   http_get(URL,json(ReturnData),
      [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (member(filename=File,ReturnData), Result=[File])
   ;  Result= json(ReturnData)
   ).

%% gpt_files_retrieve_content(+FileID:atom,+Purpose:text,-Result:list) is semidet.
%% gpt_files_retrieve(+FileID:atom,+Purpose:text,-Result:list,+Raw:boolean) is semidet.
%  Retrieve a (user) file details
%
%  Example use:
%  ~~~
%  :- gpt_files_retrieve('file-XjGxS3KTG0uNmNOK362iJua3',Result),
%  Result = ['myfile.jsonl']
%  ~~~
%
%  @arg FileID       File ID of file in GPT storage to retrieve
%  @arg Result       List with file name, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
% TODO: ***** this API doesn't work for some reason *****
gpt_files_retrieve_content(FileID,Result):-
   gpt_files_retrieve_content(FileID,Result,false),!.
gpt_files_retrieve_content(FileID,Result,Raw):-
   current_prolog_flag(gptkey,Key),
   atomic_list_concat(['https://api.openai.com/v1/files/',FileID,'/content'],URL),
   http_get(URL,ReturnData, [authorization(bearer(Key))]),
   (  Raw=false
   -> (member(filename=File,ReturnData), Result=[File])
   ;  Result= ReturnData
   ).



%% gpt_fine_tunes(+TrainingFile:text,-Result:list) is semidet.
%% gpt_fine_tunes(+TrainingFile:text,-Result:list,+Raw:boolean) is semidet.
%  Get a vector representation of a given TrainingFile that can be easily consumed by machine learning models and algorithms.
%
%  Example use:
%  ~~~
%  :- gpt_fine_tunes('file-XGinujblHPwGLSztz8cPS8XY',Result),
%  Result = ['ft-AF1WoRqd3aJAHsqc9NY7iL8F']
%  ~~~
%
%  @arg TrainingFile Atom with the GPT file ID of an uploaded file
%  @arg Result       Fine-tuned request event in list, or json term of details (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
%  Options (Note option descriptions are mostly from the GPT API reference -- see the https://platform.openai.com/docs/api-reference for up-to-date and further details):
%  * validation_file=F
%    The ID of an uploaded file that contains validation data.
%
%    If you provide this file, the data is used to generate validation 
%    metrics periodically during fine-tuning. These metrics can be viewed 
%    in the fine-tuning results file. Your train and validation data should 
%    be mutually exclusive.
%
%    Your dataset must be formatted as a JSONL file, where each validation 
%    example is a JSON object with the keys "prompt" and "completion". 
%    Additionally, you must upload your file with the purpose fine-tune.
%  * model=M
%    The name of the base model to fine-tune. You can select one of 'ada', 
%    'babbage', 'curie', 'davinci', or a fine-tuned model created after 
%    2022-04-21. To learn more about these models, see the Models documentation.
%    Defaults to 'curie'.
%  * n_epochs=N
%    The number of epochs to train the model for. An epoch refers to one full 
%    cycle through the training dataset. Defaults to 4.
%  * batch_size=N
%    The batch size to use for training. The batch size is the number of 
%    training examples used to train a single forward and backward pass.
%
%    By default, the batch size will be dynamically configured to be ~0.2% 
%    of the number of examples in the training set, capped at 256 - in 
%    general, we've found that larger batch sizes tend to work better for 
%    larger datasets. Defaults to `null`.
%  * learning_rate_multiplier=N
%    The learning rate multiplier to use for training. The fine-tuning 
%    learning rate is the original learning rate used for pretraining 
%    multiplied by this value.
%
%    By default, the learning rate multiplier is the 0.05, 0.1, or 0.2 
%    depending on final batch_size (larger learning rates tend to perform 
%    better with larger batch sizes). We recommend experimenting with 
%    values in the range 0.02 to 0.2 to see what produces the best results.
%    Defaults to `null`.
%  * prompt_loss_weight=N
%    The weight to use for loss on the prompt tokens. This controls how 
%    much the model tries to learn to generate the prompt (as compared to 
%    the completion which always has a weight of 1.0), and can add a 
%    stabilizing effect to training when completions are short.
%
%    If prompts are extremely long (relative to completions), it may make 
%    sense to reduce this weight so as to avoid over-prioritizing learning 
%    the prompt. Defaults to `0.01`
%  * compute_classification_metrics=B
%    If set, we calculate classification-specific metrics such as accuracy 
%    and F-1 score using the validation set at the end of every epoch. 
%    These metrics can be viewed in the results file.
%
%    In order to compute classification metrics, you must provide a 
%    validation_file. Additionally, you must specify classification_n_classes 
%    for multiclass classification or classification_positive_class for 
%    binary classification. Defaults to `false`
%  * classification_n_classes=N
%    The number of classes in a classification task. This parameter is 
%    required for multiclass classification. Defaults to `null`.
%  * classification_positive_class=S
%    The positive class in binary classification. This parameter is needed 
%    to generate precision, recall, and F1 metrics when doing binary 
%    classification. Defaults to `null`.
%  * classification_betas=List
%    If this is provided, we calculate F-beta scores at the specified beta 
%    values. The F-beta score is a generalization of F-1 score. This is only 
%    used for binary classification.
%
%    With a beta of 1 (i.e. the F-1 score), precision and recall are given 
%    the same weight. A larger beta score puts more weight on recall and 
%    less on precision. A smaller beta score puts more weight on precision 
%    and less on recall. Defaults to `null`.
%  * suffix=S
%    A string of up to 40 characters that will be added to your fine-tuned 
%    model name. For example, a suffix of "custom-model-name" would produce 
%    a model name like `ada:ft-your-org:custom-model-name-2022-02-15-04-21-04`.
%
gpt_fine_tunes(TrainingFile,Result,Options):-
   gpt_fine_tunes(TrainingFile,Result,false,Options),!.
gpt_fine_tunes(TrainingFile,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key),
   atom_json_term(D,json([training_file=TrainingFile|Options]),[]),
   Data = atom(application/json,D),
   http_post('https://api.openai.com/v1/fine-tunes',Data,json(ReturnData),
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> member(id=Result,ReturnData)
   ;  Result= json(ReturnData)
   ).

%% gpt_fine_tunes(-Result:list) is semidet.
%% gpt_fine_tunes(-Result:list,+Raw:boolean) is semidet.
%  Gets a list of fine-tunes jobs.
%
%  Example use:
%  ~~~
%  :- gpt_fine-tunes(Result),
%  Result = ['curie:ft-personal-2022-02-15-04-21-04'-'ft-090asf0asf0',...]
%  ~~~
%
%  @arg Result       List with file name, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
gpt_fine_tunes(Result):-
   gpt_fine_tunes(Result,false),!.
gpt_fine_tunes(Result,Raw):-
   current_prolog_flag(gptkey,Key),
   http_get('https://api.openai.com/v1/fine-tunes',json(ReturnData),
      [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (  member(data=Models,ReturnData),
         gpt_extract_field_pairs(fine_tuned_model,id,Models,Result)
      )
   ;  Result= json(ReturnData)
   ).

%% gpt_fine_tunes_detail(+ID:atom,-Result:list) is semidet.
%% gpt_fine_tunes_detail(+ID:atom,-Result:list,+Raw:boolean) is semidet.
%  Gets details of a fine-tunes job.
%
%  Example use:
%  ~~~
%  :- gpt_fine_tunes_detail('ft-090asf0asf0',Result),
%  Result = ['curie:ft-personal-2022-02-15-04-21-04']
%  ~~~
%
%  @arg Result       List with file name, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
gpt_fine_tunes_detail(ID,Result):-
   gpt_fine_tunes_detail(ID,Result,false),!.
gpt_fine_tunes_detail(ID,Result,Raw):-
   current_prolog_flag(gptkey,Key),
   atomic_concat('https://api.openai.com/v1/fine-tunes/',ID,URL),
   http_get(URL,json(ReturnData),
      [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (  member(fine_tuned_model=TunedModel,ReturnData),
         Result=[TunedModel]
      )
   ;  Result= json(ReturnData)
   ).

%% gpt_fine_tunes_cancel(+ID:atom,-Result:list) is semidet.
%% gpt_fine_tunes_cancel(+ID:atom,-Result:list,+Raw:boolean) is semidet.
%  Cancel a fine-tunes job.
%
%  Example use:
%  ~~~
%  :- gpt_fine_tunes_cancel([_-ID]),(ID,Result),
%  Result = ['curie:ft-personal-2022-02-15-04-21-04']
%  ~~~
%
%  @arg ID           ID of the fine-tunes job
%  @arg Result       List with file name, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
% TODO: ***** DOES NOT WORK **** something to do with post without data?
gpt_fine_tunes_cancel(ID,Result):-
   gpt_fine_tunes_cancel(ID,Result,false),!.
gpt_fine_tunes_cancel(ID,Result,Raw):-
   current_prolog_flag(gptkey,Key),
   atomic_list_concat(['https://api.openai.com/v1/fine-tunes/',ID,'/cancel'],URL),
   http_post(URL,[],json(ReturnData),
      [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (  member(fine_tuned_model=TunedModel,ReturnData),
         Result=[TunedModel]
      )
   ;  Result= json(ReturnData)
   ).

%% gpt_fine_tunes_events(+ID:atom,-Result:list) is semidet.
%% gpt_fine_tunes_events(+ID:atom,-Result:list,+Raw:boolean) is semidet.
%  List events of a fine-tunes job.
%
%  Example use:
%  ~~~
%  :- gpt_fine_tunes_events([_-ID]),(ID,Result),
%  Result = ['curie:ft-personal-2022-02-15-04-21-04']
%  ~~~
%
%  @arg ID           ID of the fine-tunes job
%  @arg Result       List with file name, or json term (depending on `Raw`)
%  @arg Raw          If `true` the Result will be the json term, if `false` (default)
%                    the Result will be a simple list of file names
% TODO: ***** DOES NOT WORK **** something to do with post without data?
gpt_fine_tunes_events(ID,Result):-
   gpt_fine_tunes_events(ID,Result,false),!.
gpt_fine_tunes_events(ID,Result,Raw):-
   current_prolog_flag(gptkey,Key),
   atomic_list_concat(['https://api.openai.com/v1/fine-tunes/',ID,'/events'],URL),
   http_get(URL,json(ReturnData),
      [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (  member(fine_tuned_model=TunedModel,ReturnData),
         Result=[TunedModel]
      )
   ;  Result= json(ReturnData)
   ).

%% gpt_fine_tunes_delete(+ID:atom,-Result:list) is semidet.
%% gpt_fine_tunes_delete(+ID:atom,-Result:list,+Raw:boolean) is semidet.
%  Delete a fine-tunes job from GPT storage
%
%  Example use:
%  ~~~
%  :- gpt_fine_tunes([_-ID]),gpt_fine_tunes_delete(ID,Result),
%  Result = ['ft-XjGxS3KTG0uNmNOK362iJua3']
%  ~~~
%
%  @arg ID       File ID of file in GPT storage to delete
%  @arg Purpose  Purpose of the file. Currently only 'fine-tune'
%  @arg Result   List of file names, or json term (depending on `Raw`)
%  @arg Raw      If `true` the Result will be the json term, if `false` (default)
%                the Result will be a simple list of file names
gpt_fine_tunes_delete(ID,Result):-
   gpt_fine_tunes_delete(ID,Result,false),!.
gpt_fine_tunes_delete(ID,Result,Raw):-
   current_prolog_flag(gptkey,Key),
   atomic_concat('https://api.openai.com/v1/models/',ID,URL),
   http_delete(URL,json(ReturnData),
      [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (member(id=ID,ReturnData), Result=[ID])
   ;  Result= json(ReturnData)
   ).


%% gpt_moderations(+Model:atom,+Input:text,-Result:list,+Options:list) is semidet.
%  Given a input text, outputs if the model classifies it as violating OpenAI's content policy.
%
%  Example use:
%  ~~~
%  :- gpt_moderations('I want to kill them',Result),
%  Result = [sexual=false, hate=false, violence=true, 'self-harm'=false, 
%  'sexual/minors'=false, 'hate/threatening'=false, 'violence/graphic'=false].
%  ~~~
%
%  @arg Input        Text to test for content policy violation
%  @arg Result       JSON structure with policy scores
gpt_moderations(Input,Result,Options):-
   gpt_moderations(Input,Result,false,Options).
gpt_moderations(Input,Result,Raw,Options):-
   current_prolog_flag(gptkey,Key), 
   atom_json_term(D,json([input=Input|Options]),[]),
   Data = atom(application/json,D),
   http_post('https://api.openai.com/v1/moderations',Data,ReturnData,
            [authorization(bearer(Key)),application/json]),
   (  Raw=false
   -> (  gpt_extract_data(results,categories,ReturnData,[json(R)]),
         maplist(json_pair_boolean,R,Result)
      )
   ;  Result= ReturnData
   ).

json_pair_boolean(Name='@'(Boolean),Name=Boolean):-!.
json_pair_boolean(Name=Val,Name=Val):-!.
