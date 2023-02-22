# prolog2gpt
SWI Prolog library to interface to the GPT API.

# Introduction
Large Language Models (LLM) like GPT have greatly advanced natural language processing in recent years.  However, they can benefit from interfacing with other types of reasoning modules, including logic engines (see for example the discussions in "Faithful Chain-of-Thought Reasoning" 2023 by Lyu et al or "FOLIO: Natural Language Reasoning with First-Order Logic" 2022 by Han et al).

Currently, there are interface libraries to GPT for Python and NodeJS, but not Prolog.  The work in this repo seeks to address that gap by building a library for SWI Prolog.

# Current Status
Pre-alpha: Work has only just started.

Most of the API is working and you can access GPT with simple prolog predicates. Have a look at the code documentation (see below) for examples.  Also see the unit tests in `src/test/test001.pro`

# Install

First, make sure your GPT API key is set in the environment variable `GPTKEY`. Do this:

1. Create your GPT account at https://platform.openai.com
2. Create an API key at https://platform.openai.com/account/api-keys and, as instructed, save that key
   somewhere (e.g. in a text file in a secure folder).
3. Set an environment variable called `GPTKEY` to the key value (don't forget in Linux that if you added the environment variable to your bash startup script, e.g. `~/.bashrc`, then you need to `source` your `~/.bashrc` script to activate the new environment variable).

Next, there are two ways to install.

Firstly, as a pack from within Prolog, and using it as a library module

~~~
:- pack_install('prolog2gpt').
:- use_module(library(prolog2gpt)).

% Now test that the pack installed and execute a first call to GPT
:- init_gptkey.
:- gpt_completions('text-davinci-03','Say hello',Answer,[]).

~~~

Otherwise, you can also just clone the git repository.

~~~
$ git clone https://github.com/RdR1024/prolog2gpt
~~~

Then `cd` into `prolog2gpt/src/prolog and launch`, execute `swipl` and try the following:

~~~
:- [prolog2gpt].
:- init_gptkey.  % this makes the gpt key available to the gpt api predicates
:- gpt_completions('text-davinci-003','My favourite animal is ',Text,[max_tokens=30]).

~~~

# Usage
The prolog predicates mostly follow the GPT API (see https://platform.openai.com/docs/api-reference).  However, there is usually a wrapper that returns the results from the API call into something Prolog-friendly (rather than leaving the results as a complex JSON data structure).

For example, `gpt_completions(Model,Prompt,Result,Options)` will return a Prolog list of generated texts (called "completions" in GPT speak). If you want the returned JSON results instead, you can try 
`gpt_completions(Model,Prompt,Result,Raw,Options)` where `Raw` is set to `true`.  In this case, the `Result` will be a json term structure.  Most of the api predicates have this same "Raw" option.


# Repository structure
This repository has the following structure:

- `docs`     contains literature and additional documentation. A special subdirectory called `wiki` contains the source files for the separate github wiki repository (`github.com:RdR1024/prolog2gpt.wiki.git`). Note: I haven't populated this yet.
- `rel` 	    contains the periodic releases of the library. Users should use this directory to download stable copies of library.
- `src`	    contains the source code.
  - `prolog` contains the prolog source code
- `archive`  old material that we keep for reference
- `test`     testing files for the source code


# Documentation
You can read the individual predicate comments in the source files (e.g. `prolog2gpt.pro`), or start the SWI-Prolog document server as follows:

~~~
:- doc_server(3030).
:- portray_text(true).
:- ['prolog2gpt.pro'].
:- doc_server.
~~~

This should launch a web browser with the documentation for the `prolog2gpt.pro` file.

# Known Issues
Three of the APIs don't work.  Luckily, they are not critical APIs, because there are
workarounds. The problem seems to be with the way the URLs are formulated. They are all
URLs that end in /{id}/{instruction} I'm investigating the issues, but so far no luck.  
The problem APIs are:

* `files retrieve content` (GET https://api.openai.com/v1/files/{file_id}/content)
   The workaround at the moment is to keep a copy of the file content on your own
   computer.  The file that it is referring to was by definition uploaded by you
   previously.
* `fine-tunes cancel` (POST https://api.openai.com/v1/fine-tunes/{fine_tune_id}/cancel)
   The workaround is to just let the job complete, and then delete it
* `fine-tunes events` (GET https://api.openai.com/v1/fine-tunes/{fine_tune_id}/events)
   The workaround is to get the JSON data from the plain fine-tunes API and then search
   within it for the events within the structure for {fine_tune_id}.
