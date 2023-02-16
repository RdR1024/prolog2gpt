# prolog2gpt
SWI Prolog library to interface to the GPT API.

# Introduction
Large Language Models (LLM) like GPT have greatly advanced natural language processing in recent years.  However, they can benefit from interfacing with other types of reasoning modules, including logic engines (see for example the discussions in "Faithful Chain-of-Thought Reasoning" 2023 by Lyu et al or "FOLIO: Natural Language Reasoning with First-Order Logic" 2022 by Han et al).

Currently, there are interface libraries to GPT for Python and NodeJS, but not Prolog.  The work in this repo seeks to address that gap by building a library for SWI Prolog.

# Current Status
Pre-alpha: Work has only just started

# Repository structure
This repository has the following structure:

- `docs`  contains literature and additional documentation. A special subdirectory called `wiki` contains the source files for the separate github wiki repository (`github.com:RdR1024/prolog2gpt.wiki.git`).
- `rel` contains the periodic releases of the library. Users should use this directory to download stable copies of library.
- `src`	contains the source code, with various prototypes in subdirectories called "sketches". At some point, a prototype may become a "production" version (i.e. release with a version number), but the philosophy is one of continuous iterative prototyping.
   - `sketch001`   source code for the first prototype
      - `archive`  old material that we keep for reference
      - `test`     testing files for the prototype