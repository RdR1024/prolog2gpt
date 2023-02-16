# Welcome to the prolog2gpt wiki

prolog2gpt is (will be) a SWI Prolog library to interface to the GPT API.

## Introduction

Large Language Models (LLM) like GPT have greatly advanced natural language processing in recent years. However, they can benefit from interfacing with other types of reasoning modules, including logic engines. See for example the discussions in "Faithful Chain-of-Thought Reasoning" [[1]](#1) and "FOLIO: Natural Language Reasoning with First-Order Logic" [[2]](#2).

Currently, there are interface libraries to GPT for Python and NodeJS, but not Prolog. The work in this repo seeks to address that gap by building a library for SWI Prolog.


## References
[[1]]: Lyu, Q., Havaldar, S., Stein, A., Zhang, L., Rao, D., Wong, E., Apidianaki, M. and Callison-Burch, C., 2023. Faithful Chain-of-Thought Reasoning. arXiv preprint https://arxiv.org/pdf/2301.13379.

[[2]]: Han, S., Schoelkopf, H., Zhao, Y., Qi, Z., Riddell, M., Benson, L., Sun, L., Zubova, E., Qiao, Y., Burtell, M. and Peng, D., 2022. Folio: Natural language reasoning with first-order logic. https://arxiv.org/pdf/2209.00840.