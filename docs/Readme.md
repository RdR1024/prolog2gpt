# The Docs Folder

The docs folder contains any relevant literature and extra documentation (e.g. user guides) for the project.  

A special subfolder called "wiki" contains the source files for the github wiki repo (github.com/<user>/<repo>.wiki.git). For this project, we assume that you have a directory (folder) called `prolog2gpt` and one called `prolog2gpt.wiki`.  You would not edit the content in `prolog2gpt.wiki` directly, but instead edit the content in `prolog2gpt/src/docs/wiki/` and then copy with `cp -R prolog2gpt/src/docs/wiki/* prolog2gpt.wiki` (and commit+push `prolog2gpt.wiki`).

This enables us to work on the documentation as "source" and only push the finished version to the wiki.
