/** <module> file_path_name_ext

A utility to construct or deconstruct file, path, name and extension
*/
:- module(filename,[
    file_path_name_ext/4,
    file_path_name_ext/5,
    lastchar/2,
    firstchar/2
]).

%%  file_path_name_ext(+File:atom,-Path:atom,-Name:atom,-Extension:atom) is semidet.
%%  file_path_name_ext(-File:atom,?Path:atom,+Name:atom,+Extension:atom) is multi.
%%  file_path_name_ext(+File:atom,+DExt:atom,-Path:atom,-Name:atom,?Extension:atom) is semidet.
%%  file_path_name_ext(-File:atom,-DExt:atom,?Path:atom,+Name:atom,+Extension:atom) is multi.
%
%   Separate a (full) URL into `Path`, `Name` and `Extension`, or construct the absolute file
%   name from `Path` (optional), `Name` and `Extension` (or `DExt`).
%
%   Note: In alignment with URI formats, filenames may end with `#`, typically in cases where 
%   the filename is used as a prefix for entities.  In this case, the `#` is stripped 
%   from the name and ignored.  
%
%   @arg File       If input, then an "expandable" file path and name. Extension is optional, if a
%                   default extension `DExt` is given. Input may optionally have a scheme prefix 
%                   of `file://`. Input may also have an ending URI entity suffix (`#`) which will
%                   be stripped. If output, then the absolute file path and name.
%   @arg Path       If input, then an "expandable" path. Trailing slashes are optional for resulting
%                   absolute file names. If output, then absolute path, including trailing slash.
%   @arg Name       Filename without extension
%   @arg DExt       Default filename extension (e.g. `txt`) without the dot prefix
%   @arg Extension  Filename extension (e.g. `txt`) without the dot prefix
%
%   Example use:
%   ~~~
%   :- file_path_name_ext('~/.config/swi-prolog/init.pl',P,N,E).
%   P = '/home/myusername/.config/swi-prolog/''
%   N = init
%   E = pl
%
%   :- file_path_name_ext(F,library(pldoc),doc_process,pl).
%   F = '/home/myusername/.config/swi-prolog/library/pldoc/doc_process.pl';
%   F = '/usr/lib/swi-prolog/library/pldoc/doc_process.pl';
%   ...etc.
%   ~~~
%
file_path_name_ext(File,Path,Name,Ext):- file_path_name_ext(File,_,Path,Name,Ext).
file_path_name_ext(Filename,DExt,Path,Name,Ext):-
    nonvar(Filename),
    absolute_file_name(Filename,Full,[expand(true)]),
    ( lastchar(Full,'#') -> After=1; After=0),  % maybe remove '#'
    ( atom_prefix(Full,'file://') -> Before=7; Before=0),  % maybe remove 'file://'
    sub_string(Full,Before,_,After,File),
    file_name_extension(PathName,X,File),
    file_base_name(File,BaseName),
    file_base_name(PathName,Name),
    (   X = ''   % check if extension is empty, but default extension exists
    ->  ( (Name= BaseName, ground(DExt)) -> Ext= DExt; Ext= X ) 
    ;   Ext = X 
    ),
    file_directory_name(File,P),
    atom_concat(P,'/',Path).
    
file_path_name_ext(Filename,DExt,Path,Name,Ex):-
    var(Filename),
    (   (atom(Name),lastchar(Name,'#'))  % maybe remove '#'
    ->  sub_string(Name,0,_,1,Nam)
    ;   Nam = Name
    ),
    ( Ex=DExt; true ),		% maybe use default extension
    (		firstchar(Ex,'.') % maybe remove extra '.' from extension
    ->	sub_string(Ex,1,_,0,Ext)
	 ;		Ext=Ex    	
    ),
    ( working_directory(Path,Path); true),
    (		lastchar(Path,'/') % maybe remove extra slash from path
    ->	sub_string(Path,0,_,1,FPath)
    ;		FPath=Path
    ),!,
    expand_file_search_path(FPath,FullPath),
    atomic_list_concat([FullPath,'/',Nam,'.',Ext],Filename).

%%  lastchar(+Text:atomic,?Char:char) is semidet.
%   Get or check the last character of a string or atom
%
%   @arg Text   string or atom
%   @arg Char   the last character of `Text`
%
%   Example use:
%   ~~~
%   :- lastchar('Hello World!',C).
%   C = '!'
%   ~~~
%
lastchar(String,Char):-
    atom_string(Text,String),
    sub_atom(Text,_,1,0,Char).

%%  firstchar(+Text:atomic,?Char:char) is semidet.
%   Get or check the first character of a string or atom
%
%   @arg String   string or atom
%   @arg Char   the first character of `Text`
%
%   Example use:
%   ~~~
%   :- fistchar('Hello World!',C).
%   C = 'H'
%   ~~~
%
firstchar(String,Char):-
    atom_string(Text,String),
    sub_atom(Text,0,1,_,Char).
