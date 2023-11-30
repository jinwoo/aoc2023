:- module(util, []).

%   Registers the file search path for the 'input' alias.
%   input('some.txt') --> .../input/some.txt.
user:file_search_path(input, InputDir) :-
    (   current_prolog_flag(associated_file, F)
    ->  file_directory_name(F, D),
        atomic_list_concat([D, input], /, InputDir)
    ;   InputDir = input
    ).
