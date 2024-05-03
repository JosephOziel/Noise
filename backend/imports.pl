:- module(imports, [each_mod/3]).

:- working_directory(_, "../frontend").
:- use_module(parser).
:- use_module(library(readutil), [read_file_to_string/3]).

%import each module from the list
each_mod([], CTX, CTX).
each_mod([File|Rest], CTX, NCTX) :-
    parse_mod(File, AST),
    new_ctx(AST, CTX, NCTX1),
    each_mod(Rest, NCTX1, NCTX).

% implement parsing the modules a module imports, and adding them to a context
parse_mod(File, AST) :-
    (   (string_codes(File, L), member(0'/, L)) ->
        (   read_file_to_string(File, Code, []), parse(Code, prog(_, AST)))
    ;   (string_concat("../std", File, Path), read_file_to_string(Path, Code, [])) ->
        parse(Code, prog(_, AST))
%   ; (atom_string(A, File), throw(error(format("unknown import: imported module ~a not found", A))))
    ).


new_ctx([], CTX, CTX).
new_ctx([def(pub, Name, Body, _)|Rest], CTX, NCTX) :-
    atom_string(N, Name),
    WCTX = CTX.put(N, wd(Body, WCTX)),
    NCTX0 = CTX.put(N, wd(Body, WCTX)),
    new_ctx(Rest, NCTX0, NCTX).
new_ctx([_|Rest], CTX, NCTX) :-
    new_ctx(Rest, CTX, NCTX).
