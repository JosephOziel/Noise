:- module(parser, [
              parse/2,
              pparse/1
          ]).

:- use_module(lexer).

% WHY THE FRICK WONT THE PARSER WANT TO FAIL.

parse(Text, AST) :-
    string_codes(Text, Code),
    tokenize(Code, Out),
    program(AST, Out, []).

pparse(Code) :-
    parse(Code, AST),
    print_term(AST, []).

program(prog(Uses, [Node|Rest])) --> (use(Uses) | {Uses = []}), item(Node), (program(prog(_, Rest)) | {Rest = []}).

item(Node) --> lit(Node) | quote(Node) | pat_obj(Node) | alias(Node) | word_def(Node) | word(Node).

% item(Node) --> lit(Node), !.
% item(Node) --> quote(Node), !.
% item(Node) --> pat_obj(Node), !.
% item(Node) --> word_def(Node), !.
% item(Node) --> word(Node), !.

use(Files) --> [word_s("use", _)], files(Files), [word_s(";", _)].
files([File|Rest]) --> [word_s(File, _)], (files(Rest) | {Rest = []}).

lit(num(N, L)) --> [lit(N, L)], {number(N)}.
lit(str(S, L)) --> [lit(S, L)], {string(S)}.
lit(true(L)) --> [word_s("t", L)].
lit(false(L)) --> [word_s("f", L)].
lit(atom(A, L)) --> [lit(A, L)], {atom(A)}.
lit(_) --> !, {fail}.

word(word(right, Name, L)) --> [word(">", Name, L)].
word(word(left, Name, L)) --> [word("<", Name, L)].
word(word_s(Name, L)) --> [word_s(Name, L)], {Name \= "{", Name \= "}", Name \= ";"}.
word(_) --> !, {fail}.

% maybe add shorthand for quote with variables, and then desugar it later
quote(quote(Body, L)) --> [word_s("[", L)], program(Body), [word_s("]", _)].

% maybe allow nested objects?
pat_obj(pattern_obj(Pats, L)) --> [word_s("(", L)], pats(Pats), [word_s(")", _)].
pats([Pat|Rest]) --> pat(Pat), (pats(Rest) | {Rest = []}).

pat(X) --> [word_s(C, _)], pat2(C, X).
pat(lit(Lit)) --> [lit(Lit, _)].

pat2("[", quo_spl(left, Pats, R)) --> pats(Pats), !, [word_s("|<",_)], pat(R), [word_s("]", _)].
pat2("[", quo_spl(right, Pats, R)) --> pat(R), !, [word_s("|>",_)], pats(Pats), [word_s("]", _)].
pat2("[", quote(Quote)) --> pats(Quote), [word_s("]", _)].
pat2(Name, var(Name)) --> !, {Name \= "|>", Name \= "|<"}.

% pat(quo_spl(left, Pats, R)) --> [word_s("[", _)], pats(Pats), [word_s("|<",_)], pat(R), [word_s("]", _)]. % too complicated?, also maybe change symbol for it
% pat(quo_spl(right, Pats, R)) --> [word_s("[", _)], pat(R), [word_s("|>",_)], pats(Pats), [word_s("]", _)]. % too complicated?
% pat(quote(Quote)) --> [word_s("[", _)], pats(Quote), [word_s("]", _)].
% pat(lit(Lit)) --> [lit(Lit, _)].
% pat(var(Name)) --> [word_s(Name, _)], {\+ (Name = "|>"), \+ (Name = "|<")}.

word_def(def(Vis, Name, Body, L)) --> (([word_s("pv", _)], {Vis = pv}) ; {Vis = pub}), [word_s("{", L), word_s(Name, _)],  program(Body), [word_s("}", _)].

alias(alias(NewName, Name, L)) --> [word_s("rw", L), word_s(NewName, _), word_s(Name, _)].
