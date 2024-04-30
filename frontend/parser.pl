:- module(parser, [
              parse/2,
              pparse/1
          ]).

:- use_module(lexer).

parse(Text, AST) :-
    string_codes(Text, Code),
    tokenize(Code, Out),
    program(AST, Out, []).

pparse(Code) :-
    parse(Code, AST),
    print_term(AST, []).

program([Node|Rest]) --> item(Node), (program(Rest) | {Rest = []}).

item(Node) --> lit(Node) | quote(Node) | pat_obj(Node) | word_def(Node) | word(Node).

lit(num(N, L)) --> [lit(N, L)], {number(N)}.
lit(str(S, L)) --> [lit(S, L)], {string(S)}.
lit(true(L)) --> [word_s("t", L)].
lit(false(L)) --> [word_s("f", L)].
lit(atom(A, L)) --> [lit(A, L)], {atom(A)}.
lit(_) --> !, {fail}.

word(word(right, Name, L)) --> [word(">", Name, L)].
word(word(left, Name, L)) --> [word("<", Name, L)].
word(word_s(Name, L)) --> [word_s(Name, L)], {Name \= "{", Name \= "}"}.
word(_) --> !, {fail}.

% maybe add shorthand for quote with variables, and then desugar it later
quote(quote(Body, L)) --> [word_s("[", L)], program(Body), [word_s("]", _)].
quote(_) --> !, {fail}.

% maybe allow nested objects?
pat_obj(pattern_obj(Pats, L)) --> [word_s("(", L)], pats(Pats), [word_s(")", _)].
pats([Pat|Rest]) --> pat(Pat), (pats(Rest) | {Rest = []}).
pat(quo_spl(left, Pats, R)) --> [word_s("[", _)], pats(Pats), [word_s("|<",_)], pat(R), [word_s("]", _)]. % too complicated?, also maybe change symbol for it
pat(quo_spl(right, Pats, R)) --> [word_s("[", _)], pat(R), [word_s("|>",_)], pats(Pats), [word_s("]", _)]. % too complicated?
pat(quote(Quote)) --> [word_s("[", _)], pats(Quote), [word_s("]", _)].
pat(lit(Lit)) --> [lit(Lit, _)].
pat(var(Name)) --> [word_s(Name, _)], {\+ (Name = "|>"), \+ (Name = "|<")}.
pat(_) --> !, {fail}.

word_def(def(Name, Body, L)) --> [word_s("{", L), word_s(Name, _)],  program(Body), [word_s("}", _)].
