:- module(lexer, [tokenize/2, plex/1]).

plex(Code) :-
    tokenize(Code, Out),
    print_term(Out, []).

tokenize(In, Out) :- tokenize(In, Out, 1).

tokenize([], [], _LineNo).

% increment lineno on newline
tokenize([In|T], Out, LineNo) :-
    code_type(In, newline),
    NextLineNo is LineNo + 1, !,
    tokenize(T, Out, NextLineNo).

% ignore whitespace
tokenize([In|T], Out, LineNo) :-
    code_type(In, space),
    tokenize(T, Out, LineNo).

% ignore comments
tokenize([0'%|T], Out, LineNo) :-
consume_until(T, 0'\n, Remain, _),
NextLineNo is LineNo + 1, !,
tokenize(Remain, Out, NextLineNo).

% numbers (add negative numbers, and hex)
tokenize([In|T_i], [Out|T_o], LineNo) :-
    code_type(In, digit),
    consume_type([In|T_i], digit, [0'.|Next], WholeDigits),
    consume_type(Next, digit, Remain, FractionDigits),
    append(WholeDigits, [0'.|FractionDigits], DigitList),
    number_codes(Value, DigitList),
    Out = lit(Value, LineNo),
    tokenize(Remain, T_o, LineNo).

tokenize([In|T_i], [Out|T_o], LineNo) :-
    code_type(In, digit),
    consume_type([In|T_i], digit, Remain, DigitList),
    number_codes(Value, DigitList),
    Out = lit(Value, LineNo),
    tokenize(Remain, T_o, LineNo).

% strings
tokenize([0'"|T_i], [Out|T_o], LineNo) :-
    consume_until(T_i, 0'", Remain, Codes),
    string_codes(Value, Codes),
    Out = lit(Value, LineNo),
    tokenize(Remain, T_o, LineNo).

% atoms
tokenize([0':|T_i], [Out|T_o], LineNo) :-
    consume_type([0':|T_i], graph, Remain, [_|Codes]),
    string_codes(Value, Codes),
    atom_string(A, Value),
    Out = lit(A, LineNo),
    tokenize(Remain, T_o, LineNo).

% word
tokenize([A|T_i], [Out|T_o], LineNo) :-
    (   A = 0'< ; A = 0'> ),
    consume_type([A|T_i], graph, Remain, [_|Codes]),
    string_codes(Value, Codes),
    string_codes(T, [A]),
    Out = word(T, Value, LineNo),
    tokenize(Remain, T_o, LineNo).

% word_s
tokenize([In|T_i], [Out|T_o], LineNo) :-
    code_type(In, graph), In \= 0'",
    consume_type([In|T_i], graph, Remain, CharList),
    string_codes(Name, CharList),
    Out = word_s(Name, LineNo),
    tokenize(Remain, T_o, LineNo).

% utils
consume_type([], _, [], []).
consume_type([Char|In], Type, Remain, [Char|Out]) :-
    code_type(Char, Type),
    consume_type(In, Type, Remain, Out).
consume_type([Char|In], Type, [Char|In], []) :-
    \+ code_type(Char, Type).

consume_until([], _, [], []).
consume_until([TargetChar|In], TargetChar, In, []).
consume_until([Char|In], TargetChar, Remain, [Char|Out]) :-
    consume_until(In, TargetChar, Remain, Out).

