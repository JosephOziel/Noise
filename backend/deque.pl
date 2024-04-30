:- module(deque, [
              lib/0,
              create_empty/1,
              create_with/2,
              append_right/3,
              append_left/3,
              pop_right/3,
              pop_left/3,
              extend_right/3,
              extend_left/3,
              clear/2,
              copy/2,
              count/3,
              index/3,
              remove_index/3,
              insert/4,
              remove/3,
              rotate/3
          ]).

:- use_module(library(janus)).

lib :- py_add_lib_dir("C:/Users/vatis/OneDrive/Documents/Noise/backend").
lib.

create_empty(D) :-
    py_call(helper:create_empty(), D).

create_with(Ls, D) :-
    map(Ls, Ls1),
    py_call(helper:create_with(Ls1), D0),
    map(D, D0).

append_right(D, A, ND) :-
    map(D, D0), term_string(A, A0),
    py_call(helper:append_right(D0, A0), ND0),
    map(ND, ND0).

append_left(D, A, ND) :-
    map(D, D0), term_string(A, A0),
    py_call(helper:append_left(D0, A0), ND0),
    map(ND, ND0).

pop_right(D, ND, P) :-
    map(D, D0),
    py_call(helper:pop_right(D0), [ND0, P0]),
    map(ND, ND0), term_string(P, P0).

pop_left(D, ND, P) :-
    map(D, D0),
    py_call(helper:pop_left(D0), [ND0, P0]),
    map(ND, ND0), term_string(P, P0).

extend_right(D, Ls, ND) :-
    map(D, D0), map(Ls, Ls0),
    py_call(helper:extend_right(D0, Ls0), ND0),
    map(ND, ND0).

extend_left(D, Ls, ND) :-
    map(D, D0), map(Ls, Ls0),
    py_call(helper:extend_right(D0, Ls0), ND0),
    map(ND, ND0).

clear(D, ND) :-
    py_call(helper:clear(D), ND).

copy(D, ND) :-
    map(D, D0),
    py_call(helper:copy(D0), ND0),
    map(ND, ND0).

count(D, A, ND) :-
    map(D, D0), term_string(A, A0),
    py_call(helper:count(D0, A0), ND0),
    map(ND, ND0).

index(D, I, ND) :-
    map(D, D0),
    py_call(helper:index(D0, I), ND0),
    map(ND, ND0).

remove_index(D, I, ND) :-
    map(D, D0),
    py_call(helper:remove_index(D0, I), ND0),
    map(ND, ND0).

insert(D, I, A, ND) :-
    map(D, D0), term_string(A, A0),
    py_call(helper:insert(D0, I, A0), ND0),
    map(ND, ND0).

remove(D, A, ND) :-
    map(D, D0), term_string(A, A0),
    py_call(helper:remove(D0, A0), ND0),
    map(ND, ND0).

rotate(D, N, ND) :-
    map(D, D0),
    py_call(helper:rotate(D0, N), ND0),
    map(ND, ND0).


%utils
map([], []).
map([X|Ls], [Y|NLs]) :-
    term_string(X, Y),
    map(Ls, NLs).
