:- module(deque, [
              lib/0,
              create_empty/1,
              create_with/2,
              append_right/3,
              append_left/3
          ]).

:- use_module(library(janus)).

:- table create_empty/1, create_with/2, append_right/3, append_left/3.

lib :- py_add_lib_dir("C:/Users/vatis/OneDrive/Documents/Noise/backend").
lib.

create_empty(D) :-
    py_call(helper:create_empty(), D).

create_with(Ls, D) :-
    py_call(helper:create_with(Ls), D).

append_right(D, A, ND) :-
    py_call(helper:append_right(D, A), ND).

append_left(D, A, ND) :-
    py_call(helper:append_left(D, A), ND).
