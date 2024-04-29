:- module(rtp_dqs,
	[ empty_deque/1,         % creates/tests for the empty deque
	  inject_deque/3,        % add to back of deque
	  eject_deque/3,         % remove from back of deque
	  push_deque/3,          % add to front of deque
	  pop_deque/3,           % remove from front of deque
	  reverse_deque/2,       % reverse a deque
	  poplist_deque/3,       % add/remove a list of 
	  pushlist_deque/3,      %   elements from 
	  injectlist_deque/3,    %   the front or back
	  ejectlist_deque/3      %   of the deque
      ]).

% :- use_module(library(type_check)).

% :- type deque(T) ---> dq(integer,list(T),integer,list(T),state(T)).
% :- type state(T) ---> ready
%                    %     Lgs     Lgs_revs      Ss_tail
%                     ; rot(list(T),list(list(T)),list(T))
%                     %     Ss      Ss_open Ls      Lks      Lgs_revs
%                    ; rot(list(T),list(T),list(T),list(T), list(list(T))).


%% reverse_deque(+DQ:deque,-DQ_rev:deque) is det.
%
% Reverse a deque. 
%
% :- pred reverse_deque(deque(T),deque(T)).
%
reverse_deque(dq(Fs_len,Fs,Rs_len,Rs,St),dq(Rs_len,Rs,Fs_len,Fs,St)).


%% empty_deque(?DQ:deque) is semidet.
% 
% Holds of the empty deque.
%
% :- pred empty_deque(deque(_T)).
%
empty_deque(dq(0,[],0,[],ready)).


%% push_deque(?El,+DQ_old:deque,-DQ_new:deque) is det.
%
% Adds El to the front of DQ_old to give DQ_new.
%
% :- pred push_deque(T, deque(T),deque(T)).
%
push_deque(F,dq(Fs_len,Fs,Rs_len,Rs,St),DQ_new):-
    Fs1_len is Fs_len+1,
    make_dq(St,Fs1_len,[F|Fs],Rs_len,Rs,DQ_new).


%% inject_deque(?El,+DQ_old:deque,-DQ_new:deque) is det.
%
% Adds El to the back of DQ_old to give DQ_new.
%
% :- pred inject_deque(T,deque(T),deque(T)).
%
inject_deque(El,DQ_old,DQ_new):-
    reverse_deque(DQ_old,DQ_dlo),
    push_deque(El,DQ_dlo,DQ_wen),
    reverse_deque(DQ_wen,DQ_new).


%% pop_deque(+DQ_old:deque,?El,-DQ_new:deque) is semidet.
%
% Removes ?El from the front of DQ_old to give DQ_new. 
%
% May fail if ?El cannot be unified with the front element of DQ_old
% or if DQ_old is empty.
%
% :- pred pop_deque(deque(T),T, deque(T)).
%
pop_deque(dq(Fs_len,Fs,Rs_len,Rs,St),F,DQ_new):-
    ( Fs == []
      -> Rs = [F],
         empty_deque(DQ_new)
    ; Fs = [F|Fs1],
      Fs1_len is Fs_len-1,
      make_dq(St,Fs1_len,Fs1,Rs_len,Rs,DQ_new)
    ).


%% eject_deque(+DQ_old:deque,?El,-DQ_new:deque) is semidet.
%
% Removes ?El from the back of DQ_old to give DQ_new. 
%
% May fail if ?El cannot be unified with the rear element of DQ_old
% or if DQ_old is empty
% :- pred eject_deque(deque(T),T, deque(T)).
%
eject_deque(DQ_old,El,DQ_new):-
    reverse_deque(DQ_old,DQ_dlo),
    pop_deque(DQ_dlo,El,DQ_wen),
    reverse_deque(DQ_wen,DQ_new).


%% pushlist_deque(+Els:list,+DQ_old:deque,-DQ_new:deque) is det.
%
% Pushes elements of Els onto DQ_old to give DQ_new. 
%
% :- pred pushlist_deque(list(T),deque(T),deque(T)).
%
pushlist_deque([],DQ,DQ).
pushlist_deque([El|Els],DQ_old,DQ_new):-
	push_deque(El,DQ_old,DQ_mid),
	pushlist_deque(Els,DQ_mid,DQ_new).


%% injectlist_deque(+Els:list,+DQ_old:deque,-DQ_new:deque) is det.
%
% Injects elements of Els into DQ_old to give DQ_new. 
%
% :- pred injectlist_deque(list(T),deque(T),deque(T)).
injectlist_deque(Els,DQ_old,DQ_new):-
	reverse_deque(DQ_old,DQ_dlo),
	pushlist_deque(Els,DQ_dlo,DQ_wen),
	reverse_deque(DQ_wen,DQ_new).


%% poplist_deque(+DQ_old:deque,?Els:list,-DQ_new:deque) is nondet.
%
% Pops elements from DQ_old onto Els to give DQ_new. See pop_deque/3.
%
% Gives a.o. quick (and dirty?)
% ways of implementing deque-to-list 
% ( =|poplist_deque(DQ_old,Els,DQ_new), empty(DQ_new)|= ) and  `take N'
% ( =|length(N,Els), poplist(DQ_old,Els,DQ_new)|= ). 
%
% :- pred poplist_deque(deque(T),list(T),deque(T)).
%
poplist_deque(DQ,[],DQ).
poplist_deque(DQ_old,[El|Els],DQ_new):-
	pop_deque(DQ_old,El,DQ_mid),
	poplist_deque(DQ_mid,Els,DQ_new).


%% ejectlist_deque(+DQ_old:deque,?Els:list,-DQ_new:deque) is nondet.
%
% Ejects elements from DQ_old onto Els to give DQ_new. See
% eject_deque/3 and usage hints at poplist_deque/3.
%
% :- pred ejectlist_deque(deque(T),list(T),deque(T)).
%
ejectlist_deque(DQ_old,Els,DQ_new):-
	reverse_deque(DQ_old,DQ_dlo),
	poplist_deque(DQ_dlo,Els,DQ_wen),
	reverse_deque(DQ_wen,DQ_new).
	

% Aux predicates. See the paper for explanation. Compared to the
% paper, these are longer, `unrolled' versions that do not use the
% second-order predicate call/2.
%

% make_dq/4     +State,  +Fs_len +Fs     +Rs_len +Rs     -DQ_new
% :- pred make_dq(state(T),integer,list(T),integer,list(T),deque(T)).
%
make_dq(ready,Fs_len,Fs,Rs_len,Rs,DQ):-
	( Rs_len > 3*Fs_len
          -> Rs1_len is 2*Fs_len+1,
             Fs1_len is Rs_len-Fs_len-1,
             four_rot(Fs,Fs1,Rs,Rs1,[[]|_],St1),
             DQ = dq(Fs1_len,Fs1,Rs1_len,Rs1,St1)

	; Fs_len > 3*Rs_len
          -> Fs1_len is 2*Rs_len+1,
             Rs1_len is Fs_len-Rs_len-1,
	     four_rot(Rs,Rs1,Fs,Fs1,[[]|_],St1),
             DQ = dq(Fs1_len,Fs1,Rs1_len,Rs1,St1)

	; DQ = dq(Fs_len,Fs,Rs_len,Rs,ready)
        ).
make_dq(rot(A1,A2,A3),Fs_len,Fs,Rs_len,Rs,dq(Fs_len,Fs,Rs_len,Rs,St1)):-
	two_rot(A1,A2,A3,St1).
make_dq(rot(A1,A2,A3,A4,A5),Fs_len,Fs,Rs_len,Rs,dq(Fs_len,Fs,Rs_len,Rs,St1)):-
	two_rot(A1,A2,A3,A4,A5,St1).


% The state update predicates contain now the state update logic as
% well as the counting, so that we handle doing two or four repeated
% updates without meta-calling. Should really be autogenerated code,
% though...

% rot/4           +Lgs    ?Lgs_revs     ?Ss_tail -State
% :- pred  four_rot(list(T),list(list(T)),list(T), state(T)).
% :- pred three_rot(list(T),list(list(T)),list(T), state(T)).
% :- pred   two_rot(list(T),list(list(T)),list(T), state(T)).
% :- pred   one_rot(list(T),list(list(T)),list(T), state(T)).
%
four_rot([],[Ss_tail],Ss_tail,ready).
four_rot([Lg|Lgs],[Lgs_rev|Lgs_revs],Ss_tail,St1):-
	Lgs_revs = [[Lg|Lgs_rev]|_],
	three_rot(Lgs,Lgs_revs,Ss_tail,St1).
three_rot([],[Ss_tail],Ss_tail,ready).
three_rot([Lg|Lgs],[Lgs_rev|Lgs_revs],Ss_tail,St1):-
	Lgs_revs = [[Lg|Lgs_rev]|_],
	two_rot(Lgs,Lgs_revs,Ss_tail,St1).
two_rot([],[Ss_tail],Ss_tail,ready).
two_rot([Lg|Lgs],[Lgs_rev|Lgs_revs],Ss_tail,St1):-
	Lgs_revs = [[Lg|Lgs_rev]|_],
	one_rot(Lgs,Lgs_revs,Ss_tail,St1).
one_rot([],[Ss_tail],Ss_tail,ready).
one_rot([Lg|Lgs],[Lgs_rev|Lgs_revs],Ss_tail,rot(Lgs,Lgs_revs,Ss_tail)):-
	Lgs_revs = [[Lg|Lgs_rev]|_].

% rot/6           +Ss     ?Ss_open +Ls     ?Lks    ?Lgs_revs     -State
% :- pred  four_rot(list(T),list(T), list(T),list(T),list(list(T)),state(T)).
% :- pred three_rot(list(T),list(T), list(T),list(T),list(list(T)),state(T)).
% :- pred   two_rot(list(T),list(T), list(T),list(T),list(list(T)),state(T)).
% :- pred   one_rot(list(T),list(T), list(T),list(T),list(list(T)),state(T)).
%
four_rot([],Ss_tail,[L|Lgs],[L],Lgs_revs,St1):-
	three_rot(Lgs,Lgs_revs,Ss_tail,St1).
four_rot([S|Ss],[S|Ss_open],[L1,L2|Ls],[L1,L2|Lks],Lgs_revs,St1):-
	three_rot(Ss,Ss_open,Ls,Lks,Lgs_revs,St1).
three_rot([],Ss_tail,[L|Lgs],[L],Lgs_revs,St1):-
	two_rot(Lgs,Lgs_revs,Ss_tail,St1).
three_rot([S|Ss],[S|Ss_open],[L1,L2|Ls],[L1,L2|Lks],Lgs_revs,St1):-
	two_rot(Ss,Ss_open,Ls,Lks,Lgs_revs,St1).
two_rot([],Ss_tail,[L|Lgs],[L],Lgs_revs,St1):-
	one_rot(Lgs,Lgs_revs,Ss_tail,St1).
two_rot([S|Ss],[S|Ss_open],[L1,L2|Ls],[L1,L2|Lks],Lgs_revs,St1):-
	one_rot(Ss,Ss_open,Ls,Lks,Lgs_revs,St1).
one_rot([],Ss_tail,[L|Lgs],[L],Lgs_revs,rot(Lgs,Lgs_revs,Ss_tail)).
one_rot([S|Ss],[S|Ss_open],[L1,L2|Ls],[L1,L2|Lks],Lgs_revs,rot(Ss,Ss_open,Ls,Lks,Lgs_revs)).