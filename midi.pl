:- working_directory(_, 'C:/Users/vatis/OneDrive/Documents/Sikum/utils').
:- use_module(mididcg, [note/5]). 

main :-
%    mididcg:instr(0, 1),
    minidcg:volume(0, 100),
    mididcg:note(0, 0, 100, 1, 60).
