% Small test predicates for arithmetic                            -*- prolog -*-

max(X,Y,X) :- lt(Y,X).
max(X,Y,Y) :- lt(X,Y).

listlen([],0).
listlen([_|T], N) :- listlen(T,X), is(N,plus(X,1)).

smallNums([], _).
smallNums([H | T], N) :- lt(H,N), smallNums(T,N).

% Filter out numbers bigger than or equal to 10
filterBig([], []).
filterBig([H|T1], [H|T2]) :- filterBig(T1, T2), lt(H, 10).
filterBig([H|T1], T2) :- lt(9, H), filterBig(T1, T2).

id([],[]).
id([X|T], [X|T2]) :- id(T,T2).