max(X,Y,X) :- lt(Y,X).
max(X,Y,Y) :- lt(X,Y).

listlen([],0).
listlen([_|T], N) :- listlen(T,X), is(N,plus(X,1)).