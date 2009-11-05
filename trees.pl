%%%%%%%%%%

% A tree is either a leaf, or a parent node with a list of trees.

istree(l(_)).
istree(n(Ts)) :- istreel(Ts).

istreel([]).
istreel([T|Ts]) :- istree(T), istreel(Ts).

%%%%%%%%%%

% A branch is either the tree itself, or a branch of one of the children
% of a parent node.

hasbranch(T, T).
hasbranch(n(Ts), U) :- branchl(Ts, U).

branchl([T|_], U) :- hasbranch(T, U).
branchl([_|Ts], U) :- branchl(Ts, U).

%%%%%%%%%%

% A thinning of a leaf is the leaf itself; a thinning of a parent node
% contains the thinnings of its children, possibly with some omitted.

hasthinning(l(X), l(X)).
hasthinning(n(Ts), n(Us)) :- thinl(Ts, Us).

thinl([], []).
thinl([T|Ts], [U|Us]) :- hasthinning(T, U), thinl(Ts, Us).
thinl([_|Ts], Us) :- thinl(Ts, Us).

%%%%%%%%%%

% A flatteing of a leaf is the leaf itself; a flattening of a 
% parent node contains the flattenings of its children, possibly with
% some of them integrated in the list.

hasflattening(l(X), l(X)).
hasflattening(n(Ts), n(Us)) :- flatl(Ts, Us).

flatl([],[]).
flatl([T|Ts], [U|Us]) :- hasflattening(T, U), flatl(Ts, Us).
flatl([n(Ts1)|Ts2], Us) :- flatl(Ts1, Us1), flatl(Ts2, Us2), app(Us1, Us2, Us).

app([], Ys, Ys).
app([X|Xs], Ys, [X|Zs]) :- app(Xs, Ys, Zs).

%%%%%%%%%%
