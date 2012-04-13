translate(a,[dot,dash]).
translate(b,[dash,dot,dot,dot]).
translate(c,[dash,dot,dash,dot]).
translate(d,[dash,dot,dot]).
translate(e,[dot]).
translate(f,[dot,dot,dash,dot]).
translate(g,[dash,dash,dot]).
translate(h,[dot,dot,dot,dot]).
translate(i,[dot,dot]).
translate(j,[dot,dash,dash,dash]).
translate(k,[dash,dot,dash]).
translate(l,[dot,dash,dot,dot]).
translate(m,[dash,dash]).
translate(n,[dash,dot]).
translate(o,[dash,dash,dash]).
translate(p,[dot,dash,dash,dot]).
translate(q,[dash,dash,dot,dash]).
translate(r,[dot,dash,dot]).
translate(s,[dot,dot,dot]).
translate(t,[dash]).
translate(u,[dot,dot,dash]).
translate(v,[dot,dot,dot,dash]).
translate(w,[dot,dash,dash]).
translate(x,[dash,dot,dot,dash]).
translate(y,[dash,dot,dash,dash]).
translate(z,[dash,dash,dot,dot]).

app([], X, X).
app([H|T], Y, [H|Z]) :- app(T, Y, Z).

encode([], []).
encode([Letter|Text], Coding) :-
   translate(Letter, Code),
   app(Code, RestCode, Coding),
   encode(Text, RestCode).

sofia(X) :- encode(X, [dot,dot,dot,dash,dash,dash,dot,dot,dash,dot,dot,dot,dot,dash]).
