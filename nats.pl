nat(z).
nat(s(N)) :- nat(N).

natlist([]).
natlist([N|Ns]) :- nat(N), natlist(Ns).
