% FLP CVICENI 4 - PROLOG 1 - UVOD

% ukazka predikatu pro vypocet funkce faktorial
factorial( 0, 1 ).
factorial( N, Value ) :-
     N > 0,
     Prev is N - 1,
     factorial( Prev, Prevfact ),
     Value is Prevfact * N.

% databaze rodinnych vztahu
muz(jan).
muz(pavel).
muz(robert).
muz(tomas).
muz(petr).

zena(marie).
zena(jana).
zena(linda).
zena(eva).

otec(tomas,jan).
otec(jan,robert).
otec(jan,jana).
otec(pavel,linda).
otec(pavel,eva).

matka(marie,robert).
matka(linda,jana).
matka(eva,petr).

% Implementujte nasledujici predikaty:
rodic(X,Y) :- otec(X, Y) ; matka(X, Y).
sourozenec(X,Y) :- rodic(Z, X), rodic(Z, Y).
sestra(X,Y) :- sourozenec(X, Y), zena(Y).
deda(X,Y) :- rodic(X, Z), rodic(Z, Y).
je_matka(X) :- rodic(X, _), zena(X).
teta(X,Y) :- sourozenec(X, Z), rodic(Z, Y), zena(X).

% Seznamy:
neprazdny([_|_]) :- true.
hlavicka([H|_], H).
posledni([H], H) :- !.
posledni([_|T], Res) :- posledni(T, Res).

% Dalsi ukoly:
spoj([], B, B).
spoj([H|T], B, [H|R]) :- spoj(T, B, R).

obrat([],[]) :- !.
obrat([H|T], Res) :- obrat(T, R), spoj(R, [H], Res).

sluc(L, [], L).
sluc([], L, L).
sluc([X|XS], [Y|YS], [X|T]) :- X =< Y, sluc(XS, [Y|YS], T).
sluc([X|XS], [Y|YS], [Y|T]) :- X > Y, sluc([X|XS], YS, T).

serad([], []) :- !.
serad([H|T], SL) :- serad(T, ST), sluc([H], ST, SL).

split([], []) :- !.
split([H|T], [A|AS]) :- tillSpace([H|T], A, R), split(R, AS).

tillSpace([], [], []).
tillSpace([' '|T], [], T) :- !.
tillSpace([X|T], [X|A], B) :- tillSpace(T, A, B).


plus(X,Y,Z) :- Z is X + Y.

zipWith(_, [], _, []).
zipWith(_, _, [], []).
zipWith(F, [X|XS], [Y|YS], [R|RS]) :- call(F, X, Y, R), zipWith(F, XS, YS, RS).
