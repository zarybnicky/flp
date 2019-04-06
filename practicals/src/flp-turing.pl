
isUpper(C) :- memberchk(C, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
                            'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
                            'U', 'V', 'W', 'X', 'Y', 'Z']).
isLower(C) :- memberchk(C, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
                            'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
                            'u', 'v', 'w', 'x', 'y', 'z']).

start :-
    loadTuring(Rules, [T|Ts]),
    extractFinal(Rules, Finals, Others),
    append(Finals, Others, All),
    loadRules(All),
    step('S', ([], T, Ts), 'F', _),
    halt.

loadTuring(Rules, TInit) :-
    prompt(_, ''),
    readLine(L, C),
    ( isUpper(C), !, length(L, 7), parseRule(L, R), loadTuring(Rs, TInit), Rules = [R|Rs]
    ; isLower(C), TInit = L, Rules = []
    ).

parseRule([S], S).
parseRule([' '|T], S) :- parseRule(T, S), !.
parseRule([H|T], (H, R)) :- parseRule(T, R).

readLine(L, C) :-
    get_char(C),
    ( ( C == end_of_file
      ; (char_code(C,Code), Code == 10)
      ), L = [], !
    ; readLine(LL,_), [C|LL] = L
    ).

% sort state transitions -> final states first!
extractFinal([], [], []).
extractFinal([(S, A, 'F', B)|T], [(S, A, 'F', B)|Final], Other) :-
    extractFinal(T, Final, Other).
extractFinal([(S, A, Ss, B)|T], Final, [(S, A, Ss, B)|Other]) :-
    extractFinal(T, Final, Other).

loadRules([]).
loadRules([(S, A, Ss, B)|T]) :-
    ( memberchk((S, A, Ss, B), T)
    ; assertz(tm(S, A, Ss, B))
    ),
    loadRules(T).

step(SFin, (L, C, R), SFin, (L, C, R)) :-
    maplistr(write, L), write('F'), write(C), maplist(write, R), writeln(''),
    !.
step(S, (L, C, R), SFin, TFin) :-
    maplistr(write, L), write(S), write(C), maplist(write, R), writeln(''),
    once(tm(S, C, S2, Act)),
    perform(Act, (L, C, R), T2),
    step(S2, T2, SFin, TFin).

% left
perform('L', ([],     C, R     ), ([],    ' ', [C|R])) :- !.
perform('L', ([L|Ls], C, R     ), (Ls,    L,   [C|R])) :- !.
% right
perform('R', (L,      C, []    ), ([C|L], ' ', []   )) :- !.
perform('R', (L,      C, [R|Rs]), ([C|L], R,   Rs   )) :- !.
% write
perform(C,   (L,      _, R     ), (L,     C,   R    )).

maplistr(F, L) :- reverse(L, R), maplist(F, R).

:- dynamic tm/4.
% tm('S', 'a', 'B', 'a').
% tm('B', 'a', 'B', 'b').
% tm('B', 'b', 'B', 'R').
% tm('B', 'c', 'F', 'c').
% tm('B', 'c', 'B', 'a').
