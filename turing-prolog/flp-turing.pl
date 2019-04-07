:- dynamic rule/4.

start :-
    loadTuring(Rules, [T|Ts]),
    extractFinal(Rules, Final, Other), append(Final, Other, Sorted),
    maplist(loadRule, Sorted), !,
    step('S', ([], T, Ts), 'F', _),
    halt.

loadRule((S, A, Ss, B)) :- assertz(rule(S, A, Ss, B)) ; true.

step(SFin, TFin, SFin, TFin) :- writeTuring(SFin, TFin), !.
step(S, (L, C, R), SFin, TFin) :-
    writeTuring(S, (L, C, R)),
    rule(S, C, S2, Act),
    perform(Act, (L, C, R), T2), !,
    step(S2, T2, SFin, TFin).

perform('L', ([],     C, R     ), ([],    ' ', [C|R])). % left
perform('L', ([L|Ls], C, R     ), (Ls,    L,   [C|R])). % left
perform('R', (L,      C, []    ), ([C|L], ' ', []   )). % right
perform('R', (L,      C, [R|Rs]), ([C|L], R,   Rs   )). % right
perform(C,   (L,      _, R     ), (L,     C,   R    )). % write

loadTuring(Rules, Tape) :-
    prompt(_, ''),
    readLine(L, C),
    ( % State transition
      [S, ' ', A, ' ', T, ' ', B] = L,
      isUpper(S), isLower(A), isUpper(T), (isLower(B) ; B == 'L' ; B == 'R'),
      loadTuring(Rs, Tape), Rules = [(S, A, T, B)|Rs]
    ; % Initial tape
      isLower(C), maplist(isLower, L),
      Rules = [], Tape = L
    ).

readLine(L, C) :-
    get_char(C),
    ( (C == end_of_file; (char_code(C,Code), Code == 10)), L = [], !
    ; readLine(LL, _), [C|LL] = L
    ).

isUpper(C) :- memberchk(C, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
                            'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
                            'U', 'V', 'W', 'X', 'Y', 'Z']).
isLower(C) :- memberchk(C, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
                            'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
                            'u', 'v', 'w', 'x', 'y', 'z']).

% sort state transitions -> final states first!
extractFinal([], [], []).
extractFinal([(S, A, 'F', B)|T], [(S, A, 'F', B)|Final], Other) :-
    extractFinal(T, Final, Other).
extractFinal([(S, A, Ss, B)|T], Final, [(S, A, Ss, B)|Other]) :-
    extractFinal(T, Final, Other).

writeTuring(S, (L, C, R)) :-
    reverse(L, LL), maplist(write, LL), write(S), write(C), maplist(write, R), writeln('').
