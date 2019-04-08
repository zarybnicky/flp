% FLP 2019, FIT VUT Brno
% "Turing machine"
% Jakub Zárybnický <xzaryb00@stud.fit.vutbr.cz>

%! start()
%
% Load a turing machine from stdin, load its rules into database, and run it
% from state 'S' to state 'F'.
start :-
    loadTuring(Rules, [T|Ts]),
    sortRules(Rules, Sorted),
    maplist(loadRule, Sorted), !,
    step('S', ([], T, Ts), 'F', _),
    halt.

%! rule(+State0, +Head, -State, -Action)
%
% Transition rule of the turing machine
:- dynamic rule/4.

%! loadRule(+Rule)
%
% Load a transition rule into database, skipping already existing rules
loadRule((S, A, Ss, B)) :- assertz(rule(S, A, Ss, B)) ; true.

%! step(+InitState, +InitTape, ?FinalState, ?FinalTape)
%
% Run the machine given initial and final states and the initial state of the
% tape, outputting the final state of the tape.
step(SFin, TFin, SFin, TFin) :- writeTuring(SFin, TFin), !.
step(S, (L, C, R), SFin, TFin) :-
    writeTuring(S, (L, C, R)),
    rule(S, C, S2, Act),
    perform(Act, (L, C, R), T2), !,
    step(S2, T2, SFin, TFin).

%! perform(+Action, +Tape0, -Tape)
%
% Apply a transition action to the tape - 'L' and 'R' move the tape head, any
% other symbol is written out
perform('L', ([],     C, R     ), ([],    ' ', [C|R])). % left
perform('L', ([L|Ls], C, R     ), (Ls,    L,   [C|R])). % left
perform('R', (L,      C, []    ), ([C|L], ' ', []   )). % right
perform('R', (L,      C, [R|Rs]), ([C|L], R,   Rs   )). % right
perform(C,   (L,      _, R     ), (L,     C,   R    )). % write

%! loadTuring(-Rules, -Tape)
%
% Load a description of the machine from stdin (a sequence of transition tules
% followed by the initial tape)
loadTuring(Rules, Tape) :-
    prompt(_, ''),
    readLine(L),
    ( % State transition
      [S, ' ', A, ' ', T, ' ', B] = L,
      isState(S), isSymbol(A), isState(T), (isSymbol(B) ; B == 'L' ; B == 'R'),
      loadTuring(Rs, Tape), Rules = [(S, A, T, B)|Rs]
    ; % Initial tape, final line
      [_|_] = L, maplist(isSymbol, L),
      Rules = [], Tape = L
    ).

%! readLine(-Line)
%
% Read a sequence of characters terminated by EOL or EOF
readLine(L) :-
    get_char(C),
    ( (C == end_of_file; (char_code(C,Code), Code == 10)), L = [], !
    ; readLine(LL), [C|LL] = L
    ).

%! isState(+Char)
%
% Check if C is a member of the set of the machine's states (uppercase letters)
isState(C) :- memberchk(C, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
                            'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
                            'U', 'V', 'W', 'X', 'Y', 'Z']).

%! isSymbol(+Char)
%
% Check if C is a member of the set of the machine's tape alphabet (lowercase
% letters and the space character)
isSymbol(C) :- memberchk(C, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
                             'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
                             'u', 'v', 'w', 'x', 'y', 'z', ' ']).

%! sortRules(+Rules0, -Rules)
%
% Sort transitions so that all final state transitions come first
sortRules(X, Res) :-
    ( selectchk((S, A, 'F', B), X, Xs), !, sortRules(Xs, Fs), Res = [(S, A, 'F', B)|Fs]
    ; Res = X
    ).

%! writeTuring(+State, +Tape)
%
% Print out the current state of the machine
writeTuring(S, (L, C, R)) :-
    reverse(L, LL), maplist(write, LL), write(S), write(C), maplist(write, R), writeln('').
