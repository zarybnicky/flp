/** FLP 2019
Toto je ukazkovy soubor zpracovani vstupu v prologu.
Tento soubor muzete v projektu libovolne pouzit.

autor: Martin Hyrs, ihyrs@fit.vutbr.cz
preklad: swipl -q -g start -o flp19-log -c input2.pl
*/


/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
    get_char(C),
    ( ( C == end_of_file
      ; (char_code(C,Code), Code == 10)
      ), L = [], !
    ; read_line(LL,_), [C|LL] = L
    ).

read_lines(Ls) :-
    read_line(L,C),
    ( C == end_of_file, Ls = []
    ; read_lines(LLs), Ls = [L|LLs]
    ).

/** nacte zadany pocet radku */
read_lines2([],0).
read_lines2([L|Ls],N) :-
    N > 0,
    read_line(L,_),
    N1 is N-1,
    read_lines2(Ls, N1).

split([], []) :- !.
split([H|T], [A|AS]) :- tillSpace([H|T], A, R), split(R, AS).

tillSpace([], [], []).
tillSpace([' '|T], [], T) :- !.
tillSpace([X|T], [X|A], B) :- tillSpace(T, A, B).

start :-
    prompt(_, ''),
    read_lines(LL),
    maplist(split, LL, S),
    write(S).

/** nacte N radku vstupu, zpracuje, vypise */
start2(N) :-
    prompt(_, ''),
    read_lines2(LL, N),
    maplist(split, LL, S),
    maplist(writeln, S).

/** prevede retezec na seznam atomu */
% pr.: string("12.35",S). S = ['1', '2', '.', '3', '5'].
retezec([],[]).
retezec([H|T],[C|CT]) :- atom_codes(C,[H]), retezec(T,CT).

/** knihovni predikat number_chars(?Number, ?CharList) */
% pr.: number_chars(12.35, ['1', '2', '.', '3', '5']).
