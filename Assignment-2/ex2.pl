/* I, Pan Eyal (208722058) assert that the work I submitted is entirely my own.
I have not received any part from any other student in the class (or other source),
nor did I give parts of it for use to others. I have clearly marked in the comments
of my program any code taken from an external source. */

user:file_search_path(xxxx, 'C:/Users/paney/Documents/Studies/Logic_Programming_Class/Assignment-2/satsolver').
:- use_module(xxxx(satsolver)).


/* ---------------------------- TASK 1 ---------------------------- */

/*
---- Full Adder Truth Table ----
    X  Y Cin |  Z Cout | Value
_____________|_________|________
    0  0  0  |  0   0  |   T
    0  0  0  |  1   0  |   F
    0  0  0  |  0   1  |   F
    0  0  0  |  1   1  |   F
             |         |
    1  0  0  |  0   0  |   F
    1  0  0  |  1   0  |   T
    1  0  0  |  0   1  |   F
    1  0  0  |  1   1  |   F
             |         |
    0  1  0  |  0   0  |   F
    0  1  0  |  1   0  |   T
    0  1  0  |  0   1  |   F
    0  1  0  |  1   1  |   F
             |         |
    1  1  0  |  0   0  |   F
    1  1  0  |  1   0  |   F
    1  1  0  |  0   1  |   T
    1  1  0  |  1   1  |   F
             |         |
    0  0  1  |  0   0  |   F
    0  0  1  |  1   0  |   T
    0  0  1  |  0   1  |   F
    0  0  1  |  1   1  |   F
             |         |
    1  0  1  |  0   0  |   F
    1  0  1  |  1   0  |   F
    1  0  1  |  0   1  |   T
    1  0  1  |  1   1  |   F
             |         |
    0  1  1  |  0   0  |   F
    0  1  1  |  1   0  |   F
    0  1  1  |  0   1  |   T
    0  1  1  |  1   1  |   F
             |         |
    1  1  1  |  0   0  |   F
    1  1  1  |  1   0  |   F
    1  1  1  |  0   1  |   F
    1  1  1  |  1   1  |   T
             |         |

By negating all the false values rows, we can convert from DNF to CNF.
Therefore the CNF for the full adder is:

*/
full_adder(X, Y, Cin, Z, Cout, CNF) :-
    CNF =  [[ X, Y, Cin,-Z, Cout],
            [ X, Y, Cin, Z,-Cout],
            [ X, Y, Cin,-Z,-Cout],

            [-X, Y, Cin, Z, Cout],
            [-X, Y, Cin, Z,-Cout],
            [-X, Y, Cin,-Z,-Cout],

            [ X,-Y, Cin, Z, Cout],
            [ X,-Y, Cin, Z,-Cout],
            [ X,-Y, Cin,-Z,-Cout],

            [-X,-Y, Cin, Z, Cout],
            [-X,-Y, Cin,-Z, Cout],
            [-X,-Y, Cin,-Z,-Cout],

            [ X, Y,-Cin, Z, Cout],
            [ X, Y,-Cin, Z,-Cout],
            [ X, Y,-Cin,-Z,-Cout],

            [-X, Y,-Cin, Z, Cout],
            [-X, Y,-Cin,-Z, Cout],
            [-X, Y,-Cin,-Z,-Cout],

            [ X,-Y,-Cin, Z, Cout],
            [ X,-Y,-Cin,-Z, Cout],
            [ X,-Y,-Cin,-Z,-Cout],

            [-X,-Y,-Cin, Z, Cout],
            [-X,-Y,-Cin,-Z, Cout],
            [-X,-Y,-Cin, Z,-Cout]].

% Base case for X=0, Y=0
add([-1], [-1], Cin, [Z,Cout], CNF) :-
    full_adder(-1, -1, Cin, Z, Cout, CNF).

% Case for X=0 Y!=0
add([-1], [Y|Ys], Cin, [Z|Zs], CNF) :-
    full_adder(-1, Y, Cin, Z, Cout, CNF1),
    add([-1], Ys, Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF).

% Case for X!=0 Y=0
add([X|Xs], [-1], Cin, [Z|Zs], CNF) :-
    full_adder(X, -1, Cin, Z, Cout, CNF1),
    add(Xs, [-1], Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF).

% Case for X!=0 Y!=0
add([X|Xs], [Y|Ys], Cin, [Z|Zs], CNF) :-
    full_adder(X, Y, Cin, Z, Cout, CNF1),
    add(Xs, Ys, Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF).

add(Xs, Ys, Zs, CNF) :-
    append(Xs, [-1], PaddedXs),
    append(Ys, [-1], PaddedYs),
    add(PaddedXs, PaddedYs, -1, Zs, CNF).

/* ---------------------------- TASK 2 ---------------------------- */

pad(Ys, Size, Ys) :-
    Size < 0,!.

pad(Ys, Size, PaddedYs) :-
    append(Ys, [-1], TempYs),
    pad(TempYs, Size - 1, PaddedYs).

leq(Xs, Ys, CNF) :-
    length(Xs, Xs_Size),
    length(Ys, Ys_Size),
    pad(Ys, (Xs_Size - Ys_Size) +2, PaddedYs),
    add(Xs, _, PaddedYs, CNF),!.

lt(Xs, Ys, CNF) :-
    add(Xs, [1], Ws, CNF1),
    leq(Ws, Ys, CNF2),
    append(CNF1, CNF2, CNF),!.

/* ---------------------------- TASK 3 ---------------------------- */

sum(PREV, [], PREV, []).

sum(PREV, [Xs|REST], Zs, CNF) :-
    add(PREV, Xs, Ws, CNF1),
    sum(Ws, REST, Zs, CNF2),
    append(CNF1, CNF2, CNF).

sum(LON, Zs, CNF) :-
    sum([-1], LON, Zs, CNF),!.

/* ---------------------------- TASK 4 ---------------------------- */