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
add([X], [Y], Cin, [Z,Cout], CNF) :-
    full_adder(X, Y, Cin, Z, Cout, CNF).

% Case for X=0 Y!=0
add([X], [Y|Ys], Cin, [Z|Zs], [[-N]|CNF]) :-
    full_adder(X, Y, Cin, Z, Cout, CNF1),
    add([N], Ys, Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF).

% Case for X!=0 Y=0
add([X|Xs], [Y], Cin, [Z|Zs], [[-N]|CNF]) :-
    full_adder(X, Y, Cin, Z, Cout, CNF1),
    add(Xs, [N], Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF).

% Case for X!=0 Y!=0
add([X|Xs], [Y|Ys], Cin, [Z|Zs], CNF) :-
    full_adder(X, Y, Cin, Z, Cout, CNF1),
    add(Xs, Ys, Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF).

add(Xs, Ys, Zs, [[-N]|CNF]) :-
    add(Xs, Ys, N, Zs, CNF).

/* ---------------------------- TASK 2 ---------------------------- */

pad(Xs, Size, Xs, []) :-
    Size =< 0,!.

pad(Xs, Size, PaddedXs, [[-N]|CNF]) :-
    append(Xs, [N], TempXs),
    pad(TempXs, (Size - 1), PaddedXs, CNF).

leq(Xs, Ys, CNF) :-
    length(Xs, Xs_Size),
    length(Ys, Ys_Size),
    pad(Ys, (Xs_Size - Ys_Size) + 1, PaddedYs, CNF1),
    add(Xs, _, PaddedYs, CNF2),
    append(CNF1, CNF2, CNF),!.

increment(Xs, Zs, [[P]|CNF]) :-
    add(Xs, [P], Zs, CNF),!.

lt(Xs, Ys, CNF) :-
    increment(Xs, Ws, CNF1),
    leq(Ws, Ys, CNF2),
    append(CNF1, CNF2, CNF),!.

/* ---------------------------- TASK 3 ---------------------------- */

sum(PREV, [], PREV, []).

sum(PREV, [Xs|REST], Zs, CNF) :-
    add(PREV, Xs, Ws, CNF1),
    sum(Ws, REST, Zs, CNF2),
    append(CNF1, CNF2, CNF).

sum(LON, Zs, [[-N]|CNF]) :-
    sum([N], LON, Zs, CNF).

/* ---------------------------- TASK 4 ---------------------------- */

shift(Xs, [N|Xs], [[-N]]).

list_shift([Xs], [Zs], CNF) :-
    shift(Xs, Zs, CNF).

list_shift([Xs|OLD_LON], [Zs|LON], CNF) :-
    shift(Xs, Zs, CNF),
    list_shift(OLD_LON, LON, CNF).

partial_prod([N], _Ys, [N], [[-N]]).
partial_prod([P], Ys, Ys, [[P]]).

build_list([X], Ys, [Zs], CNF) :-
    partial_prod([X], Ys, Zs, CNF).

build_list([X|Xs], Ys, [Zs|LON], CNF) :-
    partial_prod([X], Ys, Zs, CNF1),
    build_list(Xs, Ys, PREV_LON, CNF2),
    list_shift(PREV_LON, LON, CNF3),
    append(CNF1, CNF2, CNF12),
    append(CNF12, CNF3, CNF).

times(Xs, Ys, Zs, CNF) :-
    build_list(Xs, Ys, LON, CNF1),
    sum(LON, Zs, CNF2),
    append(CNF1, CNF2, CNF).

% % base case for multiplying with zero.
% times(Zero, _Y, [-1], CNF) :- leq(Zero, [-1], CNF).

% % for 0 LSBF multiply last calucaltion by 2.
% times([-1|Xs], Ys, Zs, CNF) :-
%     lt([-1],Xs,CNF1),
%     times(Xs, Ys, PREV_Z, CNF2),
%     add(PREV_Z, PREV_Z, Zs, CNF3),
%     append(CNF1, CNF2, CNF12),
%     append(CNF12, CNF3, CNF).

% % for 1 LSBF calculate like 0 is the LSBF and then add Y.
% times([1|Xs], Ys, Zs, CNF) :-
%     times([-1|Xs], Ys, PREV_Zs, CNF1),
%     add(PREV_Zs, Ys, Zs, CNF2),
%     append(CNF1, CNF2, CNF).

/* ---------------------------- TASK 5 ---------------------------- */

power(0, _Xs, [P], [[P]]):-!.

power(N, Xs, Zs, CNF) :-
    New_N is (N - 1),
    power(New_N, Xs, PREV_Zs, CNF1),
    times(Xs, PREV_Zs, Zs, CNF2),
    append(CNF1, CNF2, CNF).

/*
Task 1
    Xs=[1,_], Ys=[_,_,_], add(Xs,Ys,Zs,Cnf), sat(Cnf).
    Zs=[1,-1,1,1], PaddedZs=[1,-1,1,1,-1], length(Xs,4), length(Ys,4), add(Xs,Ys,PaddedZs,Cnf), sat(Cnf).
Task 2
    Xs=[1,_,_], Ys=[_,_], leq(Xs,Ys,Cnf), sat(Cnf).
    Xs=[1,_,_], Ys=[_,_], lt(Xs,Ys,Cnf), sat(Cnf).
Task 3
    Xs1 = [-1, -1, -1, 1, -1], Xs2 = [-1, 1, -1, -1, -1], Xs3 = [1, -1, -1, -1, -1], sum([Xs1,Xs2,Xs3],Zs,Cnf), sat([Xs1,Xs2,Xs3|Cnf]).
    Zs = [-1,1,1,1,1], PaddedZs= [-1,1,1,1,1,-1,-1,-1], List = [Xs1, Xs2, Xs3], length(Xs1,5), length(Xs2,5), length(Xs3,5), sum(List,PaddedZs,Cnf), sat(Cnf).
Task 4
    Xs=[_,_], Ys=[_,_,_], times(Xs,Ys,Zs,Cnf),sat([Xs,Ys|Cnf]).
    Zs=[1,1,1,1], PaddedZs=[1,1,1,1,-1,-1,-1,-1],length(Xs,4), length(Ys,4), times(Xs,Ys,PaddedZs,Cnf), sat(Cnf).

Task 5
    Xs=[_,_,_], power(3,Xs,Zs,Cnf), sat([Xs|Cnf]).

*/