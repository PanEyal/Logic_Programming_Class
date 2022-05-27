/* I, Pan Eyal (208722058) assert that the work I submitted is entirely my own.
I have not received any part from any other student in the class (or other source),
nor did I give parts of it for use to others. I have clearly marked in the comments
of my program any code taken from an external source. */

user:file_search_path(xxxx, 'C:/Users/paney/Documents/Studies/Logic_Programming_Class/Assignment-2/satsolver').
:- use_module(xxxx(satsolver)).


/* ---------------------------- TASK 1 ---------------------------- */

/*
---- Full Adder Truth Table ----
    X  Y Cin |  Z  | Value
_____________|_____|________
    0  0  0  |  0  |   T
    0  0  0  |  1  |   F
             |     |
    1  0  0  |  0  |   F
    1  0  0  |  1  |   T
             |     |
    0  1  0  |  0  |   F
    0  1  0  |  1  |   T
             |     |
    1  1  0  |  0  |   T
    1  1  0  |  1  |   F
             |     |
    0  0  1  |  0  |   F
    0  0  1  |  1  |   T
             |     |
    1  0  1  |  0  |   T
    1  0  1  |  1  |   F
             |     |
    0  1  1  |  0  |   T
    0  1  1  |  1  |   F
             |     |
    1  1  1  |  0  |   F
    1  1  1  |  1  |   T
             |     |

    X  Y Cin |  Cout | Value
_____________|_______|________
    0  0  0  |   0   |   T
    0  0  0  |   1   |   F
             |       |
    1  0  0  |   0   |   T
    1  0  0  |   1   |   F
             |       |
    0  1  0  |   0   |   T
    0  1  0  |   1   |   F
             |       |
    1  1  0  |   0   |   F
    1  1  0  |   1   |   T
             |       |
    0  0  1  |   0   |   T
    0  0  1  |   1   |   F
             |       |
    1  0  1  |   0   |   F
    1  0  1  |   1   |   T
             |       |
    0  1  1  |   0   |   F
    0  1  1  |   1   |   T
             |       |
    1  1  1  |   0   |   F
    1  1  1  |   1   |   T
             |       |

By negating all the false values rows, we can convert from DNF to CNF.
Therefore the CNF for the full adder is:

*/
full_adder(X, Y, Cin, Z, Cout, CNF) :-
    CNF =  [[  X,  Y,  Cin, -Z],
            [ -X,  Y,  Cin,  Z],
            [  X, -Y,  Cin,  Z],
            [ -X, -Y,  Cin, -Z],
            [  X,  Y, -Cin,  Z],
            [ -X,  Y, -Cin, -Z],
            [  X, -Y, -Cin, -Z],
            [ -X, -Y, -Cin,  Z],

            [  X,  Y , Cin, -Cout],
            [ -X,  Y,  Cin, -Cout],
            [  X, -Y,  Cin, -Cout],
            [ -X, -Y,  Cin,  Cout],
            [  X,  Y, -Cin, -Cout],
            [ -X,  Y, -Cin,  Cout],
            [  X, -Y, -Cin,  Cout],
            [ -X, -Y, -Cin,  Cout]].

% Base case for X=0, Y=0
add([X], [Y], Cin, [Z,Cout], CNF) :-
    full_adder(X, Y, Cin, Z, Cout, CNF),!.

% Case for X=0 Y!=0
add([X], [Y|Ys], Cin, [Z|Zs], [[-N]|CNF]) :-
    full_adder(X, Y, Cin, Z, Cout, CNF1),
    add([N], Ys, Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

% Case for X!=0 Y=0
add([X|Xs], [Y], Cin, [Z|Zs], [[-N]|CNF]) :-
    full_adder(X, Y, Cin, Z, Cout, CNF1),
    add(Xs, [N], Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

% Case for X!=0 Y!=0
add([X|Xs], [Y|Ys], Cin, [Z|Zs], CNF) :-
    full_adder(X, Y, Cin, Z, Cout, CNF1),
    add(Xs, Ys, Cout, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

add(Xs, Ys, Zs, [[-N]|CNF]) :-
    add(Xs, Ys, N, Zs, CNF),!.

/* ---------------------------- TASK 2 ---------------------------- 

---- Less Equal/Than Truth Table ----

    X  Y LEQin |  LEQout | Value
_______________|_________|________
    0  0   0   |    0    |   T
    0  0   0   |    1    |   F
               |         |
    1  0   0   |    0    |   T
    1  0   0   |    1    |   F
               |         |
    0  1   0   |    0    |   F
    0  1   0   |    1    |   T
               |         |
    1  1   0   |    0    |   T
    1  1   0   |    1    |   F
               |         |
    0  0   1   |    0    |   F
    0  0   1   |    1    |   T
               |         |
    1  0   1   |    0    |   T
    1  0   1   |    1    |   F
               |         |
    0  1   1   |    0    |   F
    0  1   1   |    1    |   T
               |         |
    1  1   1   |    0    |   F
    1  1   1   |    1    |   T
               |         |
    
    By negating all the false values rows, we can convert from DNF to CNF.
    Therefore the CNF for the Less Equal/Than is:
*/

bit_leq(X, Y, LEQin, LEQout, CNF) :-
    CNF =  [[ X,  Y,  LEQin, -LEQout],
            [ X,  Y, -LEQin,  LEQout],
            [ X, -Y,  LEQout],
            [-X,  Y, -LEQout],
            [-X, -Y,  LEQin, -LEQout],
            [-X, -Y, -LEQin,  LEQout]].

leq([], [], LEQin, LEQin, []).

leq([], [Y|Ys], LEQin, LEQout, [[-Pad_X]|CNF]) :-
    bit_leq(Pad_X, Y, LEQin, LEQtemp, CNF1),
    leq([], Ys, LEQtemp, LEQout, CNF2),
    append(CNF1, CNF2, CNF),!.

leq([X|Xs], [], LEQin, LEQout, [[-X]|CNF]) :-
    leq(Xs, [], LEQin, LEQout, CNF),!.

leq([X|Xs], [Y|Ys], LEQin, LEQout, CNF) :-
    bit_leq(X, Y, LEQin, LEQtemp, CNF1),
    leq(Xs, Ys, LEQtemp, LEQout, CNF2),
    append(CNF1, CNF2, CNF),!.

leq(Xs, Ys, [[P]|CNF]) :-
    leq(Xs, Ys, P, P, CNF),!.

lt(Xs, Ys, [[P],[-N]|CNF]) :-
    leq(Xs, Ys, N, P, CNF),!.

/* ---------------------------- TASK 3 ---------------------------- */

sum(Zs, [], Zs, []) :- !.

sum(PREV_Zs, [Xs|REST], Zs, CNF) :-
    add(PREV_Zs, Xs, CURR_Zs, CNF1),
    sum(CURR_Zs, REST, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

sum(LON, Zs, [[-N]|CNF]) :-
    sum([N], LON, Zs, CNF),!.

/* ---------------------------- TASK 4 ---------------------------- */

bit_bit_prod(X, Y, Z, CNF):-
    CNF =  [[  X, -Z],
            [  Y, -Z],
            [ -X, -Y,  Z]],!.

bit_bin_prod(X, [Y], [Z], CNF) :-
    bit_bit_prod(X, Y, Z, CNF),!.

bit_bin_prod(X, [Y|Ys], [Z|Zs], CNF) :-
    bit_bit_prod(X, Y, Z, CNF1),
    bit_bin_prod(X, Ys, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

builld_times_list([X], Ys, [Zs], CNF) :-
	bit_bin_prod(X, Ys, Zs, CNF),!.

builld_times_list([X|Xs], Ys, [Zs|LON], [[-N]|CNF]) :-
	bit_bin_prod(X, Ys, Zs, CNF1),
	builld_times_list(Xs, [N|Ys], LON, CNF2),
	append(CNF1, CNF2, CNF),!.

times(Xs, Ys, Zs, CNF) :-
	builld_times_list(Xs, Ys, LON, CNF1),
	sum(LON, Zs, CNF2),
	append(CNF1, CNF2, CNF),!.

/* ---------------------------- TASK 5 ---------------------------- */

power(0, _Xs, [Z], [[Z]]) :- !.

power(1, Xs, Xs, []) :- !.

power(N, Xs, Zs, CNF) :-
    0 is (N mod 2),
    PREV_N is (N / 2),
    power(PREV_N, Xs, PREV_Zs, CNF1),
    times(PREV_Zs, PREV_Zs, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

power(N, Xs, Zs, CNF) :-
    1 is (N mod 2),
    PREV_N is (N - 1),
    power(PREV_N, Xs, PREV_Zs, CNF1),
    times(Xs, PREV_Zs, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

/* ---------------------------- TASK 6 ---------------------------- */

build_pe_list(N, Zs, [As], [P_As], CNF) :-
    length(Zs, L_Zs),
    length(As, L_Zs),
    lt(As, Zs, CNF1),

    power(N, As, P_As, CNF2),
    append(CNF1, CNF2, CNF),!.

build_pe_list(N, Zs, [As|LON], [P_As|P_LON], CNF) :-
    length(Zs, L_Zs),
    length(As, L_Zs),
    lt(As, Zs, CNF1),

    power(N, As, P_As, CNF2),
    build_pe_list(N, Zs, LON, P_LON, CNF3),
    append([CNF1, CNF2, CNF3], CNF),!.

powerEquation(N, M, Zs, LON, CNF) :-
    length(LON, M),
    build_pe_list(N, Zs, LON, P_LON, CNF1),
    sum(P_LON, P_Zs1, CNF2),
    power(N, Zs, P_Zs2, CNF3),
    leq(P_Zs1, P_Zs2, CNF4),
    leq(P_Zs2, P_Zs1, CNF5),
    append([CNF5, CNF4, CNF3, CNF2, CNF1], CNF),!.

/* ---------------------------- TASK 7 ---------------------------- */

encode(euler(N, NumBits), [Zs|LON], CNF) :-
    M is N - 1,
    length(Zs, NumBits),
    powerEquation(N, M, Zs, LON, CNF).

bin_to_dec(_I, [], 0).

bin_to_dec(I, [1|Xs], Z) :-
    Ipp is (I + 1),
    bin_to_dec(Ipp, Xs, PREV_Z),
    Z is (PREV_Z + (2 ** I)).

bin_to_dec(I, [-1|Xs], Z) :-
    Ipp is (I + 1),
    bin_to_dec(Ipp, Xs, Z).

bin_to_dec(Xs, Z) :-
    bin_to_dec(0, Xs, Z).

bins_to_decs([], []).

bins_to_decs([Xs|LOXs], [D|LOD]) :-
    bin_to_dec(Xs, D),
    bins_to_decs(LOXs, LOD).

decode(Map,Solution) :-
    bins_to_decs(Map, Solution).

solve(Instance, Solution) :-
    encode(Instance,Map,Cnf),
    sat(Cnf),
    decode(Map,Solution).

/* ------------------------------------------ */

get_cnf_size(N, NumBits, A) :-
    encode(euler(N, NumBits), _Map, Cnf),
    length(Cnf, A).

/*169
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

Task 6
    Zs=[_,_,_], powerEquation(2,2,Zs,List,Cnf), sat([Zs|Cnf]).
statistics(cputime,Time1), solve(euler(5,8), Solution), statistics(cputime,Time2), Time12 is floor(Time2-Time1).
*/