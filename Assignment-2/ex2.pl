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
            [ -X, -Y, -Cin,  Cout]],!.

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

% For two LSB the carry in bit is 0 (-1)
add(Xs, Ys, Zs, CNF) :-
    add(Xs, Ys, -1, Zs, CNF),!.

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

/* 
    In bit_compare, the Cnf holds only if the leq property holds.
    - CompareIn represent if the X's that builds the NumBit Xs until now,
      was less or equal than the Y's that builds the NumBit Ys 
    - CompareOut represent if the leq property holds after the visit in the current X and Y bits.
*/
bit_compare(X, Y, CompareIn, CompareOut, CNF) :-
    CNF =  [[ X,  Y,  CompareIn, -CompareOut],
            [ X,  Y, -CompareIn,  CompareOut],
            [ X, -Y,  CompareOut],
            [-X,  Y, -CompareOut],
            [-X, -Y,  CompareIn, -CompareOut],
            [-X, -Y, -CompareIn,  CompareOut]].

% Case for |Xs|=|Ys| and we scanned through all the bits
compare([], [], CompareIn, CompareIn, []).

% Case for |Xs|<|Ys|. Keep scanning Ys, We might need a 1 bit to be bigger than X
compare([], [Y|Ys], CompareIn, CompareOut, [[-Padded_X]|CNF]) :-
    bit_compare(Padded_X, Y, CompareIn, CompareTemp, CNF1),
    compare([], Ys, CompareTemp, CompareOut, CNF2),
    append(CNF1, CNF2, CNF),!.

% Case for |Xs|>|Ys|. Make sure all X's are -1
compare([X|Xs], [], CompareIn, CompareOut, [[-X]|CNF]) :-
    compare(Xs, [], CompareIn, CompareOut, CNF),!.

% General Case, Scan both Xs and Ys and advance
compare([X|Xs], [Y|Ys], CompareIn, CompareOut, CNF) :-
    bit_compare(X, Y, CompareIn, CompareTemp, CNF1),
    compare(Xs, Ys, CompareTemp, CompareOut, CNF2),
    append(CNF1, CNF2, CNF),!.

% Empty Xs is leq than Ys as they are equal (Sending CompareIn = 1)
% The CompareOut needs to be 1 to make leq holds.
leq(Xs, Ys, CNF) :-
    compare(Xs, Ys, 1, 1, CNF),!.

% Empty Xs is not smaller than Ys as that they are equal (Sending CompareIn = -1)
% The CompareOut needs to be 1 to make lt holds.
lt(Xs, Ys, CNF) :-
    compare(Xs, Ys, -1, 1, CNF),!.

/* ---------------------------- TASK 3 ---------------------------- */

% Case for empty list, return the sum until now.
sum(Zs, [], Zs, []) :- !.

sum(PREV_Zs, [Xs|REST], Zs, CNF) :-
    add(PREV_Zs, Xs, CURR_Zs, CNF1),
    sum(CURR_Zs, REST, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

sum(LON, Zs, CNF) :-
    sum([-1], LON, Zs, CNF),!.

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

builld_times_list([X|Xs], Ys, [Zs|LON], CNF) :-
	bit_bin_prod(X, Ys, Zs, CNF1),
	builld_times_list(Xs, [-1|Ys], LON, CNF2),
	append(CNF1, CNF2, CNF),!.

times(Xs, Ys, Zs, CNF) :-
	builld_times_list(Xs, Ys, LON, CNF1),
	sum(LON, Zs, CNF2),
	append(CNF1, CNF2, CNF),!.

/* ---------------------------- TASK 5 ---------------------------- */

power(0, _Xs, [Z], [[Z]]) :- !.

power(1, Xs, Xs, []) :- !.

power(N, Xs, Zs, CNF) :-
    PREV_N is (N - 1),
    power(PREV_N, Xs, PREV_Zs, CNF1),
    times(Xs, PREV_Zs, Zs, CNF2),
    append(CNF1, CNF2, CNF),!.

/* ---------------------------- TASK 6 ---------------------------- */

build_pe_list(N, Zs, PREV_As, [As], [P_As], CNF) :-
    length(Zs, L_Zs),
    length(As, L_Zs),
    lt(As, Zs, CNF1),
    leq(PREV_As, As, CNF2),

    power(N, As, P_As, CNF3),
    append([CNF1, CNF2, CNF3], CNF),!.

build_pe_list(N, Zs, PREV_As, [As|LON], [P_As|P_LON], CNF) :-
    length(Zs, L_Zs),
    length(As, L_Zs),
    lt(As, Zs, CNF1),
    leq(PREV_As, As, CNF2),
    
    power(N, As, P_As, CNF3),
    build_pe_list(N, Zs, As, LON, P_LON, CNF4),
    append([CNF1, CNF2, CNF3, CNF4], CNF),!.

powerEquation(N, M, Zs, LON, CNF) :-
    length(LON, M),
    build_pe_list(N, Zs, [-1], LON, P_LON, CNF1),
    sum(P_LON, P_Zs1, CNF2),
    power(N, Zs, P_Zs2, CNF3),
    leq(P_Zs1, P_Zs2, CNF4),
    leq(P_Zs2, P_Zs1, CNF5),
    append([CNF1, CNF2, CNF3, CNF4, CNF5], CNF),!.

/* ---------------------------- TASK 7 ---------------------------- */

encode(euler(N, NumBits), [Zs|LON], CNF) :-
    M is N - 1,
    length(Zs, NumBits),
    powerEquation(N, M, Zs, LON, CNF),!.

bin_to_dec(_I, [], 0).

bin_to_dec(I, [1|Xs], Z) :-
    Ipp is (I + 1),
    bin_to_dec(Ipp, Xs, PREV_Z),
    Z is (PREV_Z + (2 ** I)),!.

bin_to_dec(I, [-1|Xs], Z) :-
    Ipp is (I + 1),
    bin_to_dec(Ipp, Xs, Z),!.

bin_to_dec(Xs, Z) :-
    bin_to_dec(0, Xs, Z),!.

bins_to_decs([], []).

bins_to_decs([Xs|LOXs], [D|LOD]) :-
    bin_to_dec(Xs, D),
    bins_to_decs(LOXs, LOD),!.

decode(Map,Solution) :-
    bins_to_decs(Map, Solution).

build_pe_dec_list(N,[As], [P_As]) :-
    P_As is (As ** N),!.

build_pe_dec_list(N, [As|LON], [P_As|P_LON]) :-
    P_As is (As ** N),
    build_pe_dec_list(N, LON, P_LON),!.

verify(euler(N, _NumBits), [_B|As]) :-
    length(As, As_L),
    As_L =\= (N - 1),!,
    writeln(wrong_length).

verify(euler(N, _NumBits), [B|As]) :-
    build_pe_dec_list(N, As, P_As),
    sumlist(P_As, RHS),
    RHS =\= (B ** N),!,
    writeln(lhs_not_equal_rhs).

verify(_,_) :-
        writeln(verified:ok).

solve(Instance, Solution) :-
    encode(Instance, Map, Cnf),
    sat(Cnf),
    decode(Map, Solution),
    verify(Instance, Solution).

/* ---------------------------- TASK 8 ---------------------------- */

encodeAll(partition(N, NumBits), [Zs|LON], Cnf) :-
    length(Zs, NumBits),
    powerEquation(N, N, Zs, LON, Cnf),!.

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss),!.

trans([], []).
trans([F|Fs], Ts) :-
    trans(F, [F|Fs], Ts),!.

trans([], _, []).
trans([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        trans(Rs, Ms1, Tss),!.

decodeAll([Map],[Sol]) :-
    trans(Map, T_Map),
    bins_to_decs(T_Map, Sol),!.

decodeAll([Map|Maps],[Sol|Sols]) :-
    trans(Map, T_Map),
    bins_to_decs(T_Map, Sol),
    decodeAll(Maps, Sols),!.

verify2(partition(N, _NumBits), [_B|As]) :-
    length(As, As_L),
    As_L =\= N,!,
    writeln(wrong_length).

verify2(partition(N, _NumBits), [B|As]) :-
    build_pe_dec_list(N, As, P_As),
    sumlist(P_As, RHS),
    RHS =\= (B ** N),!,
    writeln(lhs_not_equal_rhs).

verifyAll(partition(N, NumBits), [Sol]) :-
    verify2(partition(N, NumBits), Sol),!.

verifyAll(partition(N, NumBits), [Sol|Sols]) :-
    verify2(partition(N, NumBits), Sol),
    verifyAll(partition(N, NumBits), Sols),!.

verifyAll(_,_) :-
        writeln(verified:ok),!.

solveAll(Instance, Solution) :-
    encodeAll(Instance, Map, Cnf),
    satMulti(Cnf, 1000, _Count, _Time),
    decodeAll(Map, T_Solution),
    trans(T_Solution, Solution),
    verifyAll(Instance, Solution).

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

Task 6
    Zs=[_,_,_], powerEquation(2,2,Zs,List,Cnf), sat([Zs|Cnf]).

Task 7
statistics(cputime,Time1), solve(euler(5,8), Solution), statistics(cputime,Time2), Time12 is floor(Time2-Time1).

Task 8
    statistics(cputime,Time1), solveAll(partition(2,5), Solutions), statistics(cputime,Time2), Time12 is (Time2-Time1).
    statistics(cputime,Time1), solveAll(partition(3,4), Solutions), statistics(cputime,Time2), Time12 is (Time2-Time1).
    statistics(cputime,Time1), solveAll(partition(4,9), Solutions), statistics(cputime,Time2), Time12 is (Time2-Time1).
    statistics(cputime,Time1), solveAll(partition(5,7), Solutions), statistics(cputime,Time2), Time12 is (Time2-Time1).
*/