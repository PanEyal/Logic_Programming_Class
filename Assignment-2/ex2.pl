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

By negating all the false values rows, we can convert from DNF to Cnf.
Therefore the Cnf for the full adder is:

*/
full_adder(X, Y, Cin, Z, Cout, Cnf) :-
    Cnf =  [[  X,  Y,  Cin, -Z],
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
add([X], [Y], Cin, [Z,Cout], Cnf) :-
    full_adder(X, Y, Cin, Z, Cout, Cnf),!.

% Case for X=0 Y!=0
add([X], [Y|Ys], Cin, [Z|Zs], [[-N]|Cnf]) :-
    full_adder(X, Y, Cin, Z, Cout, Cnf1),
    add([N], Ys, Cout, Zs, Cnf2),
    append(Cnf1, Cnf2, Cnf),!.

% Case for X!=0 Y=0
add([X|Xs], [Y], Cin, [Z|Zs], [[-N]|Cnf]) :-
    full_adder(X, Y, Cin, Z, Cout, Cnf1),
    add(Xs, [N], Cout, Zs, Cnf2),
    append(Cnf1, Cnf2, Cnf),!.

% Case for X!=0 Y!=0
add([X|Xs], [Y|Ys], Cin, [Z|Zs], Cnf) :-
    full_adder(X, Y, Cin, Z, Cout, Cnf1),
    add(Xs, Ys, Cout, Zs, Cnf2),
    append(Cnf1, Cnf2, Cnf),!.

% For two LSB the carry in bit is 0 (-1)
add(Xs, Ys, Zs, Cnf) :-
    add(Xs, Ys, -1, Zs, Cnf),!.

/* ---------------------------- TASK 2 ----------------------------

---- Less Equal/Than Truth Table ----

    X  Y CompareIn |  CompareOut | Value
___________________|_____________|________
    0  0      0    |      0      |   T
    0  0      0    |      1      |   F
                   |             |
    1  0      0    |      0      |   T
    1  0      0    |      1      |   F
                   |             |
    0  1      0    |      0      |   F
    0  1      0    |      1      |   T
                   |             |
    1  1      0    |      0      |   T
    1  1      0    |      1      |   F
                   |             |
    0  0      1    |      0      |   F
    0  0      1    |      1      |   T
                   |             |
    1  0      1    |      0      |   T
    1  0      1    |      1      |   F
                   |             |
    0  1      1    |      0      |   F
    0  1      1    |      1      |   T
                   |             |
    1  1      1    |      0      |   F
    1  1      1    |      1      |   T
                   |             |
    
    When X > Y, CompareIn is irrelevant -> CompareOut is true
    And when X < Y, CompareIn is irrelevent -> CompareOut is false

    By negating all the false values rows, we can convert from DNF to Cnf.
    Therefore the Cnf for the Less Equal/Than is:
*/

/* 
    In bit_compare, the Cnf holds only if the leq property holds.
    - CompareIn represent if the X's that builds the NumBit Xs until now,
      was less or equal than the Y's that builds the NumBit Ys 
    - CompareOut represent if the leq property holds after the visit in the current X and Y bits.
*/
bit_compare(X, Y, CompareIn, CompareOut, Cnf) :-
    Cnf =  [[ X,  Y,  CompareIn, -CompareOut],
            [ X,  Y, -CompareIn,  CompareOut],
            [ X, -Y,  CompareOut],
            [-X,  Y, -CompareOut],
            [-X, -Y,  CompareIn, -CompareOut],
            [-X, -Y, -CompareIn,  CompareOut]].

% Case for |Xs|=|Ys| and we scanned through all the bits
compare([], [], CompareIn, CompareIn, []).

% Case for |Xs|<|Ys|. Keep scanning Ys, We might need a 1 bit to be bigger than X
compare([], [Y|Ys], CompareIn, CompareOut, [[-Padded_X]|Cnf]) :-
    bit_compare(Padded_X, Y, CompareIn, CompareTemp, Cnf1),
    compare([], Ys, CompareTemp, CompareOut, Cnf2),
    append(Cnf1, Cnf2, Cnf),!.

% Case for |Xs|>|Ys|. Make sure all X's are -1
compare([X|Xs], [], CompareIn, CompareOut, [[-X]|Cnf]) :-
    compare(Xs, [], CompareIn, CompareOut, Cnf),!.

% General Case, Scan both Xs and Ys and advance
compare([X|Xs], [Y|Ys], CompareIn, CompareOut, Cnf) :-
    bit_compare(X, Y, CompareIn, CompareTemp, Cnf1),
    compare(Xs, Ys, CompareTemp, CompareOut, Cnf2),
    append(Cnf1, Cnf2, Cnf),!.

% Empty Xs is leq than Ys as they are equal (Sending CompareIn = 1)
% The CompareOut needs to be 1 to make leq holds.
leq(Xs, Ys, Cnf) :-
    compare(Xs, Ys, 1, 1, Cnf),!.

% Empty Xs is not smaller than Ys as that they are equal (Sending CompareIn = -1)
% The CompareOut needs to be 1 to make lt holds.
lt(Xs, Ys, Cnf) :-
    compare(Xs, Ys, -1, 1, Cnf),!.

/* ---------------------------- TASK 3 ---------------------------- */

% Case for empty list, return the sum until now.
sum(Zs, [], Zs, []) :- !.

% General Case, Iterate on list and sum it up
sum(ZsPrev, [Xs|Rest], Zs, Cnf) :-
    add(ZsPrev, Xs, ZsCurr, Cnf1),
    sum(ZsCurr, Rest, Zs, Cnf2),
    append(Cnf1, Cnf2, Cnf),!.

% Call for iterate function. Start with value 0 (= -1)
% LON stands for List of Numbers
sum(LON, Zs, Cnf) :-
    sum([-1], LON, Zs, Cnf),!.

/* ---------------------------- TASK 4 ----------------------------

---- bit on bit product Truth Table ----

    X  Y |  Z  | Value
_________|_____|_______
    0  0 |  0  |   T
    0  0 |  1  |   F
         |     |
    1  0 |  0  |   T
    1  0 |  1  |   F
         |     |
    0  1 |  0  |   T
    0  1 |  1  |   F
         |     |
    1  1 |  0  |   F
    1  1 |  1  |   T
         |     |

    When X is 0 or Y is 0 -> Z value is 0
    We can minimize some rows in the Cnf in that manner.

    By negating all the false values rows, we can convert from DNF to Cnf.
    Therefore the Cnf for the bit on bit product is:

*/

bit_bit_prod(X, Y, Z, Cnf):-
    Cnf =  [[  X, -Z],
            [  Y, -Z],
            [ -X, -Y,  Z]],!.

% Base case multiply single bit entry X with last bit of Y
bit_bin_prod(X, [Y], [Z], Cnf) :-
    bit_bit_prod(X, Y, Z, Cnf),!.

% Iterate over Ys and multiply each bit by X
bit_bin_prod(X, [Y|Ys], [Z|Zs], Cnf) :-
    bit_bit_prod(X, Y, Z, Cnf1),
    bit_bin_prod(X, Ys, Zs, Cnf2),
    append(Cnf1, Cnf2, Cnf),!.

% Last bit entry X, multyply it by Ys
builld_times_list([X], Ys, [Zs], Cnf) :-
	bit_bin_prod(X, Ys, Zs, Cnf),!.

% Iterate over Xs and multiply each bit of it by Ys
builld_times_list([X|Xs], Ys, [Zs|LON], Cnf) :-
	bit_bin_prod(X, Ys, Zs, Cnf1),
	builld_times_list(Xs, [-1|Ys], LON, Cnf2),
	append(Cnf1, Cnf2, Cnf),!.

% (Long Multiplication)
% Build producs of each X bit with Ys with the right shift.
% Then sum it all up
times(Xs, Ys, Zs, Cnf) :-
	builld_times_list(Xs, Ys, LON, Cnf1),
	sum(LON, Zs, Cnf2),
	append(Cnf1, Cnf2, Cnf),!.

/* ---------------------------- TASK 5 ---------------------------- */

% Define power by 0 to be 1
power(0, _Xs, [Z], [[Z]]) :- !.

% Base Case for power by 1 to be Xs
power(1, Xs, Xs, []) :- !.

% Multiply by Xs N times
power(N, Xs, Zs, Cnf) :-
    NPrev is (N - 1),
    power(NPrev, Xs, ZsPrev, Cnf1),
    times(Xs, ZsPrev, Zs, Cnf2),
    append(Cnf1, Cnf2, Cnf),!.

/* ---------------------------- TASK 6 ---------------------------- */

% Base Case for one element in list
build_pe_list(N, Zs, AsPrev, [As], [P_As], Cnf) :-
    length(Zs, L_Zs),
    length(As, L_Zs),
    lt(As, Zs, Cnf1),
    leq(AsPrev, As, Cnf2),

    power(N, As, P_As, Cnf3),
    append([Cnf1, Cnf2, Cnf3], Cnf),!.

% Iterate over LON (list of numbers) and build a new list of their power
build_pe_list(N, Zs, AsPrev, [As|LON], [P_As|P_LON], Cnf) :-
    length(Zs, L_Zs),
    length(As, L_Zs),
    lt(As, Zs, Cnf1),
    leq(AsPrev, As, Cnf2),
    
    power(N, As, P_As, Cnf3),
    build_pe_list(N, Zs, As, LON, P_LON, Cnf4),
    append([Cnf1, Cnf2, Cnf3, Cnf4], Cnf),!.

% Return Cnf that satisfies Xs = Ys
equal(Xs, Ys, Cnf) :-
    leq(Xs, Ys, Cnf1),
    leq(Ys, Xs, Cnf2),
    append(Cnf1, Cnf2, Cnf).

% Build new list of the power of each element.
% Sum it up and compare with the power of Z
% powerEquation also sorts LON (list of numbers) for Task 7 and 8
% given unsorted LON into powerEquation will results in failure.
powerEquation(N, M, Zs, LON, Cnf) :-
    length(LON, M),
    build_pe_list(N, Zs, [-1], LON, P_LON, Cnf1),
    sum(P_LON, P_Zs1, Cnf2),
    power(N, Zs, P_Zs2, Cnf3),
    equal(P_Zs1, P_Zs2, Cnf4),
    append([Cnf1, Cnf2, Cnf3, Cnf4], Cnf),!.

/* ---------------------------- TASK 7 ---------------------------- */

/* --------- encode code --------- */

% The encode Code, LON stands for list of numbers
encode(euler(N, NumBits), [Zs|LON], Cnf) :-
    M is N - 1,
    length(Zs, NumBits),
    powerEquation(N, M, Zs, LON, Cnf),!.

/* --------- decode code --------- */

% bin_to_dec take a NumBits representation and convert it to decimal
bin_to_dec(_I, [], 0).

bin_to_dec(I, [1|Xs], Z) :-
    bin_to_dec((I + 1), Xs, ZPrev),
    Z is (ZPrev + (2 ** I)),!.

bin_to_dec(I, [-1|Xs], Z) :-
    bin_to_dec((I + 1), Xs, Z),!.

bin_to_dec(Xs, Z) :-
    bin_to_dec(0, Xs, Z),!.

% bins_to_decs take a list of NumBits and convert it to list of decimals
bins_to_decs([], []).

% LOXs stands for List of Xs
bins_to_decs([Xs|LOXs], [D|LOD]) :-
    bin_to_dec(Xs, D),
    bins_to_decs(LOXs, LOD),!.

% To decode -> convert from NumBits to decimal
decode(Map,Solution) :-
    bins_to_decs(Map, Solution).

/* --------- verify code --------- */

% build_pe_dec_list build a powerEquation from decimals
build_pe_dec_list(N,[As], [P_As]) :-
    P_As is (As ** N),!.

build_pe_dec_list(N, [As|LON], [P_As|P_LON]) :-
    P_As is (As ** N),
    build_pe_dec_list(N, LON, P_LON),!.

% Make sure right hand side of the equation equals to left hand side
verify(euler(N, _NumBits), [B|As]) :-
    build_pe_dec_list(N, As, P_As),
    sumlist(P_As, RHS),
    RHS =\= (B ** N),!,
    writeln(lhs_not_equal_rhs).

% make sure the right hand side composed from (N - 1) elements
verify(euler(N, _NumBits), [_B|As]) :-
    length(As, As_L),
    As_L =\= (N - 1),!,
    writeln(wrong_length).

% No issues found
verify(_,_) :-
        writeln(verified:ok).

/* --------- solve code --------- */

solve(Instance, Solution) :-
    encode(Instance, Map, Cnf),
    sat(Cnf),
    decode(Map, Solution),
    verify(Instance, Solution).

/* ---------------------------- TASK 8 ---------------------------- */

/* --------- encode code --------- */

encodeAll(partition(N, NumBits), [Zs|LON], Cnf) :-
    length(Zs, NumBits),
    powerEquation(N, N, Zs, LON, Cnf),!.

/* --------- decode code --------- */

% Transpose code for Matrix, Took from an old SWI's code of clpfd library
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

% decode each solution the same as in Task 7, and gather into one list
decode2([Map],[Sol]) :-
    trans(Map, T_Map),
    bins_to_decs(T_Map, Sol),!.

decode2([Map|Maps],[Sol|Sols]) :-
    trans(Map, T_Map),
    bins_to_decs(T_Map, Sol),
    decode2(Maps, Sols),!.

% To decode -> convert each sufficient clause from NumBits to decimal
decodeAll(Map,Solution) :-
    decode2(Map, T_Solution),
    trans(T_Solution, Solution).


/* --------- verify code --------- */

% Make sure for each sufficient clause that right hand side of the equation equals to left hand side
verify2(partition(N, _NumBits), [B|As]) :-
    build_pe_dec_list(N, As, P_As),
    sumlist(P_As, RHS),
    RHS =\= (B ** N),!,
    writeln(lhs_not_equal_rhs).

% make sure for each sufficient clause that the right hand side composed from N elements
verify2(partition(N, _NumBits), [_B|As]) :-
    length(As, As_L),
    As_L =\= N,!,
    writeln(wrong_length).

% verify each sufficient clause
verifyAll(partition(N, NumBits), [Sol]) :-
    verify2(partition(N, NumBits), Sol),!.

verifyAll(partition(N, NumBits), [Sol|Sols]) :-
    verify2(partition(N, NumBits), Sol),
    verifyAll(partition(N, NumBits), Sols),!.

% No issues found
verifyAll(_,_) :-
        writeln(verified:ok),!.

/* --------- solve code --------- */

solveAll(Instance, Solution) :-
    encodeAll(Instance, Map, Cnf),
    satMulti(Cnf, 1000, _Count, _Time),
    decodeAll(Map, Solution),
    verifyAll(Instance, Solution).

/* ---------------------------- TESTING CODE ---------------------------- 

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