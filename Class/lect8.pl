
user:file_search_path(sat, '/home/mcodish/SATSOLVERS/satsolver').
user:file_search_path(bee, '/home/mcodish/SATSOLVERS/beeCompiler').

:- use_module(sat(satsolver)).
:- use_module(bee(bCompiler)).

%% ?- solve(partition(2,3),S).
%% verify:ok
%% S = [5, 3, 4] .

%% ?- solve(partition(2,5),S).
%% verify:ok
%% S = [20, 12, 16] .

%% ?- solve(partition(3,3),S).
%% verify:ok
%% S = [6, 3, 4, 5] 


solve(Instance,Solution):-
    encode(Instance,Map,Constraints),
    bCompile(Constraints,Cnf),
    sat(Cnf),
    decode(Map,Solution),
    (verify(Instance,Solution) -> true ; writeln(wrong)).

encode(partition(N,NumBits), map([B|List]), Constraints) :-
    UB is 2^NumBits - 1,
    length(List,N),
    declare_ints([B|List],1,UB, Cs1),
    sorted(List,Cs2),
    allPower(N,[B|List],[BN|Powers],UB,Cs3), % the UB for [B|List] 
    Cs4 = [int_array_plus(Powers,BN)],
    append([Cs1,Cs2,Cs3,Cs4],Constraints).

declare_ints([], _, _, []).
declare_ints([I|Is],LB,UB, [new_int(I,LB,UB)|Constraints]):-
    declare_ints(Is,LB,UB, Constraints).

allPower(_,[],[],_,[]).
allPower(N,[X|Xs],[P|Ps],UB,Constraints) :-
    power(N,X,P,UB,Cs1),
    allPower(N,Xs,Ps,UB,Cs2),
    append(Cs1,Cs2,Constraints).

% X is a bee integer variable in domain 1..2^N-1
power(N,X,P, UB, Constraints) :- % UB for X
    length(Xs,N),
    equal(Xs,X),
    NewUB is UB^N,
    Constraints = [new_int(P,1,NewUB),int_array_times(Xs,P)].

equal([],_).
equal([X|Xs],X) :- equal(Xs,X).

sorted([],[]).
sorted([_],[]).
sorted([I,J|Is],[int_leq(I,J)|Constraints]) :-
    sorted([J|Is],Constraints).


decode(map(Nums),Solution):-
    % use BEE auxiliary module to decode integers
    bDecode:decodeIntArray(Nums,Solution).

verify(Instance,List) :-
    Instance = partition(N,_NumBits),
    findall(P,(member(X,List),P is X^N),[Left|Right]),
    sum_list(Right,Sum),
    (Left =:= Sum -> writeln(verify:ok) ; writeln(verify:wrong)).
    
