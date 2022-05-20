user:file_search_path(xxxx, 'C:/Users/paney/Documents/Studies/Logic_Programming_Class/Class/PLSATsolver').
:- use_module(xxxx(satsolver)).



/*
 An instance is a number N.

 represent the NxN board as a list of all N^2 terms of the form
 queenAt(I,J,B) where I,J are indices and B is true or false

  ?- solve(4,Solution).
  Solution = [
  queenAt(1,1,false),queenAt(1,2,false),queenAt(1,3,true),queenAt(1,4,false),
  queenAt(2,1,true),queenAt(2,2,false),queenAt(2,3,false),queenAt(2,4,false),
  queenAt(3,1,false),queenAt(3,2,false),queenAt(3,3,false),queenAt(3,4,true),
  queenAt(4,1,false),queenAt(4,2,true),queenAt(4,3,false),queenAt(4,4,false)
  ]


*/
%%%%%%%%%%%%%%%%% the template
solve(Instance, Solution) :-
    % Instance is a number N
    encode(Instance, Map, Cnf),   
    sat(Cnf), 
    decode(Map, Solution),
    verify(Instance, Solution).


encode(N,Map,Cnf) :-
    % make the board - represented as a list with Boolean variable
    % at each coordinate (I,J) of the board
    findall(queenAt(I,J,_),(between(1,N,I),between(1,N,J)),Map),
    
    %collect the pairs of attacking cells (by position in the map)
    findall((S,T), (nth1(S,Map,queenAt(I1,J1,_)),
                    nth1(T,Map,queenAt(I2,J2,_)), S<T,
                    attack(I1,J1,I2,J2)
                   ), List),
    
    noAttackClauses(List,Map,Cnf1),
    atLeastOneQueenPerRow(Map,Cnf2),
    append(Cnf1,Cnf2,Cnf).

noAttackClauses([],_,[]).
noAttackClauses([(S,T)|Pairs],Map,[[-X,-Y]|Cnf]) :-
    nth1(S,Map,queenAt(_,_,X)),
    nth1(T,Map,queenAt(_,_,Y)),
    noAttackClauses(Pairs,Map,Cnf).

atLeastOneQueenPerRow([],[]).
atLeastOneQueenPerRow([queenAt(_,_,X)],[[X]]).
atLeastOneQueenPerRow([queenAt(I,_,X),queenAt(I,_,Y)|Map],[[X|Xs]|Cnf]) :-
        atLeastOneQueenPerRow([queenAt(I,_,Y)|Map],[Xs|Cnf]).
atLeastOneQueenPerRow([queenAt(I,_,X),queenAt(I1,_,Y)|Map],[[X]|Cnf]) :-
        I1 is I+1,
        atLeastOneQueenPerRow([queenAt(I1,_,Y)|Map],Cnf).



decode([],[]).
decode([queenAt(I,J,X)|Xs],[queenAt(I,J,true)|Qs]) :-
        X = 1,!,
        decode(Xs,Qs).
decode([queenAt(I,J,_)|Xs],[queenAt(I,J,false)|Qs]) :-
        decode(Xs,Qs).


verify(N,Solution) :- 
    length(Solution,N1),
    N1 =\= N*N, !,
    writeln(board_size_wrong).

verify(N,Solution) :-
    between(1,N,I), between(1,N,J),
    \+ member(queenAt(I,J,_), Solution), !,
    writeln(missingCell(I,J)).

verify(N,Solution) :-
    between(1,N,J),
    findall((I,J),member(queenAt(I,J,true),Solution),List),
    length(List,N1),
    N1 =\= 1, !,
    writeln(numberQueensAtRow(J,N1 =\= 1)).
           
verify(_,Solution) :-
        append(_,[queenAt(I1,J1,true)|Qs],Solution),
        member(queenAt(I2,J2,true),Qs),
        attack(I1,J1,I2,J2), !,
        writeln(queens_attack(I1,J1,I2,J2)).

verify(_,_) :-
        writeln(verified:ok).

attack(I,_,I,_).
attack(_,J,_,J).
attack(I1,J1,I2,J2) :-
        abs(I1-I2) =:= abs(J1-J2).



