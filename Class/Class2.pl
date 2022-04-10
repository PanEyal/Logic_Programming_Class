s(0).
s(s(X)) :- s(X).

nat(X) :- s(X).

unary_plus(0,X,X) :- nat(X).
unary_plus(s(X),Y,s(Z)) :- unary_plus(X,Y,Z).

unary_times(0,X,0) :- nat(X).
unary_times(s(_),0,0).
unary_times(s(X),s(Y),Z) :-
    unary_plus(s(Y),W,Z),
    unary_times(X,s(Y),W).

% less or equal
leq(0,_).
leq(s(X),s(Y)) :- leq(X,Y).

greater(s(_),0).
greater(s(X),s(Y)) :- greater(X,Y).

% binary_list
b_list([]).
b_list([X|Xs]) :-
    member(X,[0,1]),
    b_list(Xs).

b_list(Xs,N) :-
    length(Xs,N),
    b_list(Xs).










