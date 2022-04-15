/**** I, Pan Eyal (208722058) assert that the work I submitted is entirely my own.
I have not received any part from any other student in the class (or other source),
nor did I give parts of it for use to others. I have clearly marked in the comments
of my program any code taken from an external source. *****/


/**** Unary Rules From Class ****/
s(0).
s(s(X)) :- s(X).

is_unary(X) :- s(X).

unary_plus(0,Y,Y) :- is_unary(Y).
unary_plus(s(X),Y,s(Z)) :- unary_plus(X,Y,Z).

unary_times1(0,_Y,0).
unary_times1(s(_X),0,0).
unary_times1(s(X),s(Y),Z) :-
    unary_times1(s(Y),X,PREV_Z),
    unary_plus(s(Y),PREV_Z,Z).

unary_times2(0,_Y,0).
unary_times2(s(_X),0,0).
unary_times2(s(X),s(Y),Z) :-
    unary_plus(s(Y),PREV_Z,Z),
    unary_times2(s(Y),X,PREV_Z).

unary_leq(0,X) :- is_unary(X).
unary_leq(s(X),s(Y)) :- unary_leq(X,Y).


/**** TASK 1 ****/
unary_sqrt(N,K) :-
    unary_leq(K,N),
    unary_times1(K,K,P1),
    unary_leq(P1,N),
    unary_times1(s(K),s(K),P2),
    unary_leq(N,P2).


/**** TASK 2 ****/
unary_divisor(N,K) :-
    unary_times2(R,K,N),
    is_unary(R).


/**** Binary Rules From Class ****/
binary_list([]).
binary_list([X|Xs]) :-
            member(X,[0,1]),
            binary_list(Xs).
binary_list(Xs,N) :-
    length(Xs,N),
    b_list(Xs).

is_binary(X) :- binary_list(X).


/**** TASK 3 ****/
binary_plus([],Y,Y) :- is_binary(Y).
binary_plus([0|Xs],[],[0|Xs]) :- is_binary(Xs).
binary_plus([1|Xs],[],[1|Xs]) :- is_binary(Xs).

binary_plus([0|Xs],[0|Ys],[0|Zs]) :- binary_plus(Xs,Ys,Zs).
binary_plus([0|Xs],[1|Ys],[1|Zs]) :- binary_plus(Xs,Ys,Zs).
binary_plus([1|Xs],[0|Ys],[1|Zs]) :- binary_plus(Xs,Ys,Zs).

binary_plus([1|Xs],[1|Ys],[0|Zs]) :-
    binary_plus(Xs,Ys,W),
    binary_plus(W,[1],Zs).


/**** TASK 4 ****/
binary_times([],Y,[]) :- is_binary(Y).
binary_times([X|Xs],[],[]) :- is_binary([X|Xs]).
binary_times([0|Xs],Y,Z) :-
    binary_times(Xs,Y,PREV_Z),
    binary_plus(PREV_Z,PREV_Z,Z).

binary_times([1|Xs],Y,Z) :-
    binary_times([0|Xs],Y,PREV_Z),
    binary_plus(PREV_Z,Y,Z).


/**** TASK 5 ****/
% divisable(X,Y) :-
%     0 is X mod Y,
%     Y >= 3.

% divisable(X,Y+2) :-
%     divisable(X,Y).

% is_prime(2).
% is_prime(X) :-
%     not(0 is X mod 2),
%     not(divisable(X,round(sqrt(X)))).