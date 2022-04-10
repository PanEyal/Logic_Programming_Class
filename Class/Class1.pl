prefix(P,Xs) :- append(P,_,Xs).
suffix(S,Xs) :- append(_,S,Xs).
sublist(Ys,Xs) :- prefix([|],Xs),suffix(Ys,P).
