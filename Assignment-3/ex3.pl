
user:file_search_path(beeCompiler, './beeCompiler').
user:file_search_path(aux, './bApplications/auxs').

:- use_module(beeCompiler(bCompiler)).
:- use_module(aux(auxRunExpr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% KAKURO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Instance Example: [14=[I11,I12,I13,I14],17=[I3,I4,I5,I6],3=[I7,I8],4=[I9,I10],11=[I3,I7],6=[I1,I2],8=[I4,I8,I11],3=[I1,I5],18=[I2,I6,I9,I13],3=[I10,I14]]
% Solution Example: [14=[5,1,6,2],17=[9,2,1,5],3=[2,1],4=[3,1],11=[9,2],6=[2,4],8=[2,1,5],3=[2,1],18=[4,5,3,6],3=[1,2]]

% ------------------------------- Verify ------------------------------- %

kakuroVerifyBlockRange([]).
kakuroVerifyBlockRange([N|Rest]) :-
    N >= 1,
    N =< 9,
    kakuroVerifyBlockRange(Rest).

kakuroVerifyElement((ClueSum=Block)) :-
    sum_list(Block, Sum),
    ClueSum =:= Sum,
    kakuroVerifyBlockRange(Block).

kakuroVerify([], []) :-
    writeln(verify:ok).

kakuroVerify([Element|Rest1], [Element|Rest2]) :-
    (kakuroVerifyElement(Element) -> kakuroVerify(Rest1, Rest2); writeln(verify:wrong)).

% ------------------------------- Encode ------------------------------- %

kakuroDeclareBlockInts([], []).
kakuroDeclareBlockInts([N|Rest], Constraints) :-
    kakuroDeclareBlockInts(Rest, Cs),
    (not(var(N)) -> append([[new_int(N, 1, 9)], Cs], Constraints); Cs = Constraints),!.

kakuroEncode([], []).
kakuroEncode([(ClueSum=Block)|Rest], Constraints) :-
    Cs1 = [new_int(ClueSumDeclared, ClueSum, ClueSum), bool_array_sum_eq(Block, ClueSumDeclared)],
    kakuroDeclareBlockInts(Block, Cs2),
    kakuroEncode(Rest, Cs3),
    append([Cs1, Cs2, Cs3], Constraints),!.

kakuroEncode(Instance, Instance, Constraints) :-
    kakuroEncode(Instance, Constraints),!.

% ------------------------------- Decode ------------------------------- %

kakuroDecode([], []).
kakuroDecode([(ClueSum=Block1)|Rest1],[(ClueSum=Block2)|Rest2]):-
    % use BEE auxiliary module to decode integers
    bDecode:decodeIntArray(Block1, Block2),
    kakuroDecode(Rest1, Rest2).

% -------------------------------- Solve -------------------------------- %

kakuroSolve(Instance,Solution):-
    runExpr(Instance, Solution, kakuroEncode, kakuroDecode, kakuroVerify).
