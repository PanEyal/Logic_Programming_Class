
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

% Making sure numbers are between 1 to 9
kakuroVerifyBlockRange(Block) :-
    forall(member(I, Block), I >= 1),
    forall(member(I, Block), I =< 9).

% Making sure numbers are unique
unique(I, Block) :-
    selectchk(I, Block, Rest),
    not(member(I, Rest)).

kakuroVerifyBlockUnique(Block) :-
    forall(member(I, Block), unique(I, Block)).

% Making sure numbers sum
kakuroVerifyBlockSum(ClueSum, Block) :-
    sum_list(Block, Sum),
    ClueSum =:= Sum.

% Verification for each element
kakuroVerifyElement((ClueSum=InsBlock),(ClueSum=SolBlock)) :-
    length(InsBlock, Len),
    length(SolBlock, Len),
    kakuroVerifyBlockRange(SolBlock),
    kakuroVerifyBlockUnique(SolBlock),
    kakuroVerifyBlockSum(ClueSum, SolBlock).

% Verificating!
kakuroVerify([], []).
kakuroVerify([InsElement|InsRest], [SolElement|SolRest]) :-
    kakuroVerifyElement(InsElement, SolElement),
    kakuroVerify(InsRest, SolRest).

% ------------------------------- Encode ------------------------------- %

% Get all Variables to declare on
kakuroGetVars([], []).
kakuroGetVars([(_ClueSum=Block)|Rest], Vars) :-
    kakuroGetVars(Rest, VRest),
    term_variables([Block, VRest], Vars),!.

% declare on all Variables
kakuroDeclareInts([], []).
kakuroDeclareInts([I|Rest], [new_int(I, 1, 9)|Constraints]) :-
    kakuroDeclareInts(Rest, Constraints),!.

% Instance is the map itself, need only 2 argumetns
kakuroEncode([], []).
kakuroEncode([(ClueSum=Block)|Rest], Constraints) :-
    % Block Sum Constrains
    Cs1 = [new_int(MClueSum, ClueSum, ClueSum), int_array_sum_eq(Block, MClueSum)],
    % Unique Block Constrains
    Cs2 = [int_array_allDiff(Block)],
    % Next Iteration
    kakuroEncode(Rest, Cs3),
    append([Cs1, Cs2, Cs3], Constraints),!.

% Encoding!
kakuroEncode(Instance, Instance, Constraints) :-
    kakuroGetVars(Instance, Vars),
    kakuroDeclareInts(Vars, Cs1),
    kakuroEncode(Instance, Cs2),
    append([Cs1, Cs2], Constraints),!.

% ------------------------------- Decode ------------------------------- %

% The map is in the solution format, just decode each Block
% Decoding!
kakuroDecode([], []).
kakuroDecode([(ClueSum=Block)|Rest],[(ClueSum=DecodedBlock)|DecodedRest]):-
    decodeIntArray(Block, DecodedBlock),
    kakuroDecode(Rest, DecodedRest).

% -------------------------------- Solve -------------------------------- %

% Solving!
kakuroSolve(Instance,Solution):-
    runExpr(Instance, Solution, kakuroEncode, kakuroDecode, kakuroVerify).
