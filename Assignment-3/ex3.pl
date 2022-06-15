
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SUDUKO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Instance Example: Killer([cell(5,3) = 1, cell(6,7) = 2, cell(4,5) = 6, cell(7,9) = 8])
% Solution 5 Example: [cell(1,1)=1,cell(1,2)=2,cell(1,3)=3,cell(1,4)=4,cell(1,5)=5,cell(2,1)=1,cell(2,2)=2,cell(2,3)=3,cell(2,4)=4,cell(2,5)=5,cell(3,1)=1,cell(3,2)=2,cell(3,3)=3,cell(3,4)=4,cell(3,5)=5,cell(4,1)=1,cell(4,2)=2,cell(4,3)=3,cell(4,4)=4,cell(4,5)=5,cell(5,1)=1,cell(5,2)=2,cell(5,3)=3,cell(5,4)=4,cell(5,5)=5]

% ------------------------------- Verify ------------------------------- %

% Make sure that board size is 9*9 = 81 and holds all cells
verify_killer_size(Solution) :-
    length(Solution, 81),
    forall((between(1,9,I), between(1,9,J)), member(cell(I,J)=_K, Solution)).

% I will be using the same function unique/2 from task 1

% Make sure that each row, column and box consist of all-different values (between 1 and 9).
verify_unique_row(I, Solution) :-
    findall(K, member(cell(I,_J)=K, Solution), Row),
    forall(member(K, Row), unique(K, Row)).

verify_unique_column(J, Solution) :-
    findall(K, member(cell(_I,J)=K, Solution), Column),
    forall(member(K, Column), unique(K, Column)).

verify_killer_line(Solution) :-
    forall(between(1,9,I), verify_unique_row(I, Solution)),
    forall(between(1,9,J), verify_unique_column(J, Solution)).

% Make sure that any two cells separated by a Knight’s move (the chess-piece) must be different.
knight_move(I, J, I+1, J+2).
knight_move(I, J, I+1, J-2).
knight_move(I, J, I-1, J+2).
knight_move(I, J, I-1, J-2).
knight_move(I, J, I+2, J+1).
knight_move(I, J, I+2, J-1).
knight_move(I, J, I-2, J+1).
knight_move(I, J, I-2, J-1).

verify_unique_knight(I, J, Solution) :-
    findall(K, (knight_move(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K, Solution)), Knight_Moves),
    forall(member(K, Knight_Moves), unique(K, Knight_Moves)). 

verify_killer_knight(Solution) :-
    forall((between(1,9,I),between(1,9,J)), verify_unique_knight(I, J, Solution)).

% Make sure that any two cells separated by a King’s move (the chess-piece) must be different.
verify_killer_king(Solution).

% Make sure that the values of any two cells which appear next to each other in a row, or column
% must have absolute difference of at least 2.
verify_killer_2_gap(Solution).


% Make sure the solution holds all the constraints
verify_killer_solution(Solution) :-
    verify_killer_size(Solution),
    verify_killer_line(Solution),
    verify_killer_knight(Solution),
    verify_killer_king(Solution),
    verify_killer_2_diff(Solution).

% Verificating!
verify_killer([], []).
verify_killer(killer(Instance), Solution) :-
    % Make sure that original cells are stil there
    forall(member(Cell, Instance), member(Cell, Solution)),
    verify_killer_solution(Solution).