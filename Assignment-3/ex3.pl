
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

% Make sure that board size is 9*9 = 81 and holds all cells (between 1 and 9)
verify_killer_size(Solution) :-
    length(Solution, 81),
    forall((between(1,9,I), between(1,9,J)), member(cell(I,J)=_K, Solution)).

% Make sure that each row and column consist of all-different values.
% I will be using the same function unique/2 from task 1
verify_unique_row(I, Solution) :-
    findall(K, member(cell(I,_J)=K, Solution), Row),
    forall(member(K, Row), unique(K, Row)).

verify_unique_column(J, Solution) :-
    findall(K, member(cell(_I,J)=K, Solution), Column),
    forall(member(K, Column), unique(K, Column)).

verify_killer_line(Solution) :-
    forall(between(1,9,I), verify_unique_row(I, Solution)),
    forall(between(1,9,J), verify_unique_column(J, Solution)).

% Make sure that each box consist of all-different values.
% box/4 function takes (I,J) as Left Upper corner of the box,
% and return true if (NewI,NewJ) is in the same Box
box(I, J, I, J).
box(I, J, I, NewJ) :- NewJ is J + 1.
box(I, J, I, NewJ) :- NewJ is J + 2.
box(I, J, NewI, J) :- NewI is I + 1.
box(I, J, NewI, NewJ) :- NewI is I + 1, NewJ is J + 1.
box(I, J, NewI, NewJ) :- NewI is I + 1, NewJ is J + 2.
box(I, J, NewI, J) :- NewI is I + 2.
box(I, J, NewI, NewJ) :- NewI is I + 2, NewJ is J + 1.
box(I, J, NewI, NewJ) :- NewI is I + 2, NewJ is J + 2.

verify_unique_box(I, J, Solution) :-
    % Find all box cell's K values for current (I,J)
    findall(K, (box(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K, Solution)), Box),
    forall(member(K, Box), unique(K, Box)).

verify_killer_box(Solution) :-
    forall((member(I,[1,4,7]),member(J,[1,4,7])), verify_unique_box(I, J, Solution)).

% Make sure that any two cells separated by a Knight’s move (the chess-piece) must be different.
knight_move(I, J, NewI, NewJ) :- NewI is I + 1, NewJ is J + 2.
knight_move(I, J, NewI, NewJ) :- NewI is I + 1, NewJ is J - 2.
knight_move(I, J, NewI, NewJ) :- NewI is I - 1, NewJ is J + 2.
knight_move(I, J, NewI, NewJ) :- NewI is I - 1, NewJ is J - 2.
knight_move(I, J, NewI, NewJ) :- NewI is I + 2, NewJ is J + 1.
knight_move(I, J, NewI, NewJ) :- NewI is I + 2, NewJ is J - 1.
knight_move(I, J, NewI, NewJ) :- NewI is I - 2, NewJ is J + 1.
knight_move(I, J, NewI, NewJ) :- NewI is I - 2, NewJ is J - 1.

verify_unique_knight(I, J, Solution) :-
    % Find all knight moves cell's K values for current (I,J)
    findall(K, (knight_move(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K, Solution)), Knight_Moves),
    % Get relevent K value for Current I,J
    member(cell(I,J)=Curr_K, Solution),
    % Make sure they are all different from K
    forall(member(N, Knight_Moves), Curr_K =\= N). 

verify_killer_knight(Solution) :-
    forall((between(1,9,I),between(1,9,J)), verify_unique_knight(I, J, Solution)).

% Make sure that any two cells separated by a King’s move (the chess-piece) must be different.

king_move(I, J, NewI, NewJ) :- NewI is I + 1, NewJ is J + 1.
king_move(I, J, NewI, NewJ) :- NewI is I + 1, NewJ is J - 1.
king_move(I, J, NewI, NewJ) :- NewI is I - 1, NewJ is J + 1.
king_move(I, J, NewI, NewJ) :- NewI is I - 1, NewJ is J - 1.
king_move(I, J, NewI, J) :- NewI is I + 1.
king_move(I, J, NewI, J) :- NewI is I - 1.
king_move(I, J, I, NewJ) :- NewJ is J + 1.
king_move(I, J, I, NewJ) :- NewJ is J - 1.

verify_unique_king(I, J, Solution) :-
    % Find all king moves cell's K values for current (I,J)
    findall(K, (king_move(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K, Solution)), King_Moves),
    % Get relevent K value for Current I,J
    member(cell(I,J)=Curr_K, Solution),
    % Make sure they are all different from K
    forall(member(N, King_Moves), Curr_K =\= N). 

verify_killer_king(Solution) :-
    forall((between(1,9,I),between(1,9,J)), verify_unique_king(I, J, Solution)).

% Make sure that the values of any two cells which appear next to each other in a row, or column
% must have absolute difference of at least 2.

neighbor_move(I, J, NewI, NewJ) :- NewI is I + 1, NewJ is J + 1.
neighbor_move(I, J, NewI, NewJ) :- NewI is I + 1, NewJ is J - 1.
neighbor_move(I, J, NewI, NewJ) :- NewI is I - 1, NewJ is J + 1.
neighbor_move(I, J, NewI, NewJ) :- NewI is I - 1, NewJ is J - 1.

verify_unique_neighbor(I, J, Solution) :-
    % Find all Neighbors cell's K values for current (I,J)
    findall(K, (neighbor_move(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K, Solution)), Neighbor_Moves),
    % Get relevent K value for Current I,J
    member(cell(I,J)=Curr_K, Solution),
    % Make sure |K-N|<=2 for any neighbor N
    forall(member(N, Neighbor_Moves), abs(-(Curr_K, N)) >= 2).

verify_killer_neighbor(Solution) :-
    forall((between(1,9,I),between(1,9,J)), verify_unique_neighbor(I, J, Solution)).

% Make sure the solution holds all the constraints
verify_killer_solution(Solution) :-
    verify_killer_size(Solution),
    verify_killer_line(Solution),
    verify_killer_box(Solution),
    verify_killer_knight(Solution),
    verify_killer_king(Solution),
    verify_killer_neighbor(Solution).

% Verificating!
verify_killer([], []).
verify_killer(killer(Instance), Solution) :-
    % Make sure that original cells are stil there
    forall(member(Cell, Instance), member(Cell, Solution)),
    % Verify soution correctness
    verify_killer_solution(Solution).

% ------------------------------- Encode ------------------------------- %

encode_killer_Declare_Ints(Instance, Map, Constrains) :-
    % Make a list of constraint for all variables K that are in Instance (Size in declaration is known)
    findall(new_int(K,Ins_K,Ins_K), (member(cell(I,J)=Ins_K, Instance), member(cell(I,J)=K, Map)), Cs2),
    % Make a list of constraint for all variables K that are not in Instance (Size in declaration between 1 and 9)
    findall(new_int(K,1,9), (not(member(cell(I,J)=_K, Instance)), member(cell(I,J)=K, Map)), Cs1),
    append(Cs1, Cs2, Constrains).

encode_unique_row(I, Map, int_array_allDiff(Row)) :-
    findall(K, member(cell(I,_J)=K, Map), Row).

encode_unique_column(J, Map, int_array_allDiff(Column)) :-
    findall(K, member(cell(_I,J)=K, Map), Column).

encode_killer_line(Map, Constraints) :-
    findall(Row_C, (between(1,9,I), encode_unique_row(I, Map, Row_C)), Row_Cs),
    findall(Column_C, (between(1,9,J), encode_unique_column(J, Map, Column_C)), Column_Cs),
    append(Row_Cs, Column_Cs, Constraints).

encode_unique_box(I, J, Map, int_array_allDiff(Box)) :-
    findall(K, (box(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K, Map)), Box).

encode_killer_box(Map, Constrains) :-
    findall(Box_C, (member(I,[1,4,7]), member(J,[1,4,7]), encode_unique_box(I, J, Map, Box_C)), Constrains).

encode_unique_knight(I, J, Map, Constrains) :-
    % Find all knight moves cell's K values for current (I,J)
    findall(K, (knight_move(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K, Map)), Knight_Moves),
    % Get relevent K for Current I,J
    member(cell(I,J)=Curr_K, Map),
    % Make sure they are all different from K
    findall(int_neq(Curr_K,K), member(K, Knight_Moves), Constrains). 

encode_killer_knight(Map, Constraints) :-
    findall(Knight_Move_C, (between(1,9,I), between(1,9,J), encode_unique_knight(I, J, Map, Knight_Move_C)), Knight_Move_Cs),
    append(Knight_Move_Cs, Constrains).

encode_unique_king(I, J, Map, Constrains) :-
    % Find all king moves cell's K values for current (I,J)
    findall(K, (King_move(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K, Map)), King_Moves),
    % Get relevent K for Current I,J
    member(cell(I,J)=Curr_K, Map),
    % Make sure they are all different from K
    findall(int_neq(Curr_K,K), member(K, King_Moves), Constrains). 

encode_killer_King(Map, Constraints) :-
    findall(King_Move_C, (between(1,9,I), between(1,9,J), encode_unique_king(I, J, Map, King_Move_C)), King_Move_Cs),
    append(King_Move_Cs, Constrains).

encode_unique_neighbor(I, J, Map, Constrains) :-
    % Find all neighbor moves cell's K values for current (I,J)
    findall(K1, (Neighbor_move(I, J, NewI, NewJ), member(cell(NewI,NewJ)=K1, Map)), Neighbor_Moves),
    % Get relevent K for Current I,J
    member(cell(I,J)=Curr_K, Map),

    % Make sure |K-N|<=2 for any neighbor N by getting Min and Max for evey comparison between Curr_K and K in Neighbor_Moves,
    % then add the number 2 to the Min, and make sure it is leq than Max
    findall([int_max(Curr_K,K2,MaxK), int_min(Curr_K,K2,MinK), int_plus(MinK,Two,TempK), int_leq(TempK,MaxK)], member(K2, Neighbor_Moves), Max_Min_Cs).
    append([new_int(Two,2,2)|Max_Min_Cs], Constrains).

encode_killer_neighbor(Map, Constraints) :-
    findall(Neighbor_Move_C, (between(1,9,I), between(1,9,J), encode_unique_neighbor(I, J, Map, Neighbor_Move_C)), Neighbor_Move_Cs),
    append(Neighbor_Move_Cs, Constrains).
    
% Encoding!
encode_killer(Instance, Map, Constraints) :-
    % Build Map as a list with int variables K at each coordinate (I,J) of the board
    findall(cell(I,J)=K, (between(1,9,I),between(1,9,J)), Map),

    % Declare each number K
    encode_killer_Declare_Ints(Instance, Map, Cs1),

    % Constraints for each row, column.
    encode_killer_line(Map, Cs2),
    % Constraints for each box.
    encode_killer_box(Map, Cs3),
    % Constraints for each knight move.
    encode_killer_knight(Map, Cs4),
    % Constraints for each king move.
    encode_killer_king(Map, Cs5),
    % Constraints for each neighbor.
    encode_killer_neighbor(Map, Cs6),
    
    append([Cs1,Cs2,Cs3,Cs4,Cs5,Cs6], Constraints).

% ------------------------------- Decode ------------------------------- %

% The map is in the solution format, just decode each cell
% Decoding!
decode_killer(Map, Solution):-
    findall(cell(I,J)=DecodedK, (decodeInt(K,DecodedBlock), member(cell(I,J)=K, Map)), Solution).

% -------------------------------- Solve -------------------------------- %

% Solving!
solve_killer(Instance, Solution) :-
    runExpr(Instance, Map, encode_killer, decode_killer, verify_killer).