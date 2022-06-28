/**** I, Pan Eyal (208722058) assert that the work I submitted is entirely my
own. I have not received any part from any other student in the class (or other
source), nor did I give parts of it for use to others. I have clearly marked in
the comments of my program any code taken from an external source. *****/

user:file_search_path(aux, './bApplications/auxs').

:- use_module(aux(auxRunExpr)).
:- use_module(aux(auxRunExprAll), except([decodeIntMatrix/2, decodeIntArray/2, printHeader/0, decodeInt/2])).

% Note to myslf, To show full solution on prolog run the comand: set_prolog_flag(answer_write_options,[max_depth(0)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% KAKURO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Kakuro Instance Example: [14=[I11,I12,I13,I14],17=[I3,I4,I5,I6],3=[I7,I8],4=[I9,I10],11=[I3,I7],6=[I1,I2],8=[I4,I8,I11],3=[I1,I5],18=[I2,I6,I9,I13],3=[I10,I14]]
% Kakuro Solution Example: [14=[5,1,6,2],17=[9,2,1,5],3=[2,1],4=[3,1],11=[9,2],6=[2,4],8=[2,1,5],3=[2,1],18=[4,5,3,6],3=[1,2]]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SUDUKO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
% box(+,+,-,-) function takes cell(I,J) as Left Upper corner of the box,
% and return true if cell(NewI,NewJ) is in the same Box
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

neighbor_move(I, J, I, NewJ) :- NewJ is J + 1.
neighbor_move(I, J, I, NewJ) :- NewJ is J - 1.
neighbor_move(I, J, NewI, J) :- NewI is I + 1.
neighbor_move(I, J, NewI, J) :- NewI is I - 1.

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

% Make a list of constraint for all variables K, If the cell is a member of Instance,
% Make a special declaration (Size is K as in the Instance), else regular (Size is between 1 and 9).
encode_killer_Declare_Ints(_Instance, [], []).
encode_killer_Declare_Ints(Instance, [cell(I,J)=MK|MRest], [Int_Dec|Constrains]) :-
    (   % If
        member(cell(I,J)=K, Instance)
    ->  % Then
        Int_Dec = new_int(MK,K,K)
    ;   % Else
        Int_Dec = new_int(MK,1,9)
    ),
    encode_killer_Declare_Ints(Instance, MRest, Constrains).

% Builds row I
encode_row(_ITarget, [], []).
encode_row(ITarget, [cell(I,_J)=MK|MRest], Row) :-
    (   % If
        ITarget =:= I
    ->  % Then
        Row = [MK|RPrev]
    ;   % Else
        Row = RPrev
    ),
    encode_row(ITarget, MRest, RPrev).

% Create constraints for each row
encode_unique_row(I, _Map, []) :-
    I < 1.
encode_unique_row(I, Map, [int_array_allDiff(Row)|Constraints]) :-
    encode_row(I, Map, Row),
    NextI is I-1,
    encode_unique_row(NextI, Map, Constraints).

% Builds row J
encode_column(_JTarget, [], []).
encode_column(JTarget, [cell(_I,J)=MK|MRest], Column) :-
    (   % If
        JTarget =:= J
    ->  % Then
        Column = [MK|CPrev]
    ;   % Else
        Column = CPrev),
    encode_column(JTarget, MRest, CPrev).

% Create constraints for each column
encode_unique_column(J, _Map, []) :-
    J < 1.
encode_unique_column(J, Map, [int_array_allDiff(Column)|Constraints]) :-
    encode_column(J, Map, Column),
    NextJ is J-1,
    encode_unique_column(NextJ, Map, Constraints).

% Create constraints for each box.
% Each ITarget and JTarget mod 9 must be equal to 1. (Left upper cell of each box)
encode_box(_ITarget, _JTarget, [], []).
encode_box(ITarget, JTarget, [cell(I,J)=MK|MRest], Box) :-
    (   % If
        box(ITarget, JTarget, I, J)
    ->  % Then
        Box = [MK|BPrev]
    ;   % Else
        Box = BPrev),
    encode_box(ITarget, JTarget, MRest, BPrev).

% Create constraints for each knight move from cell(ITarget, JTarget).
encode_knight(_ITarget, _JTarget, _Map, [], []).
encode_knight(ITarget, JTarget, Map, [cell(I,J)=MK|MRest], Knight_Cs) :-
    (   % If
        knight_move(ITarget, JTarget, I, J)
    ->  % Then
        member(cell(ITarget, JTarget)=MKTarget, Map), Knight_Cs = [int_neq(MKTarget, MK)|KPrev_Cs]
    ;   % Else
        Knight_Cs = KPrev_Cs
    ),
    encode_knight(ITarget, JTarget, Map, MRest, KPrev_Cs).

% Create constraints for each king move from cell(ITarget, JTarget).
encode_king(_ITarget, _JTarget, _Map, [], []).
encode_king(ITarget, JTarget, Map, [cell(I,J)=MK|MRest], King_Cs) :-
    (   % If
        king_move(ITarget, JTarget, I, J)
    ->  % Then
        member(cell(ITarget, JTarget)=MKTarget, Map), King_Cs = [int_neq(MKTarget, MK)|KPrev_Cs]
    ;   % Else
        King_Cs = KPrev_Cs),
    encode_king(ITarget, JTarget, Map, MRest, KPrev_Cs).

% Create constraints for each neighbor move from cell(ITarget, JTarget).
encode_neighbor(_ITarget, _JTarget, _Map, [], _Two, []).
encode_neighbor(ITarget, JTarget, Map, [cell(I,J)=MK|MRest], Two, Neighbor_Cs) :-
    (   % If
        neighbor_move(ITarget, JTarget, I, J)
    ->  % Then
        member(cell(ITarget, JTarget)=MKTarget, Map),
        Neighbor_Cs = [new_int(SubK,-8,8), int_plus(SubK,MK,MKTarget), new_int(AbsK,0,8), int_abs(SubK, AbsK), int_leq(Two, AbsK)|NPrev_Cs]
    ;   % Else
        Neighbor_Cs = NPrev_Cs
    ),
    encode_neighbor(ITarget, JTarget, Map, MRest, Two, NPrev_Cs).

% Scan all J values and create the relevant constraints for cell(I, J).
encode_killer_Per_J(_I, J, _Map, _Two, []) :-
    J < 1.
encode_killer_Per_J(I, J, Map, Two, Constraints) :-
    % Box Constraints
    (   % If
        (I mod 3 =:= 1, J mod 3 =:= 1)
    ->  % Then
        encode_box(I, J, Map, Box),
        Box_Cs = [int_array_allDiff(Box)]
    ;   % Else
        Box_Cs = []
    ),
    % Knight Constraints
    encode_knight(I, J, Map, Map, Knight_Cs),
    % King Constraints
    encode_king(I, J, Map, Map, King_Cs),
    % Neighbor Constraints
    encode_neighbor(I, J, Map, Map, Two, Neighbor_Cs),
    NextJ is J-1,
    % Continue for next J
    encode_killer_Per_J(I, NextJ, Map, Two, Rest_Cs),
    append([Box_Cs, Knight_Cs, King_Cs, Neighbor_Cs, Rest_Cs], Constraints).

% Scan all I (Start from 9 and decrement until smaller then 1),
% and for each I value scan all J values in the same manner.
encode_killer_Per_I(I, _Map, _Two, []) :-
    I < 1.
encode_killer_Per_I(I, Map, Two, Constraints) :-
    encode_killer_Per_J(I, 9, Map, Two, Cs1),
    NextI is I-1,
    encode_killer_Per_I(NextI, Map, Two, Cs2),
    append([Cs1, Cs2], Constraints).

% Encoding!
encode_killer(killer(Instance), Map, Constraints) :-
    % Build Map as a list with int variables K at each coordinate (I,J) of the board
    findall(cell(I,J)=_K, (between(1,9,I),between(1,9,J)), Map),
    
    % Declare each number K in Map
    encode_killer_Declare_Ints(Instance, Map, Cs1),

    % Encode each row constraints in Map
    encode_unique_row(9, Map, Cs2),
    % Encode each column constraints in Map
    encode_unique_column(9, Map, Cs3),

    % Declare on int with valuee two for the last constraint. (Only 1 declaration is needed,
    % so I pass it from here to each constraints of two relevant cells).
    Cs4 = [new_int(Two,2,2)],
    % Encode constraints for each cell in Map
    encode_killer_Per_I(9, Map, Two, Cs5),

    % Append them together
    append([Cs1, Cs2, Cs3, Cs4, Cs5], Constraints).

% ------------------------------- Decode ------------------------------- %

% The map is in the solution format, just decode each cell
% Decoding!
decode_killer(Map, Solution):-
    findall(cell(I,J)=DecodedK,
            (between(1,9,I), between(1,9,J), member(cell(I,J)=K, Map), decodeInt(K,DecodedK)),
            Solution).

% -------------------------------- Solve -------------------------------- %

all_ints([], []).
all_ints([cell(_I,_J)=MK|MRest], [MK|IRest]) :-
    all_ints(MRest, IRest).

% New encoder for all solutions.
encode_all_killer(killer(Instance), Map, ([], Ints), Constraints) :-
    encode_killer(killer(Instance), Map, Constraints),
    all_ints(Map, Ints).
    
% Solving!
all_killer(Instance, Solution, A) :-
    runExprAll(Instance, Solution, encode_all_killer, decode_killer, verify_killer),
    length(Solution, A).

% Sudoku Instance Example: killer([cell(5,3) = 1, cell(6,7) = 2, cell(4,5) = 6, cell(7,9) = 8])
% Sudoku Solution Example: [cell(1,1)=4,cell(1,2)=8,cell(1,3)=3,cell(1,4)=7,cell(1,5)=2,cell(1,6)=6,cell(1,7)=1,cell(1,8)=5,cell(1,9)=9,cell(2,1)=7,cell(2,2)=2,cell(2,3)=6,cell(2,4)=1,cell(2,5)=5,cell(2,6)=9,cell(2,7)=4,cell(2,8)=8,cell(2,9)=3,cell(3,1)=1,cell(3,2)=5,cell(3,3)=9,cell(3,4)=4,cell(3,5)=8,cell(3,6)=3,cell(3,7)=7,cell(3,8)=2,cell(3,9)=6,cell(4,1)=8,cell(4,2)=3,cell(4,3)=7,cell(4,4)=2,cell(4,5)=6,cell(4,6)=1,cell(4,7)=5,cell(4,8)=9,cell(4,9)=4,cell(5,1)=2,cell(5,2)=6,cell(5,3)=1,cell(5,4)=5,cell(5,5)=9,cell(5,6)=4,cell(5,7)=8,cell(5,8)=3,cell(5,9)=7,cell(6,1)=5,cell(6,2)=9,cell(6,3)=4,cell(6,4)=8,cell(6,5)=3,cell(6,6)=7,cell(6,7)=2,cell(6,8)=6,cell(6,9)=1,cell(7,1)=3,cell(7,2)=7,cell(7,3)=2,cell(7,4)=6,cell(7,5)=1,cell(7,6)=5,cell(7,7)=9,cell(7,8)=4,cell(7,9)=8,cell(8,1)=6,cell(8,2)=1,cell(8,3)=5,cell(8,4)=9,cell(8,5)=4,cell(8,6)=8,cell(8,7)=3,cell(8,8)=7,cell(8,9)=2,cell(9,1)=9,cell(9,2)=4,cell(9,3)=8,cell(9,4)=3,cell(9,5)=7,cell(9,6)=2,cell(9,7)=6,cell(9,8)=1,cell(9,9)=5]