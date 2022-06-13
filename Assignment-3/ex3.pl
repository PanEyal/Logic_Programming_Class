
user:file_search_path(sat, './satsolver').
user:file_search_path(bee, './beeCompiler').

:- use_module(sat(satsolver)).
:- use_module(bee(bCompiler)).
:- dynamic listKeepFrom/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% KAKURO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Instance Example: [[14,[I11,I12,I13,I14]],[17,[I3,I4,I5,I6]],[3,[I7,I8]],[4,[I9,I10]],[11,[I3,I7]],[6,[I1,I2]],[8,[I4,I8,I11]],[3,[I1,I5]],[18,[I2,I6,I9,I13]],[3,[I10,I14]]]
% Solution Example: [[14,[5,1,6,2]],[17,[9,2,1,5]],[3,[2,1]],[4,[3,1]],[11,[9,2]],[6,[2,4]],[8,[2,1,5]],[3,[2,1]],[18,[4,5,3,6]],[3,[1,2]]]

% ------------------------------- Verify ------------------------------- %

kakuroVerifyBlockRange([]).
kakuroVerifyBlockRange([N|Rest]) :-
    N >= 1,
    N =< 9,
    kakuroVerifyBlockRange(Rest).

kakuroVerifyElement([ClueSum, Block]) :-
    sum_list(Block, Sum),
    ClueSum =:= Sum,
    kakuroVerifyBlockRange(Block).

kakuroVerify([], []) :-
    writeln(verify:ok).

kakuroVerify([Element|Rest1], [Element|Rest2]) :-
    (kakuroVerifyElement(Element) -> kakuroVerify(Rest1, Rest2); writeln(verify:wrong)).

    
