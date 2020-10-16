% Victor Matthijs, UGent, April 2020
% ==========================================================
% part of this code is based on the following link:
% https://github.com/perkola/matrix/blob/master/matrix.pl
% 
% The following rules are all used to construct a matrix from the parsed input
% We first construct a matrix containing all zeros, after that we go through all tiles
% and place the correct color on the correcct position.
% ==========================================================

:- module(board, [construct_board_from_tiles/4, did_current_player_win/5, find_top_hulp/8, 
    find_bottom_hulp/8, change_board_for_color/2, place_tile/5,generate_top_positions/4,
    generate_bottom_positions/5, only_hold_color/4, get_element/3]).

% construct_board_from_tiles(+Rows, +Columns, +Tiles, -Board)
% This first rule will be exported in the module board
% it construct a matrix with the tile at the correct position, 
% these positions where specified in the parsing fase.
construct_board_from_tiles(Rows, Columns, Tiles, Board) :-
    make_empty_matrix(Rows, Columns, Board1),
    fill_matrix_with_tiles(Rows, Columns, Tiles, Board1, Board).

% make_empty_matrix(+Rows, +Columns, -Matrix)
% This rule will help to "generate" a matrix with a zero at every position.
make_empty_matrix(0, _, [])         :- !.
make_empty_matrix(R, C, [New|Rest]) :-
    R1 is R - 1,
    generate_row(C, New),
    make_empty_matrix(R1, C, Rest).

% generate_row(+Size, -Row)
% This rule will generate a Row of length Size
% It will generate this row recursivly and place a zero at every element
generate_row(0, [])     :- !.
generate_row(Size, [0|Rest]) :-
    M1 is Size - 1,
    generate_row(M1, Rest).

% fill_matrix_with_tiles(+TileList, +Board, -NewBoard)
% This rule will replace the zero's with colors
% The input is an list of tiles
fill_matrix_with_tiles(_, _, [], Matrix, Matrix).
fill_matrix_with_tiles(Rows, Columns, [tile(C,R,Color)|Rest], Matrix, NewMatrix) :-
    ((C >= 1, C =< Columns), (R >= 1, R =< Rows), !,
    place_tile(Matrix, R, C, Color, HelpMatrix),
    fill_matrix_with_tiles(Rows, Columns, Rest,HelpMatrix, NewMatrix)), !
    ;
    (print_message(error, invalid_Board_Tiles()), halt(2)).


prolog:message(invalid_Board_Tiles()) --> ["You gave a wrong Tile confirguration!"].

% place_tile(+List, +Row, +Column, +Color, -ResultList)
% This rule will place one tile on the correct spot of the matrix
place_tile([R|Rest], 1, Column, Color, [N|Rest]) :-
    replace(R, Column, Color, N), !.
place_tile([R|Rest], Row, Column, Color, [R|Rest1]) :-
    Row1 is Row - 1,
    place_tile(Rest, Row1, Column, Color, Rest1).

% replace(+List, +Index, +Element, -ResultList)
% This rule will replace an element at the correct index of an array
% This array will represent a row in our matrix/Board
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% This follwoing rule was constructed while using the following link
% link: https://stackoverflow.com/questions/5807455/matrix-operations-prolog
% the rule will be used when the current color that is playing, isn't the color on the 
% top and bottom of the board, if we just take the transpose of the board then we 
% can still use the same rules to calculate if a color has won
% change_board_for_color(+HorBoard, -VerBoard)
change_board_for_color(HorBoard, VerBoard) :-
    transpose_of_matrix(HorBoard, VerBoard).

% transpose_of_matrix(+Rows, -Cols)
% get a transpose matrix of a specific board
transpose_of_matrix([[]|_], []) :- !.
transpose_of_matrix([[I|Is]|MatrixRest], [Col|MT]) :-
    first_column([[I|Is]|MatrixRest], Col, [Is|NewMatrixRest]),
    transpose_of_matrix([Is|NewMatrixRest], MT).

% first_column(+Matrix, +List, -FirstCol)
first_column([], [], []).
first_column([[]|_], [], []).
first_column([[I|Is]|MatrixRest], [I|Col], [Is|Rest]) :-
    first_column(MatrixRest, Col, Rest).

% ===============================================================================================
% The code below is written to check if a player has won on a current board. The algorithm will go
% as followed: It will check if there is a path from th new tile to the top of the board for the 
% current (color) player. If that is indeed so, then it will look for a path from the new tile to
% the bottom of the board for the current (color) player. If there is a path to the top and to the 
% bottom then there is a path from the top to the bottom so the current (color) player has won.
% ===============================================================================================

% did_current_player_win(+NewTile, +MaxColumn, +MaxRow, +Board, +CurrentColor)
% This is the start of the algorithm to check if a player has won
did_current_player_win(NewTile, MCol, MRow, Board, Color) :-
    find_top_hulp([NewTile], [], MCol, MRow, Color, Board, _, path),
    find_bottom_hulp([NewTile], [], MCol, MRow, Color, Board, _, path).

% find_top_hulp(+ListOfTiles, +History, +MaxColum, +MaxRow, +CurrentColor, +Board)
% This rule will help to construct a path to the top of the board, it is a
% recursive rule, we use a history so we don't get into a loop.
find_top_hulp([], His, _, _, _, _, His, nopath).
find_top_hulp([NewTile|Rest], His, MCol, MRow, Color, OldBoard, FinalHis, FindTop) :-
    nth1(1, NewTile, SCol),
    nth1(2, NewTile, SRow),
    append([NewTile], His, NewHis),
    (find_path_to_top(SCol, SRow, MCol, MRow, Color, OldBoard, NewHis, FinalHis, FindTop), ! ;
    find_top_hulp(Rest, NewHis, MCol, MRow, Color, OldBoard, FinalHis, FindTop)).

% find_bottom_hulp(+ListOfTiles, +History, +MaxColum, +MaxRow, +CurrentColor, +Board)
% This rule is almost the same, but here we try to construct a path from the current tile
% to the bottom of the board
find_bottom_hulp([], His, _, _, _, _, His, nopath).
find_bottom_hulp([NewTile|Rest], His, MCol, MRow, Color, OldBoard, FinalHis, FindBottom) :-
    nth1(1, NewTile, SCol),
    nth1(2, NewTile, SRow),
    append([NewTile], His, NewHis),
    (find_path_to_bottom(SCol, SRow, MCol, MRow, Color, OldBoard, NewHis,FinalHis, FindBottom), ! ;
    find_bottom_hulp(Rest, NewHis, MCol, MRow, Color, OldBoard, FinalHis, FindBottom)).

% find_path_to_top(+StartColumn, +StartRow, +MaxColumn, +MaxRow, +CurrentColor, +Board, +History)
% This rule will call itself until it reaches the upper most row, then we have a path from the
% start tile to the top. It will fist generate all surrounding tiles (not taking into account the tiles
% beneath itself), it will then filter the list of next tiles, so only the tiles that have same collor
% will stay. Last step is to look in the history so we don't enter a loop in our search. Then we call
% the hulp rule again but now we search for a path closer to the top of the board. Eventually we are 
% on the top row and we then know that we have fromed a path from the new layed tile to the top.
find_path_to_top(_, 1, _, _, _, _, His, His, path) :- !.
find_path_to_top(SCol, SRow, MCol, _, Color, OldBoard, His, FinalHis, FindTop) :-
    generate_top_positions(SCol, SRow, MCol, Sol),
    only_hold_color(Sol, OldBoard, Color, NewSol),
    only_no_history(NewSol, His, NoHisSol),
    find_top_hulp(NoHisSol, His, MCol, _, Color, OldBoard, FinalHis, FindTop), !.

% find_path_to_bottom(+StartColumn, +StartRow, +MaxColumn, +MaxRow, +CurrentColor, +Board, +History)
% This rule does almost the same as the one above, but here we search for a path to the bottom of the
% board, also remembering a histroy so we don't get stuck in a loop.
find_path_to_bottom(_, MRow, _, MRow, _, _, His, His, path) :- !.
find_path_to_bottom(SCol, SRow, MCol, MRow, Color, OldBoard, His, FinalHis, FindBottom) :-
    generate_bottom_positions(SCol, SRow, MCol, MRow, Sol),
    only_hold_color(Sol, OldBoard, Color, NewSol),
    only_no_history(NewSol, His, NoHisSol),
    find_bottom_hulp(NoHisSol, His, MCol, MRow, Color, OldBoard, FinalHis, FindBottom), !.

% ==================================================================================
% The functions below will help to search for all surrounding tiles, this will help to
% search a path in our current board
% ==================================================================================

% generate_top_positions(+StartCol, +StartRow, +MaxColumn, -SolutionTiles)
% This will generate all surrounding tiles that lay above or next to the current tile
% It will generate all the N tiles below
%   |   | N | N |
%   | N | x | N |
%   |   |   |   |
generate_top_positions(SCol, SRow, MCol, Solution) :-
    findall(Position, next_top_position(SCol, SRow, MCol, Position), Solution).

% next_top_position(+Column, +Row, +MaxColumn, -[NewColumn,NewRow])
next_top_position(Col, Row, _,  [Col, NewRow])    :- Row > 1, NewRow is Row - 1.
next_top_position(Col, Row, MC, [NewCol, NewRow]) :- Row > 1, Col < MC, NewCol is Col + 1, NewRow is Row - 1.
next_top_position(Col, Row, MC, [NewCol, Row])    :- Col < MC, NewCol is Col + 1.
next_top_position(Col, Row, _,  [NewCol, Row])    :- Col > 1, NewCol is Col - 1.

% generate_bottom_positions(+StartCol, +StartRow, +MaxColumn, +MaxRow, -SolutionTiles)
% This will generate all surrounding tiles that lay under or next to the current tile
% It will generate all the N tiles below
%   |   |   |   |
%   | N | x | N |
%   | N | N |   |
generate_bottom_positions(SCol, SRow, MCol, MRow, Solution) :-
    findall(Position, next_bottom_position(SCol, SRow, MCol, MRow, Position), Solution).

% next_bottom_position(+Column, +Row, +MaxColumn, +MaxRow, -[NewColumn, NewRow])
next_bottom_position(Col, Row, _, MR, [Col, NewRow])    :- Row < MR, NewRow is Row + 1.
next_bottom_position(Col, Row, _, MR, [NewCol, NewRow]) :- Row < MR, Col > 1, NewCol is Col - 1, NewRow is Row + 1.
next_bottom_position(Col, Row, MC, _, [NewCol, Row]) :- Col < MC, NewCol is Col + 1.
next_bottom_position(Col, Row, _, _, [NewCol, Row])  :- Col > 1, NewCol is Col - 1.

% ==================================================================================
% The following rules are mainly to help the rules above
% ==================================================================================

% only_hold_color(+Tiles, +Board, +Color, -ColorTiles)
% This recursive rule will remove all tiles that aren't the current color
only_hold_color([], _, _, []) :- !.
only_hold_color([Tile|Rest], OldBoard, Color, [Tile|RestSol]) :-
    get_element(OldBoard, Tile, Value),
    Value == Color, !,
    only_hold_color(Rest, OldBoard, Color, RestSol).
only_hold_color([Tile|Rest], OldBoard, Color, RestSol) :-
    get_element(OldBoard, Tile, Value),
    not(Value = Color), !,
    only_hold_color(Rest, OldBoard, Color, RestSol).

% onmy_no_history(+Tiles, +Histroy, -NewTiles)
% This rule will only return tiles that haven't been visited yet
only_no_history([], _, []) :- !.
only_no_history([Tile|Rest], His, [Tile|RestSol]) :-
    not(member(Tile, His)), !,
    only_no_history(Rest, His, RestSol).
only_no_history([Tile|Rest], His, NewRes) :-
    member(Tile, His), !,
    only_no_history(Rest, His, NewRes).

% get_element(+Matrix, +Tile, -Value)
% This simple rule will get a specific value from a matrix/Board
get_element(Mat, Tile, Val) :- 
    nth1(1, Tile, Col),
    nth1(2, Tile, Row),
    nth1(Row, Mat, ARow), 
    nth1(Col, ARow, Val).