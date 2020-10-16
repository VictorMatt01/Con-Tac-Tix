:- use_module(src/game).
:- begin_tests(game).

% Test for the rule get_rows_and_columns
% test for a square board
test(rows_columns_game_test_1) :-
    Game = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(0,[])),
    get_rows_and_columns(Game, Rows, Columns),
    Rows =:= 3,
    Columns =:= 3.

% Test for the rule get_rows_and_columns
% test for a non square board
test(rows_columns_game_test_2) :-
    Game = game(size(8,5),turn(red),state(undecided),orientation(red,blue),tiles(0,[])),
    get_rows_and_columns(Game, Rows, Columns),
    Rows =:= 5,
    Columns =:= 8.

% Test for the rule get_tiles
% a correct test case
test(get_tiles_test_1) :-
    Game = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    get_tiles(Game, Count, TilesList),
    Count =:= 4,
    length(TilesList, 4).

% A test that must fail with the rule get_tiles_test
test(get_tiles_test_2, [fail]) :-
    Game = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(5,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    get_tiles(Game, Count, TilesList),
    Count =:= 4,
    length(TilesList, 4).
% small test to look if the tile_info works
test(get_tile_info_test_1) :-
    Tile = tile(2,2,red),
    get_tile_info(Tile, Col, Row, Color),
    Col =:= 2,
    Row =:= 2,
    Color = red.
test(get_tile_info_test_2, [fail]) :-
    Tile = tile(2,3,blue),
    get_tile_info(Tile, Col, Row, Color),
    Col =:= 2,
    Row =:= 2,
    Color = red.
% checking the rule to get the state of a game
test(get_state_test_1) :-
    Game = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    get_state(Game, State),
    State = undecided,
    Game2 = game(size(3,3),turn(red),state(red),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    get_state(Game2, State2),
    State2 = red.

test(get_next_color_test_1) :-
    Game = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    next_player_color(Game, red, NextColor),
    NextColor = blue.

test(set_new_color_test_1) :-
    Game = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    get_color(Game, Color),
    set_new_color_game(Game, Color, NewGame),
    NewGame = game(size(3,3),turn(blue),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])).

test(check_if_player_won_test_1) :-
    OldGame = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    OldBoard = [[red, 0, 0], [red, blue, blue], [0, 0, 0]],
    NewTile = [1,3],
    check_if_player_won(OldGame, OldBoard, NewTile).
% Test the rule check if player won, but give a tile that won't succeed
test(check_if_player_won_test_2, [fail]) :-
    OldGame = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    OldBoard = [[red, 0, 0], [red, blue, blue], [0, 0, 0]],
    NewTile = [3,1],
    check_if_player_won(OldGame, OldBoard, NewTile).

test(diff_of_boards_test_1) :-
    ListBoard1 = [red, 0, 0, red, blue, blue, 0, 0, 0],
    ListBoard2 = [red, 0, 0, red, blue, blue, red, 0, 0],
    diff_of_boards(ListBoard1, ListBoard2, Position),
    Position =:= 7.
test(diff_of_boards_test_2, [fail]) :-
    ListBoard1 = [red, 0, 0, red, blue, blue, 0, 0, 0],
    ListBoard2 = [red, 0, 0, red, blue, blue, red, 0, 0],
    diff_of_boards(ListBoard1, ListBoard2, Position),
    Position =:= 3.
% some small tests for the get_coor rule
test(get_coor_test_1) :-
    get_coor(7, 3, RowNumber, ColumnNumber),
    RowNumber =:= 3,
    ColumnNumber =:= 1.
test(get_coor_test_2) :-
    get_coor(5, 3, RowNumber, ColumnNumber),
    RowNumber =:= 2,
    ColumnNumber =:= 2.
test(get_coor_test_3) :-
    get_coor(12, 5, RowNumber, ColumnNumber),
    RowNumber =:= 3,
    ColumnNumber =:= 2.
test(get_coor_test_4, [fail]) :-
    get_coor(7, 10, RowNumber, ColumnNumber),
    RowNumber =:= 3,
    ColumnNumber =:= 0.

:- end_tests(game).