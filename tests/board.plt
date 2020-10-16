:- use_module(src/board).
:- begin_tests(board).

% test to construct a board from a list of tiles
test(construct_board_from_tiles_test_1) :-
    Tiles = [tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)],
    construct_board_from_tiles(3,3, Tiles, Board),
    Board = [[red, 0, 0], [red, blue, blue], [0, 0, 0]].
test(construct_board_from_tiles_test_2) :-
    Tiles = [tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue), tile(5,5,red), tile(1,5,red), tile(4,4,blue)],
    construct_board_from_tiles(5,5, Tiles, Board),
    Board = [[red, 0, 0, 0, 0], [red, blue, blue, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, blue, 0], [red, 0, 0, 0, red]].

test(change_board_layout_square_test_1) :-
    HorBoard = [[red, 0, 0], [red, blue, blue], [0, 0, 0]],
    change_board_for_color(HorBoard, VerBoard),
    VerBoard = [[red,red,0],[0,blue,0],[0,blue,0]].
test(change_board_layout_non_square_test_1) :-
    HorBoard = [[1,2,3,4],[5,6,7,8],[9,10,11,12]],
    change_board_for_color(HorBoard, VerBoard),
    VerBoard = [[1,5,9],[2,6,10],[3,7,11],[4,8,12]].
%A Winning board move
test(did_current_player_win_test1) :-
    NewTile = [2,3],
    Board = [[red, red, 0, 0], [blue, red, 0, 0], [blue, 0, 0, blue], [red, 0, 0, blue]],
    did_current_player_win(NewTile, 4, 4, Board, red).
%only a path to the top
test(did_current_player_win_test2, [fail]) :-
    NewTile = [2,3],
    Board = [[red, red, 0, 0], [blue, 0, 0, 0], [blue, 0, 0, blue], [red, 0, 0, blue]],
    did_current_player_win(NewTile, 4, 4, Board, red).
% only a path to the bottom
test(did_current_player_win_test3, [fail]) :-
    NewTile = [3,3],
    Board = [[red, 0, 0, 0], [blue, 0, 0, 0], [blue, 0, red, red], [red, 0, red, blue]],
    did_current_player_win(NewTile, 4, 4, Board, red).
test(did_current_player_win_non_square_test4) :-
    NewTile = [2,4],
    Board = [[0,red,red],[red,0,red],[0,red,0],[0,0,blue],[blue,red,0],[0,red,blue]],
    did_current_player_win(NewTile, 3, 6, Board, red).
test(generate_top_pos_test_1) :-
    generate_top_positions(2,4,3,Solution),
    Solution == [[2,3],[3,3],[3,4],[1,4]].
test(generate_bottom_pos_test_1) :-
    generate_bottom_positions(2,4,3,6,Solution),
    Solution == [[2,5],[1,5],[3,4],[1,4]].
test(get_element_test_1) :-
    Matrix = [[red, 0, 0, 0], [blue, 0, 0, 0], [blue, 0, red, red], [red, 0, red, blue]],
    Tile = [3,3],
    get_element(Matrix, Tile, Val),
    Val == red.
test(get_element_test_2, [fail]) :-
    Matrix = [[red, 0, 0, 0], [blue, 0, 0, 0], [blue, 0, red, red], [red, 0, red, blue]],
    Tile = [2,3],
    get_element(Matrix, Tile, Val),
    Val == red.
test(get_element_test_2, [fail]) :-
    Matrix = [[red, 0, 0, 0], [blue, 0, 0, 0], [blue, 0, red, red], [red, 0, red, blue]],
    Tile = [-2,10],
    get_element(Matrix, Tile, Val),
    Val == red.
:- end_tests(board).
