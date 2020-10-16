% Victor Matthijs, UGent, April 2020
% ===============================================================
% These rules will help to get the approriate arguments from game
% we could also have it done in the following way: game(Size, Turn, State, Ori, Tiles)
% and size as Size(Columns,Rows), and so on
% But I choosed to write rules/predicates for these things, so I could use the build-in arg predicate
% ===============================================================

:- module(game, [get_rows_and_columns/3, get_tiles/3, get_color/2, print_all_games/1, create_next_games/5, 
    diff_of_boards/3, get_coor/4, get_orientation/3, check_if_player_won/3, next_player_color/3, set_new_color_game/3,
    get_tile_info/4, get_state/2, create_game_from_board/5, check_orientation_colors/1, check_tiles/1]).
:- use_module(board).


% ==================================================================

% get_rows_and_columns(+Game, -Rows, -Columns)
% This rule will unify the Rows and Columns of our game that were parsed
% The game is mainly played on a squared board, but it is not always the case,
% so that's why there are separete Variables fort he rows and columns
get_rows_and_columns(Game, Rows, Columns) :-
    arg(1, Game, Size),
    arg(1, Size, Columns),
    arg(2, Size, Rows).

% get_tiles(+Game, -Count, -TilesList)
% This rule will in turn unify Count to the amount of tiles that are already on the board
% TilesArray will be unified with an array of tiles, of the form: tile('A',1,red).
get_tiles(Game, Count, TilesList) :-
    arg(5, Game, Tiles),
    arg(1, Tiles, Count),
    arg(2, Tiles, TilesList).

% get_tile_info(+Tile, -Column, -Row, -Color)
% This rule will give the specific info from one tile
get_tile_info(Tile, Column, Row, Color) :-
    arg(1, Tile, Column),
    arg(2, Tile, Row),
    arg(3, Tile, Color).

% get_state(+Game, -State)
% this rule will return either undecided or the color that has won
get_state(Game, State) :-
    arg(3, Game, St),
    arg(1, St, State).

% get_color(+Game, -Color)
% This simple rule will return the color of the player who is currently playing
get_color(Game, Color) :-
    arg(2, Game, Turn),
    arg(1, Turn, Color).

% get_orientation(+Game, -Player1, -Player2)
get_orientation(Game, Player1, Player2) :-
    arg(4, Game, Ori),
    arg(1, Ori, Player1),
    arg(2, Ori, Player2).

% next_player_color(+Game, +Color, -NextColor)
% Return the next player his color
next_player_color(Game, Color, NextColor) :-
    (get_orientation(Game, Color, NextColor) ; get_orientation(Game, NextColor, Color)), !.

% set_new_color_game(+Game, +CurrentColor, -Game)
% change the color of a game
set_new_color_game(Game, CurrentColor, game(Size, turn(NextColor), State, Ori, Tiles)) :-
    arg(1, Game, Size),
    arg(3, Game, State),
    arg(4, Game, Ori),
    arg(5, Game, Tiles),
    next_player_color(Game, CurrentColor, NextColor).

% ==========================================================================================
% Checking for errors in the board that just was parsed by the parser
% ==========================================================================================
check_orientation_colors(Game) :-
    get_orientation(Game, Player1, Player2),
    get_color(Game, Turn),
    (Turn = Player1 ; Turn = Player2) ;
    (print_message(error, invalid_Ori_color()), halt(2)).
prolog:message(invalid_Ori_color()) --> ["Current player color needs to be one of the orientation colors!"].

check_tiles(Game) :-
    get_tiles(Game, Count, Tiles),
    length(Tiles, LengthTiles),
    (LengthTiles = Count) ;
    (print_message(error, invalid_Tiles_Count()), halt(2)).
prolog:message(invalid_Tiles_Count()) --> ["The specified count of Tiles, is not the same as the total amount of given tiles!"].

% ==========================================================================================
% The following rules are used to print all the information to the screen
% ==========================================================================================

% print_all_games(+Games)
% print all Games from the list to the output
print_all_games([]).
print_all_games([Game]) :-
    print_output_one_game(Game).
print_all_games([Game|Rest]) :- 
    print_output_one_game(Game),
    print(~), nl,
    print_all_games(Rest).

% print_output_one_game(+Game)
% print the info of one game to the output
print_output_one_game(Game) :-
    get_color(Game, TurnColor),
    write("turn: "), write(TurnColor), nl,
    get_state(Game, State),
    next_player_color(Game, TurnColor, PreviousColor),
    write("state: "), print_state(State,PreviousColor), nl,
    get_rows_and_columns(Game, Rows, Columns),
    write("size: "), write(Columns), write(" * "), write(Rows), nl,
    get_orientation(Game, P1, P2),
    write("orientation: "), write(P1), write(" * "), write(P2), nl,
    get_tiles(Game, CountTiles, Tiles),
    write("tiles: "), write(CountTiles), nl,
    print_all_tiles(Tiles).

% print_state(+State, +PreviousColor)
% print the specific state to the output
print_state(State, PreviousColor) :-
    State = PreviousColor,
    write("won by "), write(PreviousColor).
print_state(State, _) :-
    write(State).

% print_all_tiles(Tiles)
% Print all the tiles to the output
print_all_tiles([]).
print_all_tiles([Tile|Rest]) :- 
    print_output_tile(Tile),
    print_all_tiles(Rest).

% print_output_tile(+Tile)
% Print one specific tile to the output
print_output_tile(Tile) :-
    get_tile_info(Tile, ColumnInt, Row, Color),
    Char_code_help is ColumnInt + 64,
    char_code(Column, Char_code_help),
    tab(5), write("("), write(Column), write(Row), write(") -> "), write(Color), nl.

% ==========================================================================================
% Everything to do with the creation of new games, starting from a current game or board
% ==========================================================================================

% create_next_games(+Game, +OldBoard, +Columns, +Boards, -Games)
% create a list of games from a list of boards.
create_next_games(_, _, _, [], []):- !.
create_next_games(Game, OldBoard, Columns, [Board|RestBoards], [NewGame|RestGames]) :- 
    create_game_from_board(Game, OldBoard, Board, Columns, NewGame),
    create_next_games(Game, OldBoard, Columns, RestBoards, RestGames).

% create_game_from_board(+Game, +OldBoard, +Board, +Columns, -Game)
% Create one game from a specific board
create_game_from_board(Game, OldBoard, Board, Columns, game(Size, turn(NextColor), state(NewState), Ori, tiles(NewCount, NewTiles))) :-
    flatten(OldBoard, OldFlatten),
    flatten(Board, NewFlatten),
    diff_of_boards(OldFlatten, NewFlatten, Position),
    get_coor(Position, Columns, Row, Column),
    get_tiles(Game, Count, Tiles),
    NewCount is Count + 1,
    get_color(Game, Color),
    append(Tiles, [tile(Column, Row, Color)], NewTiles),
    append([Column], [Row], NewTile),
    set_new_state_of_game(NewTile, Board, Game, NewState),
    arg(1, Game, Size),
    arg(4, Game, Ori),
    next_player_color(Game, Color, NextColor).

% set_new_state_of_game(+NewTile, +Board, +Game, -Color)
% if the newtile on the board, is a winning move, then the state will change
% to the winning color, otherwise it will stay undecided
set_new_state_of_game(NewTile, Board, Game, Color) :-
    check_if_player_won(Game, Board, NewTile),
    get_color(Game, Color).
set_new_state_of_game(_, _, Game, OldState) :-
    get_state(Game, OldState).

% check_if_player_won(+OldGame, +OldBoard, +NewTile)
% this rule will call the algortihm to check of a color has won, it will spin the board
% if the current color is playing from left to right
check_if_player_won(OldGame, OldBoard, NewTile) :-
    get_color(OldGame, CurrentColor),
    get_orientation(OldGame, CurrentColor, _), % horizontal
    get_rows_and_columns(OldGame, Rows, Columns),
    did_current_player_win(NewTile, Columns, Rows, OldBoard, CurrentColor),!.
check_if_player_won(OldGame, OldBoard, NewTile) :-
    get_color(OldGame, CurrentColor),
    get_orientation(OldGame, _, CurrentColor), % vertical
    get_rows_and_columns(OldGame, Rows, Columns),
    change_board_for_color(OldBoard, SymBoard),
    reverse(NewTile, RevNewTile),
    did_current_player_win(RevNewTile, Rows, Columns, SymBoard, CurrentColor),!.

% diff_of_boards(+Board1, +Board2, -Position)
% search for the newly added position, OldBoard and NewBoard
diff_of_boards(ListBoard1, ListBoard2, Position) :-
    hulp_diff_boards(ListBoard1, ListBoard2, 1, Position).

% hulp_diff_boards(+Board1, +Board2, +Index, -Solution)
hulp_diff_boards([], [], _, 0) :- !.
hulp_diff_boards([X|R1], [X|R2], Index, Solution) :-
    !,
    NextIndex is Index + 1,
    hulp_diff_boards(R1, R2, NextIndex, Solution).
hulp_diff_boards([H1|_],[H2|_], Index, Index) :-
    !,
    H1 \= H2.

% get_coor(+Number, +Columns, -Row, -ColumnNumber)
% This rule will return the matrixCoor of a specific position in a list
get_coor(Number, Columns, Row, ColumnNumber) :-
    HulpNumber is Number - 1,
    Row is div(HulpNumber, Columns) + 1,
    ColumnNumber is mod(HulpNumber, Columns)+1.
