% Victor Matthijs, UGent, April 2020
% ==========================================================
% This file will contain all the rules that will help with the
% reasoning of the game, it is mainly focused on the min-max 
% algorithm that we have seen in the lessons.
% The first version of the minimax algorithm is located below, this version didn't use 
% the alpha beta pruning yet. The second version is the one I included in 
% the final result. This one is located just below, this does contain the 
% alpha beta pruning
% LINK: http://colin.barker.pagesperso-orange.fr/lpa/tictac.htm
% LINK: https://www.cpp.edu/~jrfisher/www/prolog_tutorial/5_3.html 
% the two links above where used as inspiration for the minimax with alpha-beta pruning
% ==========================================================

:- module(min_max_ai, [generate_all_possible_moves/5, minimax_alfa_beta/9]).

:- use_module(game).
:- use_module(board).

% =========================================================================

% minimax_alfa_beta(+Depth, +CurrentMove, +Game, +OldBoard, +CurrentColor, +Alpha, +Beta, -BestMove, -BestValue)
% the actual start of the minimax algorithm, it is a recursive rule that can be called with every depth.
% It will return the bestMove with the highest value from a searchtree.
minimax_alfa_beta(0, CurrentMove, Game, OldBoard, CurrentColor, _, _, CurrentMove, BestValue) :-
    evaluate_node_minimax(Game, CurrentColor, OldBoard, CurrentMove, BestValue), !.
minimax_alfa_beta(Depth, CurrentMove, Game, OldBoard, CurrentColor, Alpha, Beta, BestMove, BestValue) :-
    Depth > 0, !,
    get_rows_and_columns(Game, Rows, Columns),
    get_the_best_moves(Game, CurrentColor, CurrentMove, Rows, Columns, MovesList), !,
    NewDepth is Depth - 1,
    NewAlpha is -Beta,
    NewBeta is -Alpha,
    choose_best_from_moves(NewDepth, MovesList, Game, OldBoard, CurrentColor, NewAlpha, NewBeta, 0, BestMove, BestValue).
% choose_best_from_moves(+Depth, +Moves, +Game, +OldBoard, +CurrentColor, +Alpha, +Beta, -Record, -BestMove, -BestValue)
% this will choose the best move on a specific depth of the searchtree, keeping in mind 
% that we will execute alpha beta pruning at these nodes
choose_best_from_moves(_Depth, [],_Game, _CurrentMove, _Color, Alpha, _Beta, Move, Move, Alpha) :- !.
choose_best_from_moves(Depth, [Move|RestMoves], Game, OldBoard, CurrentColor, Alpha, Beta, Record, BestMove, BestValue) :-
    next_player_color(Game, CurrentColor, NextColor),
    minimax_alfa_beta(Depth, Move, Game, OldBoard, NextColor, Alpha, Beta, _, HulpValue),
    Value is -HulpValue,
    try_cut_branch(Move, Value, Depth, Game, CurrentColor , Alpha, Beta, RestMoves, OldBoard, Record, BestMove, BestValue).
% try_cut_branch(+Move, +Value, +Depth, +Game, +Color, +Alpha, +Beta, +Moves, +OldBoard, -Record, -Move, -Value)
% This rule will execute the alpha beta pruning, which will speedup the overall algortihm
try_cut_branch(Move, Value, _Depth, _Game, _Color, _Alpha, Beta, _Moves, _OldBoard, _Record, Move, Value) :-
    Value >= Beta, !.
try_cut_branch(_, Value, Depth, Game, CurrentColor, Alpha, Beta, Moves, OldBoard, Record, BestMove, BestValue) :-
    Value =< Alpha, !,
    choose_best_from_moves(Depth, Moves, Game, OldBoard, CurrentColor, Alpha, Beta, Record, BestMove, BestValue).
try_cut_branch(Move, Value, Depth, Game, CurrentColor, _, Beta, Moves, OldBoard, _, BestMove, BestValue) :-
    Value < Beta, !,
    choose_best_from_moves(Depth, Moves, Game, OldBoard, CurrentColor, Value, Beta, Move, BestMove, BestValue).

% ===========================================================
% Give a score/value to a specific board. A winning board will
% get a value of 10, smaller value's will be given to boards that have a path 
% to the top or bottom of the board for there color.
% ===========================================================

% evaluate_node_minimax(+Game, +ColorToPlay, +BeginBoard, +EndBoard, -Value)
evaluate_node_minimax(Game, ColorToPlay, BeginBoard, EndBoard, Value) :-
    flatten(BeginBoard, BeginFlatten),
    flatten(EndBoard, EndFlatten),
    get_rows_and_columns(Game, _, Columns),
    get_all_diffs(BeginFlatten, EndFlatten, DiffPositions),
    get_coors(DiffPositions, Columns, Tiles),
    get_value_for_board(Tiles, ColorToPlay, Game, EndBoard, Value).

% get_value_for_board(+Tiles, +ColorToPlay, +Game, +EndBoard, -Value)
% a heuristic rule, return a value for a specfic board, a big score for
% a winning board, smaller scores for boards with a path to top or bottom.
% 0 as a score for board without a path.
get_value_for_board(Tiles, ColorToPlay, Game, EndBoard, Value) :-
    check_if_new_tiles_won(Tiles, ColorToPlay, Game, EndBoard, Value), !.
get_value_for_board(Tiles, ColorToPlay, Game, EndBoard, EndValue) :-
    next_player_color(Game, ColorToPlay, OtherColor),
    check_if_new_tiles_won(Tiles, OtherColor, Game, EndBoard, Value),
    EndValue is -Value, !.
get_value_for_board(Tiles, ColorToPlay, Game, EndBoard, EndValue) :-
    % at this point both players can't place a tile that will win the game
    % we will give a higher score for a player that has a bigger and larger path.
    calculate_score_for_top_path(Tiles, ColorToPlay, Game, EndBoard, TopEndValue),
    calculate_score_for_bottom_path(Tiles, ColorToPlay, Game, EndBoard, BottomEndValue),
    EndValue = max(TopEndValue,BottomEndValue).
get_value_for_board(_, _, _, _, 0) :- !.

% calculate_score_for_top_path(+Tiles, +Color, +Game, +EndBoard, -EndValue)
% return a score for a treenode/board, we give a bigger score if the board has a longer path to the
% top, so it has more change to win than a board that has a smaller path to the top.
calculate_score_for_top_path([], _, _, _, 0) :- !.
calculate_score_for_top_path([(Color, Tile)|RestTiles], Color, Game, EndBoard, EndValue) :-
    (get_orientation(Game, Color, _), % horizontal
    get_rows_and_columns(Game, Rows, Columns),
    find_top_hulp([Tile], [], Columns, Rows, Color, EndBoard, His, path)
        ;
    get_orientation(Game, _, Color), % vertical
    get_rows_and_columns(Game, Rows, Columns),
    change_board_for_color(EndBoard, SymBoard),
    reverse(Tile, RevNewTile),
    find_top_hulp([RevNewTile], [], Rows, Columns, Color, SymBoard, His, path)),
    length(His, LengthHis),
    CurrentValue is LengthHis/Rows,
    calculate_score_for_top_path(RestTiles, Color, Game, EndBoard, OtherEndValue),
    (OtherEndValue > CurrentValue -> EndValue = OtherEndValue ; EndValue = CurrentValue).
    
calculate_score_for_top_path([_|RestTiles], ColorToPlay, Game, EndBoard, EndValue) :-
    calculate_score_for_top_path(RestTiles, ColorToPlay, Game, EndBoard, EndValue).

% calculate_score_for_bottom_path(+Tiles, +Color, +Game, +EndBoard, -EndValue)
% return a score for a treenode/board, we give a bigger score if the baord has a longer path to the
% bottom, so it has more change to win than a board that has a smaller path to the bottom.
calculate_score_for_bottom_path([], _, _, _, 0) :- !.
calculate_score_for_bottom_path([(Color, Tile)|RestTiles], Color, Game, EndBoard, EndValue) :-
    (
    get_orientation(Game, Color, _), % horizontal
    get_rows_and_columns(Game, Rows, Columns),
    find_bottom_hulp([Tile], [], Columns, Rows, Color, EndBoard, His, path)
        ;
    get_orientation(Game, _, Color), % vertical
    get_rows_and_columns(Game, Rows, Columns),
    change_board_for_color(EndBoard, SymBoard),
    reverse(Tile, RevNewTile),
    find_bottom_hulp([RevNewTile], [], Rows, Columns, Color, SymBoard, His, path)
    ),
    length(His, LengthHis),
    CurrentValue is LengthHis/Rows,
    calculate_score_for_top_path(RestTiles, Color, Game, EndBoard, OtherEndValue),
    (OtherEndValue > CurrentValue -> EndValue = OtherEndValue ; EndValue = CurrentValue).
calculate_score_for_bottom_path([_|RestTiles], ColorToPlay, Game, EndBoard, EndValue) :-
    calculate_score_for_bottom_path(RestTiles, ColorToPlay, Game, EndBoard, EndValue).

% check_if_new_tiles_won(+Tiles, +ColorToPlay, +Game, +EndBoard, -Score)
% this will check if a player has won a game, if so than that end node in the 
% minimax tree will get a bigger score than a not winning treenode/board.
check_if_new_tiles_won([(ColorToPlay, Tile)|_], ColorToPlay, Game, EndBoard, 10) :-
    next_player_color(Game, ColorToPlay, HulpColor),
    set_new_color_game(Game, HulpColor, OldGame),
    check_if_player_won(OldGame, EndBoard, Tile),
    !.
check_if_new_tiles_won([_|RestTiles], ColorToPlay, Game, EndBoard, Value) :-
    check_if_new_tiles_won(RestTiles, ColorToPlay, Game, EndBoard, Value).

% TODO: this is almost the same rule as in the board module
% try to make one rule of it !!!!
get_coors([], _, []) :- !.
get_coors([(Pos,Color)|RestPos], Columns, [(Color, Tile)|RestTiles]) :-
    get_coor(Pos, Columns, RowNumber, ColumnNumber),
    append([ColumnNumber], [RowNumber], Tile),
    get_coors(RestPos, Columns, RestTiles).

% diff_of_boards(+Board1, +Board2, -Position)
% search for the newly added position, OldBoard and NewBoard
get_all_diffs(ListBoard1, ListBoard2, Positions) :-
    hulp_diff_boards(ListBoard1, ListBoard2, 1, Positions).

% hulp_diff_boards(+Board1, +Board2, +Index, -Solution)
hulp_diff_boards([], [], _, []) :- !.
hulp_diff_boards([X|R1], [X|R2], Index, Solution) :-
    !,
    NextIndex is Index + 1,
    hulp_diff_boards(R1, R2, NextIndex, Solution).
hulp_diff_boards([H1|R1],[H2|R2], Index, [(Index,H2)|RestIndeces]) :-
    !,
    H1 \= H2,
    NextIndex is Index + 1,
    hulp_diff_boards(R1, R2, NextIndex, RestIndeces).

% ===========================================================
% The code below will generate all next possible boards, starting
% from a specific board. We will first convert the board to a list.
% We can call a function like we have seen during the lessons to
% generate all next boards, with the findall rule.
% LINK: http://colin.barker.pagesperso-orange.fr/lpa/tictac.htm
% I found the following link on the web and took one key idea and
% implemented it myself in this code. The idea being that when you 
% have a winning board, you should only return that board and not 
% try to go down the other branches, making the final algorithm even
% faster.
% ===========================================================

% This rule will return the best possible moves, with moves being board
% representations. It has two options, one: it will return one specific move,
% this move will be a winning move. Two: it will return all moves, there is no
% winning move in the list of possible moves
get_the_best_moves(Game, CurrentColor, OldBoard, Rows, Columns, BestMove) :-
    generate_all_possible_moves(CurrentColor, OldBoard, Rows, Columns, Moves),
    get_winning_moves(Moves, OldBoard, Game, Columns, CurrentColor, BestMove).
% There is no winning move so just return all possible moves.
get_the_best_moves(_, CurrentColor, OldBoard, Rows, Columns, Moves) :-
    generate_all_possible_moves(CurrentColor, OldBoard, Rows, Columns, Moves).

% this simple rule will return a winning move from a list of moves, it will fail 
% if it doesn't find any winning move
get_winning_moves([Move|_], OldBoard, Game, Columns, CurrentColor, [Move]) :-
    flatten(OldBoard, OldFlatten),
    flatten(Move, NewFlatten),
    diff_of_boards(OldFlatten, NewFlatten, Position),
    get_coor(Position, Columns, Row, Column),
    append([Column], [Row], NewTile),
    next_player_color(Game, CurrentColor, HulpColor),
    set_new_color_game(Game, HulpColor, OldGame),
    check_if_player_won(OldGame, OldBoard, NewTile), !.
get_winning_moves([_|RestMoves], OldBoard, Game, Columns, CurrentColor, BestMove) :-
    get_winning_moves(RestMoves, OldBoard, Game, Columns, CurrentColor, BestMove).

% generate_all_possible_moves(+Color, +Board, +Rows, -Moves)
% This rule will generate all possible moves that a specific
% color can do, a correct move is bassicly a place that doesn't has
% a color on it yet.
generate_all_possible_moves(Color, Board, Rows, Columns, Moves) :-
    flatten(Board, FlattenBoard),
    all_possible_moves(Color, FlattenBoard, Moves1),!,
    get_boards_back(Moves1, Rows, Columns, Moves).

% get_boards_back(+List, +Rows, -ListBoards)
% This is a hulp rule to generate the solution boards with all
% the possible moves. It will parse all the lists into the correct
% boards.
get_boards_back([], _, _,[]).
get_boards_back([BoardList|Rest], Rows, Columns, [NewBoard|NewRest]) :-
    list2matrix(BoardList, Rows, Columns, NewBoard),
    get_boards_back(Rest, Rows, Columns, NewRest).

% possible_move(+Color, +Row, -NewRow)
% This rule is based on the implementation that we have seen in the lesson
% It will place a "tile"/color on every available spot of the board.
possible_move(Color, [0 | Rest], [Color | Rest]).
possible_move(Color, [X | Rest], [X | Rest2]) :-
    possible_move(Color, Rest, Rest2).

% all_possible_moves(+Color, +Board, -AllMoves).
% this rule uses findall to generate all possible next moves
all_possible_moves(Color, Board, AllMoves) :-
    findall(Move, possible_move(Color, Board, Move), AllMoves).

% list2matrix(+List, +Rows, -Matrix)
% this is a hulp rule that will transform a list into a Matrix
list2matrix(List, Rows, Columns, Matrix) :-
    length(Matrix, Rows),
    maplist(length_(Columns), Matrix),
    append(Matrix, List).

length_(Length, List) :- length(List, Length).

% ==========================================================================
% the code below has been commented out, the reason being that it was the first
% version of the minimax algorithm, this version didn't work with alpha-beta
% pruning and it was a bit to slow
/* % minimax_depth(+Depth, +Game, +CurrentBoard, +ColorToPlay, -Bestmove, -BestValue)
    % this is the top level rule, we start here to construct a searchtree
    % where every node in the tree is a game and the edges are moves from one
    % board to an other board. We return the best move possible from the startBoard.
    minimax_depth(Depth, Game, CurrentBoard, ColorToPlay, _, BestMove, BestVal) :-
    print("MinMax_depth rule on depth: "), write(Depth), nl,
    Depth > 0,
    get_rows_and_columns(Game, Rows, Columns),
    generate_all_possible_moves(ColorToPlay, CurrentBoard, Rows, Columns, MovesList), !,
    best_move_depth(Depth, MovesList, Game, CurrentBoard, ColorToPlay, BestMove, BestVal).
    minimax_depth(0, Game, CurrentBoard, _, OldBoard, CurrentBoard, BestValue) :-
    %print("Color to play is: "), print(ColorToPlay), nl,
    evaluate_current_board(Game, OldBoard, CurrentBoard, BestValue).
    %print("value of this board is: "), write(BestValue), nl.

    % best_move_depth(+Depth, +Boards, +Game, +OldBoard, +ColorToPlay, -CurrentMove, -BestValue)
    % this rule will select the best move from a list of moves, we expand the search tree with this rule
    best_move_depth(Depth, [CurrentMove], Game, OldBoard, ColorToPlay, CurrentMove, BestValue) :-
    Depth > 0,
    NewDepth is Depth - 1,
    next_player_color(Game, ColorToPlay, NextColor),
    minimax_depth(NewDepth, Game, CurrentMove, NextColor, OldBoard, _, BestValue).
    best_move_depth(Depth, [CurrentMove|RestMoves], Game, OldBoard, ColorToPlay, BestMove, BestValue) :-
    Depth > 0,
    NewDepth is Depth - 1,
    next_player_color(Game, ColorToPlay, NextColor),
    ( Depth =:= 4, check_if_current_board_is_end(Game, ColorToPlay, CurrentMove, OldBoard) -> 
    BestValue is 1,
    BestMove = CurrentMove ; 
    minimax_depth(NewDepth, Game, CurrentMove, NextColor, OldBoard, _, ValDeeper),
    best_move_depth(Depth, RestMoves, Game, OldBoard, ColorToPlay, BestMoveSameDepth, BestValueSameDpeth),
    compare_moves(Depth, CurrentMove, ValDeeper, BestMoveSameDepth, BestValueSameDpeth, BestMove, BestValue)).

    % check_if_current_board_is_end(+Game, _, +CurrentMove, +OldBoard)
    % This rule will check if a current board has won, this will be used to cut some
    % branches in the searchtree
    check_if_current_board_is_end(Game, _, CurrentMove, OldBoard) :-
    %nl,print("Check if current board is end, currentBoard: "), write(CurrentMove), nl,
    %print("CurretnColor: "), write(ColorToPlay), nl,
    flatten(OldBoard, OldFlatten),
    flatten(CurrentMove, NewFlatten),
    get_rows_and_columns(Game, _, Columns),
    diff_of_boards(OldFlatten, NewFlatten, Position),
    get_coor(Position, Columns, Row, Column),
    append([Column], [Row], NewTile),
    %print("NewTile on the board is: "), write(NewTile), nl,nl,
    check_if_player_won(Game, OldBoard, NewTile).
    %print("Found winning board !!!").

    % ===========================================================
    % The code below will compare two moves/boards and look which one
    % the better one is, when we are on a depth that is even in our search tree, then
    % we want to select the MAX value, if we are on an uneven depth, then the MIN value
    % will be choosen.
    % ===========================================================

    % compare_moves(+Depth, +MoveA, +ValueA, +MoveB, +ValueB, -MoveA, -ValueA)
    % Compare two moves with the corresponding value's, an even depth will return the 
    % move with the higest value, an uneven will return the move with the lowest depth.
    compare_moves(Depth, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
    check_if_even(Depth), % Depth is an even number, so choose MAX value
    ValueA > ValueB, !.
    compare_moves(Depth, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
    check_if_even(Depth), % Depth is an even number, so choose MAX value
    ValueA =< ValueB, !.
    compare_moves(Depth, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
    \+(check_if_even(Depth)), % Depth is an uneven number, so choose MIN value
    ValueA > ValueB, !.
    compare_moves(Depth, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
    \+(check_if_even(Depth)), % Depth is an uneven number, so choose MIN value
    ValueA =< ValueB, !.

    % check_if_even(+Depth)
    % this rule will simply return True if the depth is even,
    % else it will return False 
    check_if_even(Depth):- mod(Depth,2) =:= 0.
*/
% ==========================================================================