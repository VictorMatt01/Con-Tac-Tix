% Victor Matthijs, UGent, April 2020
% ===============================================================
% This main prolog file contains all the rules to play the game
% It has two main rules where we can use a command on the command line with
% or without a TEST parameter, the differnece is described below.
% There is one more argument that can be provided when a user exicutes a command, this argument
% is the SVG argument. When this is provided, the program will not return plain text for the 
% game representations, but it will generate an SVG for every board that it must return as the 
% output.
% The follwoning commands are all acceptabke and will produce an output:
% cat invoerFile | swipl -f none -t halt -g main -q main.pl SVG
% cat invoerFile | swipl -f none -t halt -g main -q main.pl TEST SVG
% cat inputFile | swipl -f none -t halt -g main -q main.pl SVG TEST
% ===============================================================

% loading in the modules that we will use
:- use_module(parser).
:- use_module(board).
:- use_module(game).
:- use_module(min_max_ai).
:- use_module(output_svg).

:- consult("tests/game.plt").
:- consult("tests/board.plt").
:- consult("tests/min_max_ai.plt").
:- run_tests.
% :- set_prolog_stack(global, limit(100 000 000 000)).


% This is the main rule, the game will be started here
% The input variables will also be handled at this stage
% possible input variables are: TEST and SVG, can be used together
main :-
    current_prolog_flag(argv, Argv),
    sort(Argv,ArgvSorted),
    main(ArgvSorted).

% main(+List)
% handle the inputfile with both TEST and SVG arguments passed
main([_,_|_]) :-
    % First we need to parse the input file into a game representation
    phrase_from_stream(parse(Game), user_input),
    
    % the following rule will create a board from our game
    main_start(Game, OldBoard, Rows, Columns, Color),
    
    % This will generate all possible moves, starting from a board
    generate_all_possible_moves(Color, OldBoard, Rows, Columns, Moves),
    arg(4, Game, Ori),
    print_svg_boards(Moves, Ori).


main(['SVG']) :-
    % First we need to parse the input file into a game representation
    phrase_from_stream(parse(Game), user_input),
    
    % the following rule will create a board from our game
    main_start(Game, OldBoard, _, _, ColorToPlay),
    
    % Call the minimax rule, this will return the best possible move that a player can make
    % it must play the winning move if it can win
    minimax_alfa_beta(4, OldBoard, Game, OldBoard, ColorToPlay, -200, 200, BestMove, _),
    arg(4, Game, Ori),
    print_svg_boards([BestMove], Ori).


% main(+List)
% This will start the correct mode
% If an argument has been passed called TEST, then the algorithm will produce every possible move
main(['TEST']) :-
    % First we need to parse the input file into a game representation
    phrase_from_stream(parse(Game), user_input),

    % the following rule will create a board from our game
    main_start(Game, OldBoard, Rows, Columns, Color),

    % This will generate all possible moves, starting from a board
    generate_all_possible_moves(Color, OldBoard, Rows, Columns, Moves),
    
    % Moves only contains board representations, so we still need to make games from it (Game is a Prolog atom)
    create_next_games(Game, OldBoard, Columns, Moves, Games),
    
    %print all the possible next moves to standard output
    print_all_games(Games).

% main(+List)
% If no TEST argument has been passed and No SVG arguement, then the algorithm will return the best possible move
% and print this move out to standard output
main([])       :- 
    % First we need to parse the input file into a game representation
    phrase_from_stream(parse(Game), user_input),
    
    % the following rule will create a board from our game
    main_start(Game, OldBoard, _, Columns, ColorToPlay),
    % Call the minimax rule, this will return the best possible move that a player can make
    % it must play the winning move if it can win
    %minimax_depth(4, Game, OldBoard, ColorToPlay, OldBoard, BestMove, _),
    minimax_alfa_beta(4, OldBoard, Game, OldBoard, ColorToPlay, -200, 200, BestMove, _),

    % BestMoves only contains board representations, so we still need to make games from it (Game is a Prolog atom)
    create_next_games(Game, OldBoard, Columns, [BestMove], Games),
    
    % Print the best game to the screen
    print_all_games(Games).

% main_start(+Game, -Board, -Rows, -Columns, -Color)
% This rule will handle the parsed input and construct a matrix board from it
% making sure that all tiles are layed on the correct spot on the board.
main_start(Game, Board, Rows, Columns, Color) :-
    % Check for parse errors
    check_orientation_colors(Game),
    check_tiles(Game),
    %get the current color of the player who's turn it is
    get_color(Game, Color),
    % get the correct size of the game, we need this to construct the board
    get_rows_and_columns(Game, Rows, Columns),
    % get_tile will return all tiles that are already on the board
    get_tiles(Game, _, ArrayOfTiles),
    % this rule will construct our board matrix
    construct_board_from_tiles(Rows, Columns, ArrayOfTiles, Board).