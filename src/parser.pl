% Victor Matthijs, UGent, April 2020
% ==========================================================
% This file will contain all the code for parsing the input file
% The input file will have the following structure (doesn't need to be in this exact order)
% A) The size of the board --> size: Columns * Rows
% B) Which player his turn it is --> turn: red
% C) The orientation of the board --> orientation: red * blue
%       The first color plays from the top to the bottom
%       The second color plays from left to right
% D) The state of the game, can be undecided or "won by color" --> state: undecided
% E) The tiles that are already placed on the board
%       tiles: 4, followed by a tile on a newline
%               tile has the following form: (A1) -> red
%
% Below we show a small example of an input file:
%       orientation: red * blue
%       tiles: 4
%           (A2) -> red
%           (B2) -> blue
%           (A1) -> red
%           (C2) -> blue
%       size: 3 * 3
%       turn: red
%       state: undecided
%
% We will use different Prolog modules to support our implementation for the parser
% ==========================================================
% Important note!
% The implemantion of the parser uses Prolog definite clause grammar (DCG)
% A DCG rule has the following form
%       Head --> Body
% A rule's body consists of terminals and nonterminals. A terminal is a list, which stands for the elements it contains.
% The following is syntactic suger:
%       s --> np, vp.
%   for
%       s(A,C) :- np(A,B),vp(B,C)
%
% ==========================================================
:- module(parser, [parse/3]).
:- use_module(library(dcg/basics)).

% This is the main rule that will be called to parse a file
% The variable Game will unify with a rule representing a game
% this will also try to satisfy the subgoal: game_parts
parse(game(Size, Turn, State, Ori, Tiles)) --> 
    game_parts(game(Size, Turn, State, Ori, Tiles)), !.

% This following rule game_parts will try to satisfy all 
% it's subgoals, this rule is recursive. It is made recursive 
% so we make sure we get all the different structures from 
% the input file to construct our game
game_parts(game(Size, Turn, State, Ori, Tiles)) --> 
    game_part(game(Size, Turn, State, Ori, Tiles)), 
    game_parts(game(Size, Turn, State, Ori, Tiles)).
game_parts(game(Size, Turn, State, Ori, Tiles)) --> 
    game_part(game(Size, Turn, State, Ori, Tiles)).

% This rule will try to satisfy all the "structure" rules
% this can also be written as the following line of code:
% game_part(game(S, T, St, O, Ti))  --> size(S) ; turn(T) ; state(St) ; orientation(O) ; tiles(Ti).
% I think it's more understandable in the following way
game_part(game(Size, _, _, _, _))  --> size(Size).
game_part(game(_, Turn, _, _, _))  --> turn(Turn).
game_part(game(_, _, State, _, _)) --> state(State).
game_part(game(_, _, _, Ori, _))   --> orientation(Ori).
game_part(game(_, _, _, _, Tiles)) --> tiles(Tiles).

% =====================================================
% The next 5 rules are the game parts we need to construct a game of Con-Tac-Tix

% This rule will handle the parsing of our turn, it will look which color his turn it is
turn(turn(Turn)) --> ws, "turn:", ws, string_color(Turn), blanks_to_nl.

% This rule will handle the parsing of the size, this will give us our rows and columns
size(size(X,Y)) --> ws, "size:", ws, int(X), ws, "*", ws, int(Y), blanks_to_nl.

% This rule will handle the parsing of the state of the game, this will always be undecided
state(state(undecided)) --> ws, "state:", ws, "undecided", blanks_to_nl.

% This rule will handle the orientation of the game, it will parse two colors
orientation(orientation(C1,C2)) --> ws, "orientation:", ws, string_color(C1), ws, "*", ws, string_color(C2), blanks_to_nl.

% The following rules are all used to parse the tiles that are on the board.
% These rules will use recursion to make sure we parse all the tiles.
tiles(tiles(Number, ArrayOfTiles)) --> ws, "tiles:", ws, int(Number), blanks_to_nl,
                        parsePlayerTiles(ArrayOfTiles).

% We parse all tiles into an array
parsePlayerTiles([Tile|ArrayOfTiles]) --> parseNewTile(Tile), parsePlayerTiles(ArrayOfTiles).
parsePlayerTiles([Tile]) --> parseNewTile(Tile).
parsePlayerTiles([]) --> [].

% This rule will parse one tile
parseNewTile(tile(C,R,Color)) --> ws, "(", ws, parse_Col(C), int(R), ws, ")", ws,"->", ws, string_color(Color), blanks_to_nl.


% =====================================================
% The rules below are necessary to satisfy some subgoals above in the structures

% handeling the optinal whitespaces
% (not really super necissary to rewrite the predicate blanks, but it just was easier this way)
ws --> blanks.

% Parsing the different colors
string_color(Color) --> string(X), {atom_codes(Color, X)}.

% parsing an integer
int(Result) --> number_(Number), { number_codes(Result, Number) }.

number_([H|T]) --> check_digit(H), number_(T).
number_([H])    --> check_digit(H).
check_digit(D) --> [D], { char_type(D, digit) }.

% Parsing a column character
parse_Col(Result) --> [C], {atom_codes(Help, [C]), char_code(Help, Help2), Result is Help2 - 64}.