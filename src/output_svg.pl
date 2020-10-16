% Victor Matthijs, UGent, April 2020
% ===============================================================
% This module will print the correct SVG representation of a specific
% board. There may also be multipleboards that must be printed.
% The svg that is printed, is based on the files that were found
% on Ufora and in the Opgave directory.
% ===============================================================

:- module(output_svg, [print_svg_boards/2]).

% print_svg_boards(+Boards, +Ori)
% This is the main rule, here all boards are passed to be printed out
% on the screen. We will handle all boards using recursion
print_svg_boards(Boards, Ori) :-
    write("<?xml version=\"1.0\"?>"), nl,
    length(Boards, Length_of_boards),
    nth1(1, Boards, Board),
    length(Board, Rows),
    nth1(1, Board, FirstRow),
    length(FirstRow, Columns),
    ViewBoxRows is (2 + Rows) * 1.5 * Length_of_boards,
    ViewBoxColumns is (2 + Columns + (Rows / 2)) * 1.75,
    Width is 500,
    Height is Rows * 250 * Length_of_boards,
    write("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""), write(Width), 
    write("\" height=\""), write(Height), write("\" viewBox=\"0 0 "), 
    write(ViewBoxColumns), write(" "),write(ViewBoxRows), write("\">"), nl,
    print_defs_part(),
    print_all_boards(Boards, Rows, Columns, 0, Ori),
    write("</svg>").

print_all_boards([], _, _, _, _) :- !.
print_all_boards([Board|RestBoards], Rows, Columns, BoardNumber, Ori) :-
    write("<g transform=\"translate(0  "), write(BoardNumber), write(")\">"),
    print_board_part(Rows, Columns, Board, Ori),
    write("</g>"),
    NewBoardNumber is BoardNumber + 10,
    print_all_boards(RestBoards, Rows, Columns, NewBoardNumber, Ori).

% print_board_part(+Rows, +Columns, +Board, +Ori)
% print the first part of our SVG board
print_board_part(Rows, Columns, Board, Ori) :-
    tab(2), write("<g transform=\"matrix(1.73205080756 0 0.86602540378 1.5 3.46410161512 2.5)\">"), nl,
    % print the names on the sidebars
    print_player_names_and_sidebars(Rows, Columns, Ori),
    %print all the column and row numbers
    print_row_column_numbers(Rows, Columns),
    % print all the tiles of the board
    print_board(0, Board),
    %print closing tag
    tab(2), write("</g>"), nl.

%print_board(+Place, +Rows)
% This rule will print all the tiles on the board, with or without value
% It will use the Variable Place to count on which row we are
print_board(Place, [Row]) :-
    print_row_from_board(Place, 0, Row).
print_board(Place, [Row|RestRows]) :-
    print_row_from_board(Place, 0, Row),
    NewPlace is Place + 1,
    print_board(NewPlace, RestRows).

% print_row_from_board(+RowPlace, +Place, +Values)
% this will evaluate a row and print all values of that row to output,
% here we also use Place to count where we are on the board
print_row_from_board(RowPlace, Place, [Value]) :-
    print_specific_tile(RowPlace, Place, Value).
print_row_from_board(RowPlace, Place, [Value|RestValues]) :-
    print_specific_tile(RowPlace, Place, Value),
    NewPlace is Place + 1,
    print_row_from_board(RowPlace, NewPlace, RestValues).

% print_specific_tile(+RowNumber, +ColumnNumber, +Value)
% print the actual tile, after we found the row and column numbers
print_specific_tile(RowNumber, ColumnNumber, Value) :-
    tab(4), write("<use href=\"#tile\" x=\""), write(ColumnNumber),
    write("\" y=\""), write(RowNumber), write("\""),
    (Value = 0 -> 
        write("/>")
    ;
    write(" fill=\""), write(Value), write("\"/>") 
    ), nl.

% print_row_column_numbers(+Rows, +Columns)
% Print the sides of our SVG board
print_row_column_numbers(Rows, Columns) :-
    print_columns(Columns),
    print_rows(Rows).

% print_rows(+Rows)
% print all of our rows on our SVG Board
print_rows(0).
print_rows(Rows) :-
    NewRows is Rows - 1,
    print_rows(NewRows),
    tab(4), write("<text class=\"row_or_col\" x=\"-0.95\" y=\""), write(NewRows), write("\">"), write(Rows), write("</text>"), nl.

% print_columns(+Columns)
% print all columns of our SVG Board
print_columns(0).
print_columns(Columns) :-
    NewColumn is Columns - 1,
    print_columns(NewColumn),
    CorrectCharNumber is Columns + 64,
    char_code(ColumnChar, CorrectCharNumber), 
    tab(4), write("<text class=\"row_or_col\" y=\"-0.65\" x=\""), write(NewColumn), write("\">"), write(ColumnChar),write("</text>"), nl.

% print_player_names_and_sidebars(+Rows, +Columns, +Ori)
% print all the player info and sidebars
print_player_names_and_sidebars(Rows, Columns, Ori) :-
    % print openeing tag
    tab(4), write("<g>"), nl,
    % get the correct colors from the orientation
    arg(1, Ori, Player1),
    arg(2, Ori, Player2),
    
    HulpRows is Rows - 1,
    HulpColumns is Columns - 1,
    
    % print the text for player 1, in the correct color
    tab(6), write("<text text-anchor=\"start\" fill=\""), write(Player1), 
    write("\" font-size=\"0.5\" y=\"-1.1\" x=\"-0.5\">Player 1</text>"), nl,
    tab(6), write("<polygon fill=\""), write(Player1) ,write("\" points=\"-1,-1 0,0 "),
    write(HulpColumns), write(",0 "), write(Columns), write(",-1\"/>"), nl,
    tab(6), write("<polygon fill=\""), write(Player1), write("\" points=\"-1,"),
    write(Rows), write(" 0,"), write(HulpRows), write(" "), write(HulpColumns), write(","), 
    write(HulpRows),write(" "), write(Columns), write(","), write(Rows), write("\"/>"), nl,
    
    % print the text for player 2, in the correct color
    tab(6), write("<text text-anchor=\"start\" fill=\""), write(Player2), 
    write("\" font-size=\"0.5\" transform=\"rotate(90)\" y=\"-"), write(Columns), write(".1\" x=\"-0.5\">Player 2</text>"), nl,
    tab(6), write("<polygon fill=\""), write(Player2), write("\" points=\"-1,-1 0,0 0,"), write(HulpRows),
    write(" -1,"), write(Rows), write("\"/>"),nl,
    tab(6), write("<polygon fill=\""), write(Player2), write("\" points=\""), write(Columns),
    write(",-1 "), write(HulpColumns), write(",0 "), write(HulpColumns), write(","), write(HulpRows),
    write(" "), write(Columns), write(","), write(Rows), write("\"/>"),nl,

    % print closing tag
    tab(4), write("</g>"), nl.

% print_defs_part()
% print the specific defs part of our svg
print_defs_part() :-
    tab(2), write("<defs>"), nl,
    print_style_of_svg(),
    tab(4), write("<polygon id=\"tile\" points=\" 0.35,0.35 0.68,-0.35 0.35,-0.68 -0.35,-0.35 -0.68,0.35 -0.35,0.68 \" stroke-width=\"0.01\" stroke=\"black\"/>"), nl,
    tab(2), write("</defs>"), nl.

% print_style_of_svg()
% print the specific style part of our svg
print_style_of_svg() :-
    tab(4), write("<style>"),nl,
    write("use:not([fill]) {fill: #ECECEC;}"), nl,
    write("text.row_or_col {"), nl,
    tab(4), write("font-size: 0.3px;"), nl,
    tab(4), write("font-weight: bold;"), nl,
    tab(4), write("font-family: sans;"), nl,
    tab(4), write("fill: white;"), nl,
    tab(4), write("stroke: black;"), nl,
    tab(4), write("stroke-width: 0.005px;"), nl,
    tab(4), write("paint-order: stroke;"), nl,
    write("}"), nl, tab(4), write("</style>"),nl.

    
    
    