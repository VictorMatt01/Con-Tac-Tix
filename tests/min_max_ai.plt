:- use_module(src/min_max_ai).
:- begin_tests(min_max_ai).

test(minimax_test_1) :-
    OldBoard = [[red,0,0],[red,blue,blue],[0,0,0]],
    Game = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    minimax_alfa_beta(4,OldBoard,Game,OldBoard,red,-200,200,BestMove,_),
    BestMove = [[red,0,0],[red,blue,blue],[red,0,0]].

test(minimax_test_2) :-
    OldBoard = [[red,0,0],[red,blue,blue],[0,0,0]],
    Game = game(size(3,3),turn(red),state(undecided),orientation(red,blue),tiles(4,[tile(1,2,red),tile(2,2,blue),tile(1,1,red),tile(3,2,blue)])),
    minimax_alfa_beta(4,OldBoard,Game,OldBoard,blue,-200,200,BestMove,_),
    BestMove = [[red,0,0],[red,blue,blue],[blue,0,0]].

test(minimax_test_3) :- 
    OldBoard = [[0,0,0,0,0,'SandyBrown',0,0,0,0],[0,0,0,0,0,'SandyBrown',lime,lime,lime,lime],[0,0,0,0,0,0,0,0,0,0],[lime,lime,lime,'SandyBrown',0,0,0,0,0,0],[0,0,0,'SandyBrown',0,0,0,0,0,0]],
    Game = game(size(10,5),turn(lime),state(undecided),orientation('SandyBrown',lime),tiles(11,[tile(1,4,lime),tile(3,4,lime),tile(4,4,'SandyBrown'),tile(2,4,lime),tile(6,1,'SandyBrown'),tile(6,2,'SandyBrown'),tile(8,2,lime),tile(4,5,'SandyBrown'),tile(10,2,lime),tile(7,2,lime),tile(9,2,lime)])),
    minimax_alfa_beta(4, OldBoard, Game, OldBoard, lime, -200, 200, BestMove, _),
    BestMove = [[0,0,0,0,0,'SandyBrown',0,0,0,0],[0,0,0,0,0,'SandyBrown',lime,lime,lime,lime],[0,0,0,0,lime,0,0,0,0,0],[lime,lime,lime,'SandyBrown',0,0,0,0,0,0],[0,0,0,'SandyBrown',0,0,0,0,0,0]].

test(generate_all_possible_moves_test_1) :-
    OldBoard = [[red,0,0],[red,blue,blue],[0,0,0]],
    generate_all_possible_moves(red, OldBoard, 3, 3, Moves),
    member([[red,0,0],[red,blue,blue],[red,0,0]], Moves),
    member([[red,0,0],[red,blue,blue],[0,red,0]], Moves),
    member([[red,0,0],[red,blue,blue],[0,0,red]], Moves),
    member([[red,red,0],[red,blue,blue],[0,0,0]], Moves),
    member([[red,0,red],[red,blue,blue],[0,0,0]], Moves).
    

:- end_tests(min_max_ai).