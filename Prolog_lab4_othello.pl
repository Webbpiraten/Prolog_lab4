% Othello
% X: a,b,c,d,e,f,g,h.
% Y: 1,2,3,4,5,6,7,8.
% Black (b), White (w).


legalMove(Color, Board, X, Y).
	

makeMove(Color, Board, X, Y, NewBoard).

% -Moves, -NewBoard
makeMoves(Color, Board, N, Moves, NewBoard).

% -Value
valueOf(Color, Board, Value).

% N = antal moves framåt.
findBestMove(Color, Board, N, X, Y).
