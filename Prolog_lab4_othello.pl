% Othello
% X: a,b,c,d,e,f,g,h.
% Y: 1,2,3,4,5,6,7,8.
% Black (b), White (w).


% legalMove: list med alla som kan flippas.
legalMove(Color, Board, X, Y).
	xAxis(Color, Board, X),
	yAxis(Color, Board, Y),
	diagonal(Color, Board, X, Y).
	
	
xAxis(Color, Board, X):-
	

yAxis(Color, Board, Y).
	

diagonal(Color, Board, X, Y).
	

% tar listan från legalMove och flippar, maplist funktion (samma som haskell).
% ifall legalMove failar, låt andra spelaren göra sitt move.
makeMove(Color, Board, X, Y, NewBoard):-
	legalMove(Color, Board, X, Y).

% -Moves, -NewBoard
% kör makeMove N antal gånger.
makeMoves(Color, Board, N, Moves, NewBoard):-
	N>0,
	makeMove(Color, Board, X, Y, NewBoard2),
	N1 is N-1,
	makeMoves(Color, NewBoard2, N1, Moves, NewBoard).

% -Value
valueOf(Color, Board, Value).

% N = antal moves framåt.
findBestMove(Color, Board, N, X, Y).
