% Othello
% X: a,b,c,d,e,f,g,h.
% Y: 1,2,3,4,5,6,7,8.
% Black (b), White (w).

% Board: [(White, d, 4),(Black, e, 4),(Black, d, 5),(White, e, 5)]
% printBoard([(white,a,1),(black,b,1),(white,c,1),(black,d,1),(white,e,1),(black,f,1),(white,g,1),(black, h,1)]).

% Grafisk representation.
%printBoard(_,0).
printBoard(Board):- % [(C,X,Y)|Board]
	printCoord(Board). %;
	%write(' '),
	%printBoard(Board).
	
% Finns A,1 i listan? Om nej, tom ruta. Annars rita ut den.
printCoord(Board):-
	nextY(Ycord,_),
	nextX(Xcord,_),
	%member((Color, Xcord, Ycord),Board),
	%col(Color,ColRev),
	%write(ColRev).
	
	% if-then-else.
	(member((Color, Xcord, Ycord),Board) ->  writeColor(Color, Xcord) ; writeSpace(Color,Xcord)),
	writeNewLine(Xcord).
	
	
%Måste veta när det är H så vi kan göra en radbrytning.
writeColor(Color,Xcord):-
	col(Color,X),
	write(' '),	write(X), write(' ').%,
	%writeNewLine(Xcord).
	
writeSpace(Color, Xcord):-
	write(' _ ').%,
	%writeNewLine(Xcord).
	
writeNewLine(Xcord):-
	Xcord = h,
	write('\n').
	
col(white, w).
col(black, b).

nextX(a,b).
nextX(b,c).
nextX(c,d).
nextX(d,e).
nextX(e,f).
nextX(f,g).
nextX(g,h).
nextX(h,i).

nextY(1,2).
nextY(2,3).
nextY(3,4).
nextY(4,5).
nextY(5,6).
nextY(6,7).
nextY(7,8).
nextY(8,9).

invColor(white, black).
invColor(black, white).

% ------------------------------- LEGALMOVE -------------------------------

% legalMove: list med alla som kan flippas.
legalMove(Color, Board, X, Y, List):-
	%findall(L1, xAxis(Color, Board, X, Y, L1), L), %,
	
	%setof(List,xAxisEast(Color, Board, X, Y, List), L1),
	% ta sista i L1 och sätt den till L4.
	%last(L1,L3),
	
	%setof(List,xAxisWest(Color, Board, X, Y, List), L2),
	%last(L2,L4),
	
	%append(L3,L4,List),
	
	%setof(List,yAxisSouth(Color, Board, X, Y, List), L1),
	%last(L1,L2).%,
		
	%setof(List,yAxisNorth(Color, Board, X, Y, List), L1),
	%last(L1,L2),
	
	%setof(List, diagonalUpEast(Color, Board, X, Y, List),L1),
	%last(L1,List),
	
	%setof(List, diagonalDownEast(Color, Board, X, Y, List),L1),
	%last(L1,List),
	
	%setof(List, diagonalUpWest(Color, Board, X, Y, List),L1),
	%last(L1,List),
	
	%setof(List, diagonalDownWest(Color, Board, X, Y, List),L1),
	%last(L1,List),
	
	printBoard(Board).
	
% Skapa en lista med positioner med motståndarens färg, som ska sedan flippas.
% är rutan av motsatt färg? -> Gå till nästa ruta. Annars return.
xAxisEast(_,_,_,_,[]).
xAxisEast(Color, Board, X, Y, [(X1, Y)|L]):- 
	X \= h,
	nextX(X, X1),
	% vill kolla motsatt färg, tills jag hittar min färg. alla mellan ska flippas.
	invColor(Color, Col),
	% Finns det en där, spara den till en lista.
	member((Col, X1, Y),Board),
	xAxisEast(Color, Board, X1, Y, L).
	
xAxisWest(_,_,_,_,[]).
xAxisWest(Color, Board, X, Y, [(X1, Y)|L]):- 
	X \= a,
	nextX(X1, X),
	% vill kolla motsatt färg, tills jag hittar min färg. alla mellan ska flippas.
	invColor(Color, Col),
	% Finns det en där, spara den till en lista.
	member((Col, X1, Y),Board),
	xAxisWest(Color, Board, X1, Y, L).

yAxisSouth(_,_,_,_,[]).
yAxisSouth(Color, Board, X, Y, [(X,Y1)|L]):-
	Y \= 8,
	nextY(Y,Y1),
	invColor(Color, Col),
	member((Col, X, Y1), Board),
	yAxisSouth(Color,Board, X, Y1, L).
	
yAxisNorth(_,_,_,_,[]).
yAxisNorth(Color, Board, X, Y, [(X,Y1)|L]):-
	Y \= 1,
	nextY(Y1,Y),
	invColor(Color, Col),
	member((Col, X, Y1), Board),
	yAxisNorth(Color,Board, X, Y1, L).

diagonalUpEast(_,_,_,_,[]).
diagonalUpEast(Color, Board, X, Y,[(X1,Y1)|L]):-
	X \= h,
	Y \= 8,
	nextX(X,X1),
	nextY(Y1,Y),
	invColor(Color,Col),
	member((Col,X1,Y1),Board),
	diagonalUpEast(Color, Board, X1, Y1, L).
	
diagonalDownEast(_,_,_,_,[]).
diagonalDownEast(Color, Board, X, Y,[(X1,Y1)|L]):-
	X \= h,
	Y \= 8,
	nextX(X,X1),
	nextY(Y,Y1),
	invColor(Color,Col),
	member((Col,X1,Y1),Board),
	diagonalUpEast(Color, Board, X1, Y1, L).
	
diagonalUpWest(_,_,_,_,[]).
diagonalUpWest(Color, Board, X, Y,[(X1,Y1)|L]):-
	X \= a,
	Y \= 1,
	nextX(X1,X),
	nextY(Y1,Y),
	invColor(Color,Col),
	member((Col,X1,Y1),Board),
	diagonalUpEast(Color, Board, X1, Y1, L).
	
diagonalDownWest(_,_,_,_,[]).
diagonalDownWest(Color, Board, X, Y,[(X1,Y1)|L]):-
	X \= a,
	Y \= 1,
	nextX(X1,X),
	nextY(Y,Y1),
	invColor(Color,Col),
	member((Col,X1,Y1),Board),
	diagonalUpEast(Color, Board, X1, Y1, L).
	
% ------------------------------- /LEGALMOVE/ -------------------------------
	
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
