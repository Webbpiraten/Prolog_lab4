% Othello
% X: a,b,c,d,e,f,g,h.
% Y: 1,2,3,4,5,6,7,8.
% Black (b), White (w).

% Board: [(White, d, 4),(Black, e, 4),(Black, d, 5),(White, e, 5)]
% printCoord([(white,a,1),(black,b,1),(white,c,1),(black,d,1),(white,e,1),(black,f,1),(white,g,1),(black, h,1)]).
% printCoord([(white,c,4),(white,d,4),(white, e,4),(white, c,5),(white,e,5),(white,c,6),(white,d,6),(white,e,6),(black,b,3),(black,c,3),(black,d,3),(black,e,3),(black, f,3),(black,b,4),(black,f,4),(black,b,5),(black,f,5),(black,b,6),(black,f,6),(black, b,7),(black,c,7),(black,d,7),(black,e,7),(black,f,7)],a,1).


% Grafisk representation.
%printBoard(_,0).
%printBoard(Board):- % [(C,X,Y)|Board]
%	printCoord(Board).
	%printBoard(Board).
	
% Finns A,1 i listan? Om nej, tom ruta. Annars rita ut den.
%printCoord([],_,_).
printCoord(Board,X,Y):-
	X \= h,
	Y \= 8,
	nextY(Ycord,_),
	nextX(Xcord,_),
	% if-then-else. Finns det någon färg i den rutan?
	(member((Color, Xcord, Ycord),Board) ->  writeColor(Color, Xcord) ; writeSpace(Color,Xcord)),
	writeNewLine(Xcord),
	printCoord(Board, Xcord, Ycord).
	
% Ritar ut färgen.
writeColor(Color,Xcord):-
	color(Color,X),
	write(' '),	write(X), write(' ').%,
	%writeNewLine(Xcord).
	
% Om det inte finns någon färg där, rita ut en tom ruta.
writeSpace(Color, Xcord):-
	write(' _ ').%,
	%writeNewLine(Xcord).

% Måste veta när det är H så vi kan göra en radbrytning.
writeNewLine(Xcord):-
	Xcord == h,
	write('\n').
	
color(white, w).
color(black, b).

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
% Skapa en lista med positioner med motståndarens färg, som ska sedan flippas.
legalmove(Color, Board, X, Y, List):-
	% findall(L1, xAxis(Color, Board, X, Y, L1), L), %,
	% Ger en lista med alla lösningar, där den sista innehåller allting.
	% Tar sista från setof (L1) och sätt den till M1.
	setof(List,xAxisEast(Color, Board, X, Y, List), L1),
	last(L1,M1),
	setof(List,xAxisWest(Color, Board, X, Y, List), L2),
	last(L2,M2),
	append(M1,M2,List1),
	
	setof(List,yAxisSouth(Color, Board, X, Y, List), L3),
	last(L3,M3),
	setof(List,yAxisNorth(Color, Board, X, Y, List), L4),
	last(L4,M4),
	append(M3,M4,List2),
	
	setof(List, diagonalUpEast(Color, Board, X, Y, List),L5),
	last(L5,M5),
	setof(List, diagonalDownEast(Color, Board, X, Y, List),L6),
	last(L6,M6),
	append(M5,M6,List3),
	
	setof(List, diagonalUpWest(Color, Board, X, Y, List),L7),
	last(L7,M7),
	setof(List, diagonalDownWest(Color, Board, X, Y, List),L8),
	last(L8,M8),
	append(M7,M8,List4),
	
	append(List1,List2,N1),
	append(List3,List4,N2),
	append(N1,N2,List).
	
	printCoord(Board, a, 1).
	

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
makemove(Color, Board, X, Y, NewBoard):-
	% Fixa legalmove signaturen så den inte ger ut en lista.
	legalmove(Color, Board, X, Y, NewBoard).

% -Moves, -NewBoard
% kör makeMove N antal gånger.
makemoves(Color, Board, N, Moves, NewBoard):-
	N>0,
	makemove(Color, Board, X, Y, NewBoard2),
	N1 is N-1,
	makemoves(Color, NewBoard2, N1, Moves, NewBoard).

% -Value
valueOf(Color, Board, Value).

% N = antal moves framåt.
findBestMove(Color, Board, N, X, Y).
