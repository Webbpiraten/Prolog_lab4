% Othello
% X: a,b,c,d,e,f,g,h.
% Y: 1,2,3,4,5,6,7,8.
% Black (b), White (w).
% Board: [(White, d, 4),(Black, e, 4),(Black, d, 5),(White, e, 5)]

% Grafisk representation.
printCoord(_,h,8):- true.
printCoord(Board,X,Y):-
	X \= h,
	Y \= 8,
	nextY(Ycord,_),
	nextX(Xcord,_),
	% if-then-else. Finns det någon färg i den rutan?
	(member((Color, Xcord, Ycord),Board) ->  writeColor(Color, Xcord) ; writeSpace(Color,Xcord)),
	writeNewLine(Xcord),
	printCoord(Board, Xcord, Ycord).
	
writeColor(Color,Xcord):-
	color(Color,X),
	write(' '),	write(X), write(' ').
	
% Om det inte finns någon färg där, rita ut en tom ruta.
writeSpace(Color, Xcord):-
	write(' _ ').

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

% Alternativ till printCoord.
writeTable([]).
writeTable([(Color, X, Y)|List]):-
	write('('),
	write((Color,X,Y)),
	write(')'),
	writeTable(List).
	
% ------------------------------- LEGALMOVE -------------------------------
legalmove(Color, Board, X, Y):-
	% Kollar ifall giveTiles returnerar en tom lista -> Draget är inte tillåtet.
	%isEmpty(Board,X,Y),!,
	%giveTiles(Color,Board,X,Y,List),
	%(List \= [] -> true ; false).
	member(X, [a,b,c,d,e,f,g,h]),
	member(Y, [1,2,3,4,5,6,7,8]),
	not(member((_,X,Y),Board)),
	nextX(X,X1),
	nextY(Y,Y1),
	checkfirst(Board, X, Y).
	%(
		%xAxisEast(Color, Board, X, Y, List1),!,isLast2(Color,List1,A1);
		%xAxisWest(Color, Board, X, Y, List2),!,isLast2(Color,List2,A2).
		%yAxisSouth(Color, Board, X, Y, List3),!,isLast2(Color,List3,A3);
		%yAxisNorth(Color, Board, X, Y, List4),!,isLast2(Color,List4,A4);
		
		%diagonalUpEast(Color, Board, X, Y, List5),!,isLast2(Color,List5,A5);
		%diagonalDownEast(Color, Board, X, Y, List6),!,isLast2(Color,List6,A6);
		%diagonalUpWest(Color, Board, X, Y, List7),!,isLast2(Color,List7,A7);
		%diagonalDownWest(Color, Board, X, Y, List8),!,isLast2(Color,List8,A8).
	%).


xEastFst(Color, X, Y, Board):- invColor(Color, Col), member((Col,X,Y),Board),!, nextX(X,X1), xEastSnd(Color, X1, Y, Board).
xEastSnd(Color, X, Y, Board):- (member((Color,X,Y),Board) -> true ; nextX(X,X1), X1 \= i, xEastSnd(Color, X1, Y, Board)). 

xWestFst(Color, X, Y, Board):- invColor(Color, Col), member((Col,X,Y),Board),!, nextX(X1,X), xWestSnd(Color, X1, Y, Board).
xWestSnd(Color, X, Y, Board):- (member((Color,X,Y),Board) -> true ; nextX(X1,X), xWestSnd(Color, X1, Y, Board)). 

yDownFst(Color, X, Y, Board):- invColor(Color, Col), member((Col,X,Y),Board),!, nextY(Y,Y1), yDownSnd(Color, X, Y1, Board).
yDownSnd(Color, X, Y, Board):- (member((Color,X,Y),Board) -> true ; nextY(Y,Y1), Y1 \= 9, yDownSnd(Color, X, Y1, Board)). 

yUpFst(Color, X, Y, Board):- invColor(Color, Col), member((Col,X,Y),Board),!, nextY(Y1,Y), yUpSnd(Color, X, Y1, Board).
yUpSnd(Color, X, Y, Board):- (member((Color,X,Y),Board) -> true ; nextY(Y1,Y), yUpSnd(Color, X, Y1, Board)). 
	
%checkfirst(white,Board, X, Y):- 
%	member((white, X, Y), Board),
%	nextX(X,X1),
%	nextY(Y,Y1),
%	checksecond(black, Board, X1, Y1).
	
%checkfirst(black,Board, X, Y):- 
%	member((black, X, Y), Board),
%	nextX(X,X1),
%	nextY(Y,Y1),
%	checksecond(black, Board, X1, Y1).

%checksecond(white, Board, X, Y):-
%	member((white, X, Y), Board),
%	nextX(X,X1),
%	nextY(Y,Y1),
%	checksecond(black, Board, X1, Y1).
	
%checksecond(black, Board, X, Y):-
%	member((white, X, Y), Board),
%	nextX(X,X1),
%	nextY(Y,Y1),
%	checksecond(white, Board, X1, Y1).

% ------

%not(member((_,X,Y),Board),
%nextX
%nextY
%checkfirst

%checkfirst:-
%member((white,X,Y)),
%nextX
%nextY
%checksecond

%checksecond
%member(black,X;Y)

%checksecond
%member(white,X;Y)
%nextX
%nextY
%checksecond
	
isLast2(Color, List):-
	(last(List,(Color,_,_)) -> true ; false).

% Skapar en lista med positioner med motståndarens färg, som ska sedan flippas.
giveTiles(Color, Board, X, Y, List):-
	% Ger en lista med alla lösningar. Vi vill åt den sista.
	isEmpty(Board,X,Y),!,
	(xAxisEast(Color, Board, X, Y, M1) -> M1=M1 ; M1=[]),
	(xAxisWest(Color, Board, X, Y, M2) -> M2=M2 ; M2=[]),
	(yAxisSouth(Color, Board, X, Y, M3) -> M3=M3 ; M3=[]),
	(yAxisNorth(Color, Board, X, Y, M4) -> M4=M4 ; M4=[]),
	(diagonalUpEast(Color, Board, X, Y, M5) -> M5=M5 ; M5=[]),
	(diagonalDownEast(Color, Board, X, Y, M6) -> M6=M6 ; M6=[]),
	(diagonalUpWest(Color, Board, X, Y, M7) -> M7=M7 ; M7=[]),
	(diagonalDownWest(Color, Board, X, Y, M8) -> M8=M8 ; M8=[]),
	
	%setof(List,xAxisEast(Color, Board, X, Y, List), L1),
	%last(L1,M1),
	%setof(List,xAxisWest(Color, Board, X, Y, List), L2),
	%last(L2,M2),
	
	%setof(List,yAxisSouth(Color, Board, X, Y, List), L3),
	%last(L3,M3),
	%setof(List,yAxisNorth(Color, Board, X, Y, List), L4),
	%last(L4,M4),
	
	%setof(List, diagonalUpEast(Color, Board, X, Y, List),L5),
	%last(L5,M5),
	%setof(List, diagonalDownEast(Color, Board, X, Y, List),L6),
	%last(L6,M6),

	%setof(List, diagonalUpWest(Color, Board, X, Y, List),L7),
	%last(L7,M7),
	%setof(List, diagonalDownWest(Color, Board, X, Y, List),L8),
	%last(L8,M8),
	
	isLast(Color, M1, N1),
	isLast(Color, M2, N2),
	isLast(Color, M3, N3),
	isLast(Color, M4, N4),
	
	isLast(Color, M5, N5),
	isLast(Color, M6, N6),
	isLast(Color, M7, N7),
	isLast(Color, M8, N8),
	
	append(N1,N2,List1),
	append(N3,N4,List2),
	append(N5,N6,List3),
	append(N7,N8,List4),
	
	append(List1,List2,A1),
	append(List3,List4,B2),
	append(A1,B2,EndList),
	(EndList \= [] -> List = EndList ; false).
	%printCoord(List, a,1).

% Är det sista elementet av samma färg?
% Om ja -> ta bort det sista och skicka vidare listan.
% Om nej -> skicka en tom lista.
% Detta kommer användas längre ner då vi ska flippa färgen.
isLast(Color, List, List2):-
	(last(List,(Color,X,Y)) -> delete(List,(Color,X,Y),List3),List2 = List3 ; List2 = []).
	
isEmpty(Board, X,Y):-
	not(member((_,X,Y),Board)),!.

% Är rutan av motsatt färg? -> Gå till nästa ruta. Annars return.
xAxisEast(_,_,h,_,[]).
xAxisEast(Color, Board, X, Y, L):- 
	X \= h,
	nextX(X, X1),
	invColor(Color, Col),
	% Kollar om platsen till höger inte är tom.
	not(isEmpty(Board, X1, Y)),
	% Om den till höger är rätt färg, fortsätt. Om inte lägger vi till den sista i listan.
	(member((Col,X1,Y),Board) -> xAxisEast(Color, Board, X1, Y, L2), L = [(Col,X1,Y)|L2]; 
								  xAxisEast(Color, Board, h, Y, L2), L = [(Color,X1,Y)|L2] ).
		
xAxisWest(_,_,a,_,[]).
xAxisWest(Color, Board, X, Y, L):- 
	X \= a,
	nextX(X1, X),
	invColor(Color, Col),
	%member((Col, X1, Y),Board),
	%xAxisWest(Color, Board, X1, Y, L).
	not(isEmpty(Board, X1, Y)),
	(member((Col,X1,Y),Board) -> xAxisWest(Color, Board, X1, Y, L2), L = [(Col,X1,Y)|L2]; 
								  xAxisWest(Color, Board, a, Y, L2), L = [(Color,X1,Y)|L2] ).

yAxisSouth(_,_,_,8,[]).
yAxisSouth(Color, Board, X, Y, L):-
	Y \= 8,
	nextY(Y,Y1),
	invColor(Color, Col),
	%member((Col, X, Y1), Board),
	%yAxisSouth(Color,Board, X, Y1, L).
	not(isEmpty(Board, X, Y1)),
	(member((Col,X,Y1),Board) -> yAxisSouth(Color, Board, X, Y1, L2), L = [(Col,X,Y1)|L2]; 
								  yAxisSouth(Color, Board, X, 8, L2), L = [(Color,X,Y1)|L2] ).
	
yAxisNorth(_,_,_,1,[]).
yAxisNorth(Color, Board, X, Y, L):-
	Y \= 1,
	nextY(Y1,Y),
	invColor(Color, Col),
	%member((Col, X, Y1), Board),
	%yAxisNorth(Color,Board, X, Y1, L).
	not(isEmpty(Board, X, Y1)),
	(member((Col,X,Y1),Board) -> yAxisNorth(Color, Board, X, Y1, L2), L = [(Col,X,Y1)|L2]; 
								  yAxisNorth(Color, Board, X, 1, L2), L = [(Color,X,Y1)|L2] ).

diagonalUpEast(_,_,h,8,[]).
diagonalUpEast(Color, Board, X, Y,L):-
	X \= h,
	Y \= 8,
	nextX(X,X1),
	nextY(Y1,Y),
	invColor(Color,Col),
	%member((Col,X1,Y1),Board),
	%diagonalUpEast(Color, Board, X1, Y1, L).
	not(isEmpty(Board, X1, Y1)),
	(member((Col,X1,Y1),Board) -> diagonalUpEast(Color, Board, X1, Y1, L2), L = [(Col,X1,Y1)|L2]; 
								  diagonalUpEast(Color, Board, h, 8, L2), L = [(Color,X1,Y1)|L2] ).

diagonalDownEast(_,_,h,8,[]).%:- write('Hej!').
diagonalDownEast(Color, Board, X, Y,L):-%[(Col,X1,Y1)|L]):-
	X \= h,
	Y \= 8,
	nextX(X,X1),
	nextY(Y,Y1),
	invColor(Color,Col),
	%member((Col,X1,Y1),Board),
	%diagonalDownEast(Color, Board, X1, Y1, L).
	not(isEmpty(Board, X1, Y1)),
	(member((Col,X1,Y1),Board) -> diagonalDownEast(Color, Board, X1, Y1, L2), L = [(Col,X1,Y1)|L2]; 
								  diagonalDownEast(Color, Board, h, 8, L2), L = [(Color,X1,Y1)|L2] ).
	
diagonalUpWest(_,_,a,1,[]).
diagonalUpWest(Color, Board, X, Y,L):-
	X \= a,
	Y \= 1,
	nextX(X1,X),
	nextY(Y1,Y),
	invColor(Color,Col),
	%member((Col,X1,Y1),Board),
	%diagonalUpWest(Color, Board, X1, Y1, L).
	not(isEmpty(Board, X1, Y1)),
	(member((Col,X1,Y1),Board) -> diagonalUpWest(Color, Board, X1, Y1, L2), L = [(Col,X1,Y1)|L2]; 
								  diagonalUpWest(Color, Board, a, 1, L2), L = [(Color,X1,Y1)|L2] ).
	
diagonalDownWest(_,_,a,1,[]).
% Returnerar en lista med (Color, X, Y).
diagonalDownWest(Color, Board, X, Y,L):-
	X \= a,
	Y \= 1,
	nextX(X1,X),
	nextY(Y,Y1),
	invColor(Color,Col),
	%member((Col,X1,Y1),Board),
	%diagonalDownWest(Color, Board, X1, Y1, L).
	not(isEmpty(Board, X1, Y1)),
	(member((Col,X1,Y1),Board) -> diagonalDownWest(Color, Board, X1, Y1, L2), L = [(Col,X1,Y1)|L2]; 
								  diagonalDownWest(Color, Board, a, 1, L2), L = [(Color,X1,Y1)|L2] ).	
% ------------------------------- /LEGALMOVE/ -------------------------------

% ------------------------------- MAKEMOVE -------------------------------
% Start bräde: makemove(black, [(white, d, 4),(black, e, 4),(black, d, 5),(white, e, 5)],d,3,L).
% makemove(white, [(black,d,3),(black,d,4),(black,e,4),(black,d,5),(white,e,5),(black,d,4),(black,c,2),(black,b,3),(black,c,4),(black,d,2),(black,b,2),(black,b,4),(white,a,5),(white,e,3)],c,3,L).
% makemove(black, [(white,c,4),(white,d,4),(white, e,4),(white, c,5),(white,e,5),(white,c,6),(white,d,6),(white,e,6),(black,b,3),(black,c,3),(black,d,3),(black,e,3),(black, f,3),(black,b,4),(black,f,4),(black,b,5),(black,f,5),(black,b,6),(black,f,6),(black, b,7),(black,c,7),(black,d,7),(black,e,7),(black,f,7)],d,5,L).

% Tar listan från legalMove och flippar färgerna.
% Ifall legalMove failar, låt andra spelaren göra sitt move.

% Fixa så att legalmove kan även ge ut alla möjliga drag. Dvs kan skicka in obundna X och Y.

makemove(Color, Board, X, Y, NewBoard):-
	% Fixa legalmove signaturen så den inte ger ut en lista?
	write('\n'),
	write(' BEFORE '),
	%write('\n'),
	%write(' A  B  C  D  E  F  G  H'),
	write('\n'),
	printCoord(Board, a, 1),
	write('\n'),
	write(' AFTER '),
	write('\n'),
	%legalmove(Color, Board, X, Y),!,
	% brickorna kallas Stones!
	giveTiles(Color, Board, X, Y, List),!,
	flipTable(Board, List, InvertBoard), % Flippar färg på brickorna.
	append(InvertBoard, [(Color, X, Y)], NewBoard1),
	printCoord(NewBoard1, a, 1),!.

% (╯°□°)╯︵ ┻━┻ (°□° )
flipTable(L1,[],L):- L = L1.
flipTable(Board, [(Color,X,Y)|List], NewBoard):-
	invertColor((Color,X,Y),Col),
	% Söker upp (Color, X,Y) och byter ut den mot Col.
	select((Color,X,Y), Board, Col, L),!,
	flipTable(L, List, NewBoard).
	
invertColor((white,X,Y),(black,X,Y)).
invertColor((black,X,Y),(white,X,Y)).
	
% Color+, Board+, N+, -Moves, -NewBoard
% kör makeMove N antal gånger.
% Skicka in Obundna X och Y värden och skicka dem till makemove.
makemoves(Color, Board, N, Moves, NewBoard):-
	N>0,
	makemove(Color, Board, X, Y, NewBoard2),
	N1 is N-1,
	makemoves(Color, NewBoard2, N1, Moves, NewBoard).

% ------------------------------- /MAKEMOVE/ -------------------------------
	
% -Value
valueOf(Color, Board, Value).

% N = antal moves framåt.
findBestMove(Color, Board, N, X, Y).
