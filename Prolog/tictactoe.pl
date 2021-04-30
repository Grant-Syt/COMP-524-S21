ordered_line(1,2,3).
ordered_line(4,5,6).
ordered_line(7,8,9).
ordered_line(1,4,7).
ordered_line(2,5,8).
ordered_line(3,6,9).
ordered_line(1,5,9).
ordered_line(3,5,7).

line(A,B,C) :- ordered_line(A,B,C).
line(A,B,C) :- ordered_line(A,C,B).
line(A,B,C) :- ordered_line(B,A,C).
line(A,B,C) :- ordered_line(B,C,A).
line(A,B,C) :- ordered_line(C,A,B).
line(A,B,C) :- ordered_line(C,B,A).

full(A) :- x(A).
full(A) :- o(A).
empty(A) :- \+ full(A).

same(A,A).
different(A,B) :- \+ same(A,B).

all_full :- full(1), full(2), full(3),
            full(4), full(5), full(6),
            full(7), full(8), full(9).
done :- ordered_line(A,B,C), x(A), x(B), x(C),
        write('I won.'), nl, halt.
done :- ordered_line(A,B,C), o(A), o(B), o(C),
        write('You won.'), nl, halt.
done :- all_full, write('Draw.'), nl, halt.

printsquare(N) :- o(N), write(' o ').
printsquare(N) :- x(N), write(' x ').
printsquare(N) :- empty(N), write('   ').
printboard :- printsquare(1), printsquare(2), printsquare(3), nl,
              printsquare(4), printsquare(5), printsquare(6), nl,
              printsquare(7), printsquare(8), printsquare(9), nl.

play :- repeat, getmove, makemove, printboard, done.

getmove :- repeat, write('Please enter a move: '), read(X),
           empty(X), assert(o(X)).

:- dynamic(o/1).
:- dynamic(x/1).

makemove :- move(X), assert(x(X)).
makemove :- all_full.

move(A) :- good(A), empty(A), !.

% computer player strategy; order is important!
good(A) :- win(A).                  % first, try to win
good(A) :- block_win(A).            % second, block opponent's win
good(A) :- fork(A).                 % third, fork the opponent
good(A) :- block_fork(A).           % fourth, block an opponent's fork
good(A) :- build(A).                % fifth, build towards a line
good(5).                            % sixth, take the middle
good(1). good(3). good(7). good(9). % seventh, take the corners
good(2). good(4). good(6). good(8). % lastly, take the edges

win(A) :- x(B), x(C), different(B,C), line(A,B,C).

block_win(A) :- o(B), o(C), different(B,C), line(A,B,C).

fork(A) :- x(B), x(C), different(B,C),
           line(A,B,D), line(A,C,E),
           empty(D), empty(E).

block_fork(A) :- o(B), o(C), different(B,C),
                 line(A,B,D), line(A,C,E),
                 empty(D), empty(E).

build(A) :- x(B), line(A,B,C), empty(C).