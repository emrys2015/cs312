%Name: Emrys Yang, Xuan Li.

operator(add).
operator(sub).
operator(mul).
operator(div).
operator(exp).
operator(sqrt).
%operator(log).
%operator(fac).

%fac(0,1).
%fac(N,F):- N>0,N1 is N-1,fac(N1,F1),F is N*F1.

cal(add, A, B, C) :- C = A + B.
cal(sub, A, B, C) :- C = A - B.
cal(mul, A, B, C) :- C = A * B.
cal(div, A, B, C) :- B1 is B, B1 \= 0, C = A / B.
cal(exp, A, B, C) :- A1 is A, B1 is B, B1 > 0, A1+B1 < 15, C = B^A.
cal(sqrt, A, B, C) :- A1 is A, A1 >0, B1 is B, B1 = 2, C = sqrt(A).

%cal(fac, A, B, C) :- A>0, A1 is A-1, C = fac(A1,B1,C), B is N*A1.

%cal(fac, A, B, C) :- C = fac(A, B).

% toexp(L: List, Prog: List, R: Int, Expr)
toexp([X], [], X).
toexp(L, [H|T], R) :-
	integer(H),
	toexp([H|L], T, R).

toexp([X,Y|Z], [H|T], R) :-
	operator(H),
	cal(H, Y, X, X1),
	toexp([X1|Z], T, R).

eval(Prog, Target) :-
	toexp([], Prog, R),
	Target is R.

% Use Reverse Polish Notation to enumerate all possible expression (5
% types)

mytry([A, B, C, D], [A, B, X, C, Y, D, Z], N) :- eval([A, B, X, C, Y, D, Z], N).
mytry([A, B, C, D], [A, B, X, C, D, Y, Z], N) :- eval([A, B, X, C, D, Y, Z], N).
mytry([A, B, C, D], [A, B, C, X, D, Y, Z], N) :- eval([A, B, C, X, D, Y, Z], N).
mytry([A, B, C, D], [A, B, C, X, Y, D, Z], N) :- eval([A, B, C, X, Y, D, Z], N).
mytry([A, B, C, D], [A, B, C, D, X, Y, Z], N) :- eval([A, B, C, D, X, Y, Z], N).

solve24(N, Prog) :- mytry(N, Prog, 24).
solve24(N, Prog) :- mytry(N, Prog, 24.0).

math24(N, Prog) :-
	lists:perm(N, T1),
	solve24(T1, Prog),
	toexp([], Prog, R1),
	write(R1),
	nl,
	fail.

% Try:
% ?- math24([4,2,5,3], X).
% ?- math24([4,9,2,9], X).
% ?- math24([3,4,6,2], X).
% ?- math24([1,2,4,5], X).
% ?- math24([5,5,5,1], X).
% ?- math24([4,4,10,10], X).
