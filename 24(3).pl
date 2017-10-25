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

% Definition for operators
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

%Evaluate Expressions, eval(Prog, Target) is true if the value of the prog is equal to the target
eval(Prog, Target) :-
	toexp([], Prog, R),
	Target is R.

% Use Reverse Polish Notation to enumerate all possible expressions (5
% types)

mytry([A, B, C, D], [A, B, X, C, Y, D, Z], N) :- eval([A, B, X, C, Y, D, Z], N).
mytry([A, B, C, D], [A, B, X, C, D, Y, Z], N) :- eval([A, B, X, C, D, Y, Z], N).
mytry([A, B, C, D], [A, B, C, X, D, Y, Z], N) :- eval([A, B, C, X, D, Y, Z], N).
mytry([A, B, C, D], [A, B, C, X, Y, D, Z], N) :- eval([A, B, C, X, Y, D, Z], N).
mytry([A, B, C, D], [A, B, C, D, X, Y, Z], N) :- eval([A, B, C, D, X, Y, Z], N).

% set the target to 24, in case of decimal in div or square root, 24.0 is included.
solve24order(Numbers, Prog) :- mytry(Numbers, Prog, 24).
solve24order(Numbers, Prog) :- mytry(Numbers, Prog, 24.0).

% lists all the results in a set list and write all the elements in that list.
solve24(Numbers, Prog) :-
	lists:perm(Numbers, T1),
	solve24order(T1, Prog),
	toexp([], Prog, E),
	write(E),
	nl,
	fail.

% Try:
% ?- solve24([4,2,5,3], X).
% ?- solve24([4,9,2,9], X).
% ?- solve24([3,4,6,2], X).
% ?- solve24([1,2,4,5], X).
% ?- solve24([5,5,5,1], X).
% ?- solve24([4,4,10,10], X).
