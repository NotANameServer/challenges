solution(S) :- S is random(1000000) + 1.

comp(X, S, -1) :- S < X.
comp(X, S, 1) :- S > X.
comp(X, S, 0) :- S = X.

verify(X, C) :- solution(S), comp(X, S, C).

guess(X) :- verify(X, 0).
