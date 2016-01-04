%prolog

map([], _, []).

map([InHead|InTail], Pred, [OutHead|OutTail]) :-
  call(Pred, InHead, OutHead),
  map(InTail, Pred, OutTail).

% TODO: can be optimizized not to use append
filter_try_to_append(Pred, Acc, Elem, NewAcc) :-
  call(Pred, Elem),
  !, append(Acc, [Elem], NewAcc).

filter_try_to_append(_, Acc, _, Acc).

filter_util([], _, L, L).

filter_util([InHead|InTail], Pred, Acc, Output) :-
  filter_try_to_append(Pred, Acc, InHead, NewAcc),
  filter_util(InTail, Pred, NewAcc, Output).
   
filter(In, Pred, Out) :-
  filter_util(In, Pred, [], Out).

reduce([], _, Acc, Acc).

reduce([InHead|InTail], Pred, Acc, OutValue) :-
  call(Pred, Acc, InHead, Result),
  reduce(InTail, Pred, Result, OutValue).

min(A, [], A).
min([], B, B).
min(A, B, A) :-
  A < B, !.
min(_, B, B).

max(A, [], A).
max([], B, B).
max(A, B, A) :-
  A > B, !.
max(_, B, B).

minlist([], Current, Current).

minlist([H|T], Current, Result) :-
  min(H, Current, M),
  minlist(T, M, Result).

maxlist([], Current, Current).

maxlist([H|T], Current, Result) :-
  max(H, Current, M),
  maxlist(T, M, Result).
