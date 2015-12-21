%prolog

:- consult(adventofcode).

day2_exec(Pred, TotalValue) :-
  open('day2_input.txt', read, File),
  do_something(Pred, File, 0, TotalValue).

do_something(_, File, TotalValue, TotalValue) :-
  at_end_of_stream(File).

do_something(Pred, File, Acc, TotalValue) :-
  read_line_to_string(File, S),
  day2_0_parse(S, Dimension), 
  call(Pred, Dimension, Value),
  Sum is Acc + Value,
  do_something(Pred, File, Sum, TotalValue).
