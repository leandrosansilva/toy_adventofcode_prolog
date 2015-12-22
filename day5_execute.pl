%prolog

:- consult(adventofcode).

day5_exec(TotalValue) :-
  open('day5_input.txt', read, File),
  do_something(File, 0, TotalValue).

do_something(File, TotalValue, TotalValue) :-
  at_end_of_stream(File).

do_something(File, Acc, TotalValue) :-
  read_line_to_string(File, S),
  day5_0(S),
  Next is Acc + 1,
  do_something(File, Next, TotalValue).

do_something(File, Acc, TotalValue) :-
  do_something(File, Acc, TotalValue).
