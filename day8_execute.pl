%prolog

:- consult(adventofcode).

day8_exec(TotalValue) :-
  open('day8_input.txt', read, File),
  do_something(File, 0, TotalValue).

do_something(File, TotalValue, TotalValue) :-
  at_end_of_stream(File).

do_something(File, Acc, TotalValue) :-
  read_line_to_string(File, S),
  string_length(S, LineLength),
  day8_0_data_length(S, DataLength),
  Diff is LineLength - DataLength,
  NewAcc is Acc + Diff,
  %write("Line: "), write(S), write("Diff: "), write(Diff), nl,
  do_something(File, NewAcc, TotalValue).
