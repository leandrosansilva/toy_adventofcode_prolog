%prolog

:- consult(adventofcode).

day8_exec(TotalValue) :-
  open('day8_input.txt', read, File),
  do_something(File, [0, 0], TotalValue).

do_something(File, TotalValue, TotalValue) :-
  at_end_of_stream(File).

do_something(File, [Day8_0, Day8_1], TotalValue) :-
  read_line_to_string(File, S),
  string_length(S, LineLength),
  day8_0_data_length(S, DataLength),
  Day8_0_Diff is LineLength - DataLength,
  NewDay8_0 is Day8_0 + Day8_0_Diff,
  day8_1_encoded_string_length(S, EncodedLength),
  Day8_1_Diff is EncodedLength - LineLength,
  NewDay8_1 is Day8_1 + Day8_1_Diff,
  %write("Line: "), write(S), write("Diff: "), write(Diff), nl,
  do_something(File, [NewDay8_0, NewDay8_1], TotalValue).
