%prolog

:- consult(adventofcode).

day6_exec(TotalValue) :-
  open('day6_input.txt', read, File),
  do_something(File, [], 0, Lights),
  day6_1_count_total_brightness(Lights, TotalValue).

do_something(File, LightsOn, _, LightsOn) :-
  at_end_of_stream(File).

do_something(File, Acc, Line, LightsOn) :-
  write(Line), nl,
  read_line_to_string(File, S),
  day6_0_parse_instruction(S, instruction(Command, PosBegin, PosEnd)),
  day6_1_execute_instruction(Command, Acc, rect(PosBegin, PosEnd), NewAcc),
  NextLine is Line + 1,
  do_something(File, NewAcc, NextLine, LightsOn).
