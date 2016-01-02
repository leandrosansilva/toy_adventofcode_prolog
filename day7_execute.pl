%prolog

:- consult(adventofcode).

day7_exec(Value) :-
  open('day7_input.txt', read, File),
  read_file_to_instructions(File, Instructions),
  day7_0_compute_wire_value(Instructions, 'a', Value).

read_file_to_instructions(File, Instructions) :-
  read_file_to_instructions_util(File, [], Instructions).
  
read_file_to_instructions_util(File, Instructions, Instructions) :-
  at_end_of_stream(File).

read_file_to_instructions_util(File, Acc, Instructions) :-
  read_line_to_string(File, S),
  day7_0_parse(S, Instruction),
  read_file_to_instructions_util(File, [Instruction|Acc], Instructions).
