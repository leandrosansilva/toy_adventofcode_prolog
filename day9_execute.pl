%prolog

:- consult(adventofcode).

day9_exec(Shortest, Longest) :-
  open('day9_input.txt', read, File),
  read_distances(File, [], Distances),
  day9_0_shortest_and_longest_paths(Distances, Shortest, Longest).

read_distances(File, Distances, Distances) :-
  at_end_of_stream(File).

read_distances(File, Acc, TotalValue) :-
  read_line_to_string(File, S),
  day9_0_parse(S, Distance),
  read_distances(File, [Distance|Acc] , TotalValue).
