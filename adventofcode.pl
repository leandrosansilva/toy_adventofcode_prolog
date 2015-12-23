%prolog

:- consult(utils).

% ensures regex library is installed
% :- pack_install(regex, [silent(true), interactive(false)]).
:- load_files(library(regex)).
:- use_module(library(semweb/rdf_db)).

day1_char_to_dir(`(`, 1).
day1_char_to_dir(`)`, -1).

day1_0_accumulator(Acc, Char, Result) :-
  day1_char_to_dir([Char], Direction), !, 
  Result is Acc + Direction.

day1_0(Input, Output) :-
  string_codes(Input, Codes),
  reduce(Codes, day1_0_accumulator, 0, Output).

day1_1(Input, Output) :-
  string_codes(Input, Codes),
  day1_1_util(Codes, 0, 0, Output).

day1_1_util(_, Position, -1, Position).

day1_1_util([H|T], Position, Floor, Result) :-
  day1_char_to_dir([H], Direction), !,
  NewFloor is Floor + Direction,
  NextPosition is Position + 1,
  day1_1_util(T, NextPosition, NewFloor, Result).

day2_0(dimensions(L, W, H), Surface) :-
  msort([L, W, H], [First, Second | _]),
  Surface is 2*L*W + 2*W*H + 2*H*L + First*Second.

day2_1(dimensions(L, W, H), Surface) :-
  msort([L, W, H], [First, Second | _]),
  Surface is First*2 + Second*2 + L*W*H.

day2_0_parse(Input, dimensions(L, W, H)) :-
  atom_string(Atomized, Input),
  regex('(\\d+)x(\\d+)x(\\d+)', [], Atomized, [SL, SW, SH]),
  number_codes(L, SL),
  number_codes(W, SW),
  number_codes(H, SH).

day3_0(Input, Result) :-
  string_codes(Input, Codes),
  day3_0_count_present_per_house(Codes, pos(0, 0), [], Houses),
  length(Houses, Result).

day3_0_compute_house(pos(X, Y), Acc, NewAcc) :-
  day3_0_get_house(Acc, house(X, Y, Count), AccWithoutHouse),
  NewCount is Count + 1,
  union(AccWithoutHouse, [house(X, Y, NewCount)], NewAcc).
 
day3_0_count_present_per_house([], Pos, Acc, NewAcc) :-
  day3_0_compute_house(Pos, Acc, NewAcc).

day3_0_count_present_per_house([H|T], Pos, Acc, Houses) :-
  day3_0_compute_house(Pos, Acc, NewAcc),
  day3_0_get_next_position(Pos, [H], NextPos),
  day3_0_count_present_per_house(T, NextPos, NewAcc, Houses).

day3_0_get_house(Acc, house(X, Y, Count), AccWithoutHouse) :-
  intersection(Acc, [house(X, Y, _)], Intersection),
  day3_0_get_house_count(Intersection, Count),
  subtract(Acc, Intersection, AccWithoutHouse).

day3_0_get_house_count([], 0).
day3_0_get_house_count([house(_, _, Count)], Count).

day3_0_get_next_position(pos(X, Y), `>`, pos(NextX, Y)) :- NextX is X + 1.
day3_0_get_next_position(pos(X, Y), `<`, pos(NextX, Y)) :- NextX is X - 1.
day3_0_get_next_position(pos(X, Y), `v`, pos(X, NextY)) :- NextY is Y + 1.
day3_0_get_next_position(pos(X, Y), `^`, pos(X, NextY)) :- NextY is Y - 1.

day3_1(Input, Result) :-
  string_codes(Input, Codes),
  day3_1_count_present_per_house(Codes, [pos(0, 0), pos(0, 0)], santa, [house(0, 0, 1)], Houses),
  length(Houses, Result).

day3_1_count_present_per_house([], _, _, Houses, Houses).
  
day3_1_count_present_per_house([H|T], Positions, Turn, Acc, Houses) :-
  day3_1_get_position_and_turn_by_current_turn(Positions, Turn, Position, NextTurn),
  day3_0_get_next_position(Position, [H], NextPos),
  day3_0_compute_house(NextPos, Acc, NewAcc),
  day3_1_get_new_positions(Positions, Turn, NextPos, NewPositions),
  day3_1_count_present_per_house(T, NewPositions, NextTurn, NewAcc, Houses).

day3_1_get_position_and_turn_by_current_turn([SantaPos, _], santa, SantaPos, robot).
day3_1_get_position_and_turn_by_current_turn([_, RobotPos], robot, RobotPos, santa).

day3_1_get_new_positions([_, RobotPos], santa, NextPos, [NextPos, RobotPos]).
day3_1_get_new_positions([SantaPos, _], robot, NextPos, [SantaPos, NextPos]).

day4_0(Input, Result) :-
  day4_0_util(Input, 0, '00000', 5, Result).

day4_1(Input, Result) :-
  day4_0_util(Input, 0, '000000', 6, Result).

day4_0_util(Input, Acc, Padding, PaddingLength, Acc) :-
  number_string(Acc, AccString), 
  string_concat(Input, AccString, Concat),
  % FIXME: rdf_atom_md5 is deprecated
  rdf_atom_md5(Concat, 1, Md5),
  sub_string(Md5, 0, PaddingLength, _, Padding).

day4_0_util(Input, Acc, Padding, PaddingLength, Result) :-
  NewAcc is Acc + 1,
  day4_0_util(Input, NewAcc, Padding, PaddingLength, Result).

% Sorry, Regex support on swi-prolog is very primitive, so the regex are dirty... :-(
day5_0(Input) :-
  Input =~ '(ab|cd|pq|xy)', !, fail.

day5_0(Input) :-
  Input =~ '(aa|bb|cc|dd|ee|ff|gg|hh|ii|jj|kk|ll|mm|nn|oo|pp|qq|rr|ss|tt|uu|vv|ww|xx|yy|zz)',
  Input =~ '[^aeiou]*([aeiou])[^aeiou]*([aeiou])[^aeiou]*([aeiou])'.

day6_0_parse_instruction(Input, instruction(Comm, pos(BX, BY), pos(EX, EY))) :-
  regex('^(.*) (\\d+),(\\d+) through (\\d+),(\\d+)$', [], Input, [CommCodes, SBX, SBY, SEX, SEY]),
  atom_codes(Comm, CommCodes),
  number_codes(BX, SBX), 
  number_codes(BY, SBY), 
  number_codes(EX, SEX), 
  number_codes(EY, SEY).
