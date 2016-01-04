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

day6_get_rect_positions_sorted(Rect, Positions) :-
  day6_get_rect_positions(Rect, PositionsList),
  list_to_ord_set(PositionsList, Positions).

day6_get_rect_positions(Rect, Positions) :-
  day6_rect_properties(Rect, Prop),
  day6_get_rect_positions_sorted_util(Prop, 0, Positions).

day6_rect_properties(rect(pos(X, Y), pos(EX, EY)), prop(X, Y, W, Length)) :-
  W is EX - X + 1,
  H is EY - Y + 1,
  Length is H * W.

day6_get_rect_positions_sorted_util(Prop, Length, []) :-
  prop(_, _, _, Length) = Prop, !.

day6_get_rect_positions_sorted_util(Prop, Count, [Pos|T]) :-
  prop(BX, BY, W, _) = Prop, !,
  X is BX + (Count mod W),
  Y is BY + (Count div W),
  pos(X, Y) = Pos,
  NextCount is Count + 1,
  day6_get_rect_positions_sorted_util(Prop, NextCount, T).

day6_0_turn_on_lights(CurrentLights, Rect, NewLights) :-
  day6_get_rect_positions_sorted(Rect, LightsInTurn),
  ord_union(CurrentLights, LightsInTurn, NewLights).

day6_0_turn_off_lights(CurrentLights, Rect, NewLights) :-
  day6_get_rect_positions_sorted(Rect, LightsOff),
  ord_subtract(CurrentLights, LightsOff, NewLights).

day6_0_toggle_lights(CurrentLights, Rect, NewLights) :-
  day6_get_rect_positions_sorted(Rect, LightsToToggle),
  ord_symdiff(CurrentLights, LightsToToggle, NewLights).

day6_0_execute_instruction('turn on', CurrentLights, Rect, NewLights) :-
  day6_0_turn_on_lights(CurrentLights, Rect, NewLights).
  
day6_0_execute_instruction('turn off', CurrentLights, Rect, NewLights) :-
  day6_0_turn_off_lights(CurrentLights, Rect, NewLights).

day6_0_execute_instruction('toggle', CurrentLights, Rect, NewLights) :-
  day6_0_toggle_lights(CurrentLights, Rect, NewLights).

day6_1_inc_counter_by_1(Count, NextCount) :-
  NextCount is Count + 1.

day6_1_operate_on_brightness(_, CurrentLights, [], CurrentLights).

day6_1_operate_on_brightness(CounterPred, CurrentLights, LightsToChange, NewLights) :-
  day6_1_get_updated_lights_and_current(CounterPred, LightsToChange, CurrentLights, [], UpdatedLights, [],
  LightsToExclude),
  sort(UpdatedLights, SortedUpdatedLights),
  sort(LightsToExclude, SortedLightsToExclude),
  ord_subtract(CurrentLights, SortedLightsToExclude, CurrentLightsWithoutUpdated),
  ord_union(SortedUpdatedLights, CurrentLightsWithoutUpdated, NewLights).

day6_1_get_updated_lights_and_current(_, [], _, UpdatedLights, UpdatedLights, LightsToExclude, LightsToExclude).

day6_1_get_updated_lights_and_current(CounterPred, [H|T], CurrentLights, Acc, UpdatedLights, AccExclude, LightsToExecute) :-
  pos(X, Y) = H,
  day6_1_replace_or_add_position_applying_pred(CounterPred, CurrentLights, X, Y, NewPos, OldPos),
  day6_1_get_updated_lights_and_current(CounterPred, T, CurrentLights, [NewPos|Acc], UpdatedLights, [OldPos|AccExclude], LightsToExecute).

day6_1_replace_or_add_position_applying_pred(CounterPred, CurrentLights, X, Y, NewPos, OldPos) :-
  OldPos = pos(X, Y, Count),
  day6_1_search_position(OldPos, CurrentLights),
  !, call(CounterPred, Count, NewCount),
  NewPos = pos(X, Y, NewCount).

day6_1_replace_or_add_position_applying_pred(CounterPred, _, X, Y, NewPos, pos(X, Y, 0)) :- 
  call(CounterPred, 0, Count),
  NewPos = pos(X, Y, Count).

day6_1_search_position(Pos, CurrentLights) :- 
  % FIXME: implement a binary search here!!!!
  memberchk(Pos, CurrentLights).

day6_1_execute_instruction('turn on', CurrentLights, Rect, NewLights) :-
  day6_1_turn_on_lights(CurrentLights, Rect, NewLights).

day6_1_execute_instruction('turn off', CurrentLights, Rect, NewLights) :-
 day6_1_turn_off_lights(CurrentLights, Rect, NewLights).

day6_1_execute_instruction('toggle', CurrentLights, Rect, NewLights) :-
 day6_1_toggle_lights(CurrentLights, Rect, NewLights).

day6_1_dec_counter_by_1(Count, NextCount) :-
  NextCount is Count - 1.

day6_1_turn_off_lights(CurrentLights, Rect, NewLights) :-
  day6_get_rect_positions(Rect, LightsInTurn),
  day6_1_operate_on_brightness(day6_1_dec_counter_by_1, CurrentLights, LightsInTurn, NewLights).

day6_1_count_total_brightness(Lights, TotalBrightness) :-
  reduce(Lights, day6_1_position_brightness, 0, TotalBrightness).

day6_1_turn_on_lights(CurrentLights, Rect, NewLights) :-
  day6_get_rect_positions(Rect, LightsInTurn),
  day6_1_operate_on_brightness(day6_1_inc_counter_by_1, CurrentLights, LightsInTurn, NewLights).

day6_1_toggle_lights(CurrentLights, Rect, NewLights) :-
  day6_get_rect_positions(Rect, LightsInTurn),
  day6_1_operate_on_brightness(day6_1_inc_counter_by_2, CurrentLights, LightsInTurn, NewLights).

day6_1_inc_counter_by_2(Count, NextCount) :-
  NextCount is Count + 2.

day6_1_position_brightness(Acc, pos(_, _, Count), Total) :-
  Total is Acc + Count.

% TODO: refactor this parse to remove code repetition
day7_0_parse(S, input(Wire, Value)) :-
  regex('^(\\w+) -> (\\w+)$', [], S, [ValueCodes, WireCodes]),
  atom_codes(Value, ValueCodes),
  atom_codes(Wire, WireCodes).

day7_0_parse(S, and(Output, [InputA, InputB])) :-
  regex('^(\\w+) AND (\\w+) -> (\\w+)$', [], S, [ACodes, BCodes, OutCodes]),
  atom_codes(Output, OutCodes),
  atom_codes(InputA, ACodes),
  atom_codes(InputB, BCodes).

day7_0_parse(S, or(Output, [InputA, InputB])) :-
  regex('^(\\w+) OR (\\w+) -> (\\w+)$', [], S, [ACodes, BCodes, OutCodes]),
  atom_codes(Output, OutCodes),
  atom_codes(InputA, ACodes),
  atom_codes(InputB, BCodes).

day7_0_parse(S, lshift(Output, Input, Shift)) :-
  regex('^(\\w+) LSHIFT (\\d+) -> (\\w+)$', [], S, [InputCodes, ShiftCodes, OutCodes]),
  atom_codes(Output, OutCodes),
  atom_codes(Input, InputCodes),
  number_codes(Shift, ShiftCodes).

day7_0_parse(S, rshift(Output, Input, Shift)) :-
  regex('^(\\w+) RSHIFT (\\d+) -> (\\w+)$', [], S, [InputCodes, ShiftCodes, OutCodes]),
  atom_codes(Output, OutCodes),
  atom_codes(Input, InputCodes),
  number_codes(Shift, ShiftCodes).

day7_0_parse(S, not(Output, Input)) :-
  regex('^NOT (\\w+) -> (\\w+)$', [], S, [InputCodes, OutCodes]),
  atom_codes(Output, OutCodes),
  atom_codes(Input, InputCodes).

day7_0_instructions(Instructions, Instructions).

:- dynamic day7_0_compute_wire_value/3.

day7_0_compute_wire_value(Instructions, Gate, Value) :-
  day7_0_compute_wire_value_util(Instructions, Gate, Value),
  asserta(day7_0_compute_wire_value(Instructions, Gate, Value)).

% Raw numeric input
day7_0_compute_wire_value_util(_, Gate, Value) :-
  atom_number(Gate, Value).

% input
day7_0_compute_wire_value_util(Instructions, Gate, Value) :-
  member(input(Gate, GateInput), Instructions), !,
  day7_0_compute_wire_value(Instructions, GateInput, Value).

% not
day7_0_compute_wire_value_util(Instructions, Gate, Value) :-
  member(not(Gate, NegGate), Instructions), !,
  day7_0_compute_wire_value(Instructions, NegGate, NegValue),
  Value is 65535 - NegValue.

% and
day7_0_compute_wire_value_util(Instructions, Gate, Value) :-
  member(and(Gate, [Comp1, Comp2]), Instructions), !,
  day7_0_compute_wire_value(Instructions, Comp1, Comp1Value),
  day7_0_compute_wire_value(Instructions, Comp2, Comp2Value),
  Value is Comp1Value /\ Comp2Value.

% or
day7_0_compute_wire_value_util(Instructions, Gate, Value) :-
  member(or(Gate, [Comp1, Comp2]), Instructions), !,
  day7_0_compute_wire_value(Instructions, Comp1, Comp1Value),
  day7_0_compute_wire_value(Instructions, Comp2, Comp2Value),
  Value is Comp1Value \/ Comp2Value.

% lshift
day7_0_compute_wire_value_util(Instructions, Gate, Value) :-
  member(lshift(Gate, Input, Shift), Instructions), !,
  day7_0_compute_wire_value(Instructions, Input, InputValue),
  Value is InputValue << Shift.

% rshift
day7_0_compute_wire_value_util(Instructions, Gate, Value) :-
  member(rshift(Gate, Input, Shift), Instructions), !,
  day7_0_compute_wire_value(Instructions, Input, InputValue),
  Value is InputValue >> Shift.

% day 8.0 implements a small state machine
day8_0_data_length(S, L) :-
  sub_string(S, 1, _, 1, Payload), 
  string_codes(Payload, Codes),
  reduce(Codes, day8_0_data_length_pred, [0, normal], [L, normal]).

% 92 is ascii('\')
day8_0_data_length_pred([Length, normal], 92, [Length, escaped]) :- !.

% 120 is ascii('x')
day8_0_data_length_pred([Length, escaped], 120, [Length, hexa1]).
day8_0_data_length_pred([Length, hexa1], _, [Length, hexa2]).
day8_0_data_length_pred([Length, hexa2], _, [NewLength, normal]) :-
  NewLength is Length + 1.

day8_0_data_length_pred([Length, _], _, [NewLength, normal]) :-
  NewLength is Length + 1.

day8_1_encoded_string_length(Input, Length) :-
  string_codes(Input, Codes),
  reduce(Codes, day8_1_encode_pred, 0, PayloadLength),
  Length is PayloadLength + 2.

% 92 is ascii('\')
% 34 is ascii('"')
day8_1_encode_pred(Length, Char, NewLength) :-
  member(Char, [92, 34]),
  !, NewLength is Length + 2.

day8_1_encode_pred(Length, _, NewLength) :-
  NewLength is Length + 1.

day9_0_parse(S, distance(From, To, Distance)) :-
  regex("^(\\w+) to (\\w+) = (\\d+)$", [], S, [FromCodes, ToCodes, DistanceCodes]),
  atom_codes(From, FromCodes),
  atom_codes(To, ToCodes),
  number_codes(Distance, DistanceCodes).

day9_0_get_possible_routes(Distances, Routes) :-
  day9_0_cities(Distances, Cities),
  findall(Perm, permutation(Cities, Perm), Routes).

day9_0_cities(Distances, Cities) :-
  day9_0_cities_util(Distances, [], L),
  sort(L, Cities).

day9_0_cities_util([], L, L).

day9_0_cities_util([distance(From, To, _)|T], Acc, L) :-
  day9_0_cities_util(T, [From, To|Acc], L).

day9_0_path_length(Distances, Path, Length) :-
  day9_0_path_length_util(Distances, Path, 0, Length).

day9_0_path_length_util(_, [_], Length, Length).

day9_0_path_length_util(Distances, [From,To|Tail], Acc, Length) :-
  (
    member(distance(From, To, Distance), Distances), !;
    member(distance(To, From, Distance), Distances)
  ),
  NewAcc is Acc + Distance,
  day9_0_path_length_util(Distances, [To|Tail], NewAcc, Length).

day9_0_shortest_and_longest_paths(Distances, Shortest, Longest) :-
  day9_0_get_possible_routes(Distances, Routes),
  findall([Distances, Path], member(Path, Routes), PathPairLists),
  map(PathPairLists, day9_0_get_length, Lengths),
  minlist(Lengths, [], Shortest),
  maxlist(Lengths, [], Longest).

day9_0_get_length([Distances, Path], Length) :-
  day9_0_path_length(Distances, Path, Length).
