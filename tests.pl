% prolog

:- load_files(adventofcode).

:- use_module(library(plunit)).

:- begin_tests(adventofcode_tests).

test(test_day1_0) :- 
  day1_0("(", 1),
  day1_0(")", -1),
  day1_0("(())", 0),
  day1_0(")())())", -3).

test(test_day1_1) :- 
  day1_1(")", 1),
  day1_1("()())", 5).

test(test_day2_0) :-
  day2_0(dimensions(2, 3, 4), 58),
  day2_0(dimensions(1, 1, 10), 43).

test(test_day2_0_parse) :-
  day2_0_parse("2x3x4", dimensions(2, 3, 4)),
  day2_0_parse("1x1x10", dimensions(1, 1, 10)).

test(test_day2_1) :-
  day2_1(dimensions(2, 3, 4), 34),
  day2_1(dimensions(1, 1, 10), 14).

test(test_day3_0) :-
  day3_0(">", 2),
  day3_0("^>v<", 4),
  day3_0("^v^v^v^v^v", 2).

test(test_day3_1) :-
  day3_1("", 1),
  day3_1("^v", 3),
  day3_1("^>v<", 3),
  day3_1("^v^v^v^v^v", 11).

test('day four') :-
  day4_0("abcdef", 609043),
  day4_0("pqrstuv", 1048970),
  day4_1("ckczppom", 3938038).

test('day five is a total garbage!') :-
  day5_0("ugknbfddgicrmopn"),
  day5_0("aaa"),
  not(day5_0("jchzalrnumimnmhp")),
  not(day5_0("haegwjzuvuyypxyu")),
  not(day5_0("dvszwmarrgswjxmb")).

test('parse day 6 instruction') :-
  day6_0_parse_instruction("turn on 0,0 through 999,999", 
    instruction('turn on', pos(0, 0), pos(999, 999))),
  day6_0_parse_instruction("turn off 499,499 through 500,500", 
    instruction('turn off', pos(499, 499), pos(500, 500))),
  day6_0_parse_instruction("toggle 0,0 through 999,0", 
    instruction('toggle', pos(0, 0), pos(999, 0))).

test('get rect for 1 light on day 6') :-
  day6_get_rect_positions_sorted(rect(pos(0, 0), pos(0, 0)), Rect),
  member(pos(0, 0), Rect).

test('turn on 2x3 rect light on day 6') :-
  day6_get_rect_positions_sorted(rect(pos(1, 2), pos(2, 4)), Rect),
  member(pos(1, 2), Rect),!,
  member(pos(1, 3), Rect),!,
  member(pos(1, 4), Rect),!,
  member(pos(2, 2), Rect),!,
  member(pos(2, 3), Rect),!,
  member(pos(2, 4), Rect),!,
  length(Rect, 6), !.

test('turn on initial 2x1 rect') :-
  day6_0_execute_instruction('turn on', [], rect(pos(3, 2), pos(4,2)), NewLights),
  length(NewLights, 2),
  member(pos(3, 2), NewLights), !,
  member(pos(4, 2), NewLights), !.

test('turn on initial 2x1 rect plus a 3x1 rect') :-
  day6_0_execute_instruction('turn on', [], rect(pos(3, 2), pos(4,2)), FirstTurn),
  day6_0_execute_instruction('turn on', FirstTurn, rect(pos(4, 2), pos(4, 4)), SecondtTurn),
  length(SecondtTurn, 4), !,
  member(pos(3, 2), SecondTurn), !,
  member(pos(4, 2), SecondTurn), !,
  member(pos(4, 3), SecondTurn), !,
  member(pos(4, 4), SecondTurn), !.

test('turn on initial 3x3 and turn off two lights') :-
  day6_0_execute_instruction('turn on', [], rect(pos(2, 6), pos(4, 8)), FirstTurn),
  day6_0_execute_instruction('turn off', FirstTurn, rect(pos(3, 7), pos(3, 9)), SecondtTurn),
  length(SecondtTurn, 7), !,
  member(pos(2, 8), SecondTurn), !,
  member(pos(2, 7), SecondTurn), !,
  member(pos(2, 6), SecondTurn), !,
  member(pos(3, 6), SecondTurn), !,
  member(pos(4, 6), SecondTurn), !,
  member(pos(4, 7), SecondTurn), !,
  member(pos(4, 8), SecondTurn), !.

test('toggles vertical 3x1 line by a horizontal 1x3 producing a "crux"') :-
  day6_0_execute_instruction('turn on', [], rect(pos(3, 3), pos(5, 3)), FirstTurn),
  day6_0_execute_instruction('toggle', FirstTurn, rect(pos(4, 2), pos(4, 4)), SecondTurn),
  length(SecondTurn, 4), !,
  member(pos(4, 2), SecondTurn), !,
  member(pos(3, 3), SecondTurn), !,
  member(pos(5, 3), SecondTurn), !,
  member(pos(4, 4), SecondTurn), !.

test('All empty, no brightness') :-
  day6_1_count_total_brightness([], 0).

test('turn on initial 2x1 rect on day 6.2') :-
  day6_1_execute_instruction('turn on', [], rect(pos(3, 2), pos(4,2)), NewLights),
  day6_1_count_total_brightness(NewLights, 2).

test('turn on initial 2x1 rect on day 6.2 twice') :-
  day6_1_execute_instruction('turn on', [], rect(pos(3, 2), pos(4,2)), FirstTurn),
  day6_1_execute_instruction('turn on', FirstTurn, rect(pos(3, 2), pos(4,2)), NewLights),
  day6_1_count_total_brightness(NewLights, 4).

test('turn on initial 2x1 rect on day 6.2 and then turn one off') :-
  day6_1_execute_instruction('turn on', [], rect(pos(3, 2), pos(4,2)), FirstTurn),
  day6_1_execute_instruction('turn off', FirstTurn, rect(pos(3, 2), pos(3,2)), NewLights),
  day6_1_count_total_brightness(NewLights, 1).

test('toggle 0,0 through 2,2 has brightness 18') :-
  day6_1_execute_instruction('toggle', [], rect(pos(0, 0), pos(2, 2)), NewLights),
  day6_1_count_total_brightness(NewLights, 18).

test('day 7 parse input') :-
  day7_0_parse("123 -> x", input('x', 123)),
  day7_0_parse("x AND y -> z", and('z', ['x', 'y'])),
  day7_0_parse("p LSHIFT 2 -> q", lshift('q', 'p', 2)),
  day7_0_parse("y RSHIFT 3 -> g", rshift('g', 'y', 3)),
  day7_0_parse("NOT e -> f", not('f', 'e')),
  day7_0_parse("x OR y -> d", or('d', ['x', 'y'])),
  day7_0_parse("1 AND 123 -> z", and(z, [1, 123])).

test('day 7.1 example circuit') :-
  Input = [
    input('x', '123'), 
    input('y', '456'),
    and('d', ['x', 'y']),
    or('e', ['x', 'y']),
    lshift('f', 'x', 2),
    rshift('g', 'y', 2),
    not('h', 'x'),
    not('i', 'y')
  ],
  day7_0_instructions(Input, Instructions),
  day7_0_compute_wire_value(Instructions, 'x', 123),
  day7_0_compute_wire_value(Instructions, 'y', 456),
  day7_0_compute_wire_value(Instructions, 'd', 72),
  day7_0_compute_wire_value(Instructions, 'e', 507),
  day7_0_compute_wire_value(Instructions, 'f', 492),
  day7_0_compute_wire_value(Instructions, 'g', 114),
  day7_0_compute_wire_value(Instructions, 'h', 65412),
  day7_0_compute_wire_value(Instructions, 'i', 65079).

test('day 7.1 circuit with other values') :-
  Input = [
    and('a', ['123', '456']),
    or('b', ['123', '456']),
    input('c', 'b')
  ],
  day7_0_instructions(Input, Instructions),
  day7_0_compute_wire_value(Instructions, 'a', 72),
  day7_0_compute_wire_value(Instructions, 'b', 507),
  day7_0_compute_wire_value(Instructions, 'c', 507).

test('day 8.0 data length') :-
  day8_0_data_length("\"\"", 0),
  day8_0_data_length("\"\\\\\"", 1),
  day8_0_data_length("\"abc\"", 3),
  day8_0_data_length("\"aaa\\\"aaa\"", 7),
  day8_0_data_length("\"\\x27\"", 1).

test('day 8.1 encoded string length') :-
  day8_1_encoded_string_length("\"\"", 6),
  day8_1_encoded_string_length("\"abc\"", 9),
  day8_1_encoded_string_length("\"aaa\\\"aaa\"", 16),
  day8_1_encoded_string_length("\"\\x27\"", 11).

:- end_tests(adventofcode_tests).
