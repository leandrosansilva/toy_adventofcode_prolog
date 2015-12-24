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
  day6_get_rect_positions(rect(pos(0, 0), pos(0, 0)), Rect),
  member(pos(0, 0), Rect).

test('turn on 2x3 rect light on day 6') :-
  day6_get_rect_positions(rect(pos(1, 2), pos(2, 4)), Rect),
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

:- end_tests(adventofcode_tests).
