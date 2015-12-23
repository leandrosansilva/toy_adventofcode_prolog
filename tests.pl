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

test('day five') :-
  day5_0("ugknbfddgicrmopn"),
  day5_0("aaa"),
  not(day5_0("jchzalrnumimnmhp")),
  not(day5_0("haegwjzuvuyypxyu")),
  not(day5_0("dvszwmarrgswjxmb")).

test('parse day 6 instruction') :-
  day6_0_parse_instruction("turn on 0,0 through 999,999", 
    instruction('turn on', pos(0, 0), pos(999, 999))
  ).

:- end_tests(adventofcode_tests).
