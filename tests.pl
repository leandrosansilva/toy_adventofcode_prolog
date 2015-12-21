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
  day4_0("pqrstuv", 1048970).

:- end_tests(adventofcode_tests).
