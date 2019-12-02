#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day2, Part1: "
swipl -g 'test, program("../../inputs/day2/input", 12, 2, Result), print(Result).' -t halt intcode.pl
echo

echo -n "Day2, Part2: "
swipl -g 'test, program("../../inputs/day2/input", Noun, Verb, 19690720), Result is (100 * Noun) + Verb, format("Noun=~d, Verb=~d, Result=~d~n", [Noun, Verb, Result]).' -t halt intcode.pl
echo

