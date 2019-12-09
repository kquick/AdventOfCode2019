#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day9, Part1: "
swipl -g 'test, boost("../../inputs/day9/input", 1, Result), print(Result).' -t halt intcode.pl
echo

echo -n "Day9, Part2: "
swipl -g 'test, boost("../../inputs/day9/input", 2, Result), print(Result).' -t halt intcode.pl
echo

