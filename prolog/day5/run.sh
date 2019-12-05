#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day5, Part1: "
swipl -g 'test, diagnostic("../../inputs/day5/input", 1, Result), print(Result).' -t halt intcode.pl
echo

echo -n "Day5, Part2: "
swipl -g 'test, diagnostic("../../inputs/day5/input", 5, Result), print(Result).' -t halt intcode.pl
echo

