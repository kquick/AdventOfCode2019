#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day10, Part1: "
swipl -g 'test, best_base("../../inputs/day10/input", Asteroid, Result), print(Result).' -t halt visibility.pl
echo

echo -n "Day10, Part2: "
swipl -g 'test, vaporized("../../inputs/day10/input", Asteroid, 200, Result), print(Result).' -t halt visibility.pl
echo

