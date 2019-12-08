#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day6, Part1: "
swipl -g 'test, count_all_orbits("../../inputs/day6/input", Result), print(Result).' -t halt orbits.pl
echo

echo -n "Day6, Part2: "
swipl -g 'test, count_transfers("../../inputs/day6/input", "YOU", "SAN", Result), print(Result).' -t halt orbits.pl
echo

