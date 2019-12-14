#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day12, Part1: "
swipl -g 'test, energy_at("../../inputs/day12/input", 1000, Result), print(Result).' -t halt moons.pl
echo

echo "Day12, Part2: "
echo swipl -g 'test, paint_panels("../../inputs/day12/input").' -t halt moons.pl
echo

