#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day3, Part1: "
swipl -g 'testd, closest_from_file("../../inputs/day3/input", Closest), print(Closest).' -t halt intmh.pl
echo

echo -n "Day2, Part2: "
swipl -g 'testl, shortest_from_file("../../inputs/day3/input", Shortest), print(Shortest).' -t halt intmh.pl
echo

