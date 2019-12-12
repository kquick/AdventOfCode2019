#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day11, Part1: "
swipl -g 'test, paint_panel_count("../../inputs/day11/input", Result), print(Result).' -t halt intcode.pl
echo

echo "Day11, Part2: "
swipl -g 'test, paint_panels("../../inputs/day11/input").' -t halt intcode.pl
echo

