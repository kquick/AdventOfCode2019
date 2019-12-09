#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day8, Part1: "
swipl -g 'test, validate_sif("../../inputs/day8/input", 25, 6, Result), print(Result).' -t halt sif.pl
echo

echo "Day8, Part2: "
swipl -g 'test, render_sif("../../inputs/day8/input", 25, 6).' -t halt sif.pl
echo

