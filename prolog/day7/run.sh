#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day7, Part1: "
swipl -g 'test, thrusters("../../inputs/day7/input", [0,1,2,3,4], Result), print(Result).' -t halt intcode.pl
echo

echo -n "Day7, Part2: "
swipl -g 'test, feed_thrusters("../../inputs/day7/input", [5,6,7,8,9], Result), print(Result).' -t halt intcode.pl
echo

