#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Day4, Part1: "
swipl -g 'test, num_passwords_part1(130254, 678275, Count), print(Count).' -t halt password.pl
echo

echo -n "Day2, Part2: "
swipl -g 'test, num_passwords_part2(130254, 678275, Count), print(Count).' -t halt password.pl
echo

