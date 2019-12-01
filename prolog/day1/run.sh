#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

echo -n "Puzzle1: "
swipl -g 'test, fuel_needed("input", Fuel), print(Fuel).' -t halt fuel.pl
echo

swipl -g 'test, fuel_needed("input", Fuel), format('\''Puzzle2: ~d~n'\'', [Fuel]).' -t halt fuel2.pl

