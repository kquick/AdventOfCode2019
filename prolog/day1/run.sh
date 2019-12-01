#!/usr/bin/env nix-shell
#! nix-shell -i bash -p swiProlog

swipl -g 'test, fuel_needed("input", Fuel), print(Fuel).' -t halt fuel.pl
echo

