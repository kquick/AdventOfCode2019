#!/usr/bin/env bash
echo -n "Puzzle1: "
nix-instantiate --eval -E '(import ./fuel1.nix { inpfile = ../../inputs/day1/input; })'
echo -n "Puzzle2: "
nix-instantiate --eval -E '(import ./fuel2.nix { inpfile = ../../inputs/day1/input; })'
