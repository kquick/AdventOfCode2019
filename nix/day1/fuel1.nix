{ inpfile ? /home/kquick/Projects/advoc019/prolog/day1/fuelfile.txt
}:

let
  masses = import ./intsFromFile.nix { inherit inpfile; };

  each_fuel = mass: mass / 3 - 2;

  sum = vals: builtins.foldl' (x: y: x+y) 0 vals;

  total_fuel = sum (map each_fuel masses);

in total_fuel
