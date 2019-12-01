{ inpfile ? /home/kquick/Projects/advoc019/prolog/day1/fuelfile.txt
}:

let
  masses = import ./intsFromFile.nix { inherit inpfile; };

  each_fuel = mass: let mf = mass / 3 - 2;
                     in if mf < 0 then 0 else mf + each_fuel mf;

  sum = vals: builtins.foldl' (x: y: x+y) 0 vals;

  total_fuel = sum (map each_fuel masses);

in total_fuel
