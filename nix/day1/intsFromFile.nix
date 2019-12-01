{ inpfile ? /home/kquick/Projects/advoc019/prolog/day1/fuelfile.txt
}:

let
  inp = builtins.readFile inpfile;
  inpParts = builtins.split "\n" inp;
  evens = upTo: builtins.genList (x: x * 2) (upTo / 2);
  strToInt = builtins.fromJSON;  # hacky
  intvals = let indices = evens (builtins.length inpParts);
                inpValParts = map (i: builtins.elemAt inpParts i) indices;
            in map strToInt inpValParts;
in intvals
