file_masses(F, Masses) :-
    open(F, read, FStrm, []),
    read_line_to_string(FStrm, Line),
    mass_line(FStrm, Line, Masses).

mass_line(_, end_of_file, []) :- !.
mass_line(FStrm, Line, [M|MS]) :-
    number_string(M, Line),
    read_line_to_string(FStrm, NewLine),
    mass_line(FStrm, NewLine, MS).

read_masses(mass(M), FName) :- file_masses(FName, Masses), member(M, Masses).

test() :- fuel_needed("../../inputs/day1/testinp", Fuel), Fuel == 51316.

fuel(0, mass(0)) :- !.
fuel(F, mass(M)) :-
    MF is max(0, (M // 3) - 2),
    fuel(FF, mass(MF)),
    F is MF + FF.

total_fuel(Fuel) :-
    findall(F, (mass(M), fuel(F, mass(M))), FS),
    sum_list(FS, Fuel).

fuel_needed(FName, Fuel) :-
    findall(F, (read_masses(mass(M), FName), fuel(F, mass(M))), FS),
    sum_list(FS, Fuel).

