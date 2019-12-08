:- dynamic orbit/2.

make_orbit(Center, Orbiter) :- assertz(orbit(Center, Orbiter)).

read_orbits(FName) :-
    open(FName, read, FStrm, []),
    read_line_to_string(FStrm, Line),
    add_orbit(FStrm, Line).

add_orbit(_, end_of_file) :- !.
add_orbit(FStrm, Line) :-
    split_string(Line, ")", "", OrbitSeq),
    apply(make_orbit, OrbitSeq),
    read_line_to_string(FStrm, NewLine),
    add_orbit(FStrm, NewLine).

indirect_orbit(A,B) :- orbit(A, C), (orbit(C, B); indirect_orbit(C, B)).
any_orbit(A,B) :- orbit(A, B) ; indirect_orbit(A, B).

% ----------------------------------------------------------------------

transfers_to_common(A, B, 0) :- orbit(C, A), orbit(C, B), !.
transfers_to_common(A, B, Count) :- transfers_to_common(l, A, B, Count).

transfers_to_common(l, A, B, Count) :-
    orbit(C, A),
    orbit(_, C),
    ((transfers_to_common(r, C, B, Cnt), !); transfers_to_common(l, C, B, Cnt)),
    succ(Cnt, Count).

transfers_to_common(r, A, B, 0) :- orbit(C, A), orbit(C, B), !.
transfers_to_common(r, A, B, Count) :-
    orbit(C, B),
    orbit(_, C),
    transfers_to_common(r, A, C, Cnt),
    succ(Cnt, Count).


% ----------------------------------------------------------------------

count_all_orbits(FName, Count) :-
    retractall(orbit(_,_)),
    read_orbits(FName),
    findall(O, any_orbit(O, _), OS),
    length(OS, Count).

count_transfers(FName, Src, Dest, Count) :-
    retractall(orbit(_,_)),
    read_orbits(FName),
    transfers_to_common(Src, Dest, Count).
    
% ----------------------------------------------------------------------

test1 :-
    retractall(orbit(_,_)),
    read_orbits("../../inputs/day6/testinp"),
    findall(O, orbit(O, "D"), OS),
    length(OS, 1).

test2 :-
    retractall(orbit(_,_)),
    read_orbits("../../inputs/day6/testinp"),
    findall(O, indirect_orbit(O, "D"), OS),
    length(OS, 2).

test3 :-
    retractall(orbit(_,_)),
    read_orbits("../../inputs/day6/testinp"),
    findall(O, any_orbit(O, "D"), OS),
    length(OS, 3).

test4 :-
    retractall(orbit(_,_)),
    read_orbits("../../inputs/day6/testinp"),
    findall(O, any_orbit(O, "L"), OS),
    length(OS, 7).

test5 :-
    retractall(orbit(_,_)),
    read_orbits("../../inputs/day6/testinp"),
    findall(O, any_orbit(O, "COM"), OS),
    length(OS, 0).

test6 :-
    count_all_orbits("../../inputs/day6/testinp", 42).

test7 :-
    count_transfers("../../inputs/day6/testinp2", "YOU", "SAN", 4).
    

test :- test1, test2, test3, test4, test5, test6.
