:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

% ----------------------------------------------------------------------
% File reading and parsing

read_positions(Positions, FName) :-
    open(FName, read, FStrm, []),
    read_line_to_string(FStrm, Line),
    parse_then_next(FStrm, Line, io, Positions).

parse_then_next(_FStrm, end_of_file, _Moon, []) :- !.
parse_then_next(FStrm, Line, Moon, [Position|Positions]) :-
    parse_position(Line, Moon, Position),
    read_line_to_string(FStrm, NextLine),
    next_moon(Moon, NextMoon),
    parse_then_next(FStrm, NextLine, NextMoon, Positions).

parse_position(Line, Moon, Position) :-
    string_codes(Line, Codes),
    phrase( sequence( ("<", blanks), string, (",", blanks), (blanks, ">"),
                      PosVars),
            Codes),
    maplist(varset, PosVars, PosVarVals),
    position_from_posvars(PosVarVals, Moon, Position).

varset(PosStr, posvar(PosVar, PosVal)) :-
    phrase( (string(PosVar), "=", number(PosVal)), PosStr).

position_from_posvars(PosVarVals, Moon, Position) :-
    foldl(posvarset, PosVarVals, pos(Moon,0,0,0,0), Position).

posvarset(posvar(`x`, PosVal), pos(Moon,0,_,OldY,OldZ), pos(Moon,0,PosVal,OldY,OldZ)).
posvarset(posvar(`y`, PosVal), pos(Moon,0,OldX,_,OldZ), pos(Moon,0,OldX,PosVal,OldZ)).
posvarset(posvar(`z`, PosVal), pos(Moon,0,OldX,OldY,_), pos(Moon,0,OldX,OldY,PosVal)).

% ----------------------------------------------------------------------
% Base facts about the moons

is_moon(io).
is_moon(europa).
is_moon(ganymede).
is_moon(callisto).

next_moon(io, europa).
next_moon(europa, ganymede).
next_moon(ganymede, callisto).
next_moon(callisto, bad_moon).  % Needed to be able to process the last input file line

% ----------------------------------------------------------------------
% Gravitational computations to update position and velocity in steps

:- dynamic pos/5.

% The calculate_pos may be performed repeatedly for each step, and as
% the steps increase this causes exponential growth.  Using get_pos as
% a front-end will cache computed results in the database via the
% assertz.
get_pos(Step, Moon, pos(Moon, Step, X, Y, Z)) :- pos(Moon, Step, X, Y, Z), !.
get_pos(Step, Moon, pos(Moon, Step, X, Y, Z)) :-
    calculate_pos(Step, Moon, pos(Moon, Step, X, Y, Z)),
    assertz(pos(Moon, Step, X, Y, Z)),
    !.

get_vel(Step, Moon, Vel) :- calculate_vel(Step, Moon, Vel).
    
calculate_pos(0, Moon, pos(Moon,0,X,Y,Z)) :- pos(Moon,0,X,Y,Z), !.
calculate_pos(StepNum, Moon, pos(Moon, StepNum, NewX, NewY, NewZ)) :-
    get_vel(StepNum, Moon, vel(Moon, StepNum, VX, VY, VZ)),
    succ(PrevStep, StepNum),
    get_pos(PrevStep, Moon, pos(Moon, PrevStep, X, Y, Z)),
    plus(X, VX, NewX),
    plus(Y, VY, NewY),
    plus(Z, VZ, NewZ), !.

calculate_vel(0, Moon, vel(Moon,0,0,0,0)) :- pos(Moon,0,_,_,_), !.
calculate_vel(StepNum, Moon, Vel) :-
    succ(PrevStep, StepNum),
    get_pos(PrevStep, Moon, PrevPos),
    gravity(PrevPos, Vel), !.

gravity(pos(Moon, Step, X, Y, Z), vel(Moon, NextStep, NVX, NVY, NVZ)) :-
    findall(DeltaV, (is_moon(OtherMoon),  % includes Moon, but that's
                                          % OK because DeltaV against
                                          % itself is always zero
                     get_pos(Step, OtherMoon, OtherPos),
                     gravity_from_moon(X,Y,Z, OtherPos, DeltaV)),
            DeltaVS),
    get_vel(Step, Moon, vel(Moon, Step, VX, VY, VZ)),
    succ(Step, NextStep),
    foldl(add_deltav, DeltaVS, v(VX, VY, VZ), v(NVX, NVY, NVZ)),
    !.

gravity_from_moon(X,Y,Z, pos(_, _, XO,YO,ZO), deltav(XDUnit, YDUnit, ZDUnit)) :-
    XDelta is XO - X,
    YDelta is YO - Y,
    ZDelta is ZO - Z,
    unit_change(XDelta, XDUnit),
    unit_change(YDelta, YDUnit),
    unit_change(ZDelta, ZDUnit), !.

% ----------------------------------------------------------------------
% Support functions

unit_change(0, 0) :- true, !.
unit_change(Delta, UnitDelta) :- UnitDelta is Delta / abs(Delta).

add_deltav(deltav(DVX, DVY, DVZ), v(VX, VY, VZ), v(NVX, NVY, NVZ)) :-
    plus(DVX, VX, NVX),
    plus(DVY, VY, NVY),
    plus(DVZ, VZ, NVZ).

% ----------------------------------------------------------------------
% Energy calculations

total_energy(Step, Energy) :-
    findall((P,V), (is_moon(Moon), get_pos(Step, Moon, P), get_vel(Step, Moon, V)), PVS),
    maplist(get_energy, PVS, ES),
    foldl(plus, ES, 0, Energy).

get_energy((P,V), E) :-
    pE(P, PE),
    kE(V, KE),
    E is PE * KE.

pE(pos(_,_,X,Y,Z), PE) :- PE is abs(X) + abs(Y) + abs(Z).

kE(vel(_,_,X,Y,Z), KE) :- KE is abs(X) + abs(Y) + abs(Z).

% ----------------------------------------------------------------------

test0 :- read_positions(Positions, '../../inputs/day12/testinp1'),
         retractall(pos(_,_,_,_,_)),
         maplist(assertz, Positions),
         get_pos(0, io,       pos(io,       0, -1,   0,  2)),
         get_pos(0, europa,   pos(europa,   0,  2, -10, -7)),
         get_pos(0, ganymede, pos(ganymede, 0,  4,  -8,  8)),
         get_pos(0, callisto, pos(callisto, 0,  3,   5, -1)),
         get_vel(0, io,       vel(io,       0,  0,   0,  0)),
         get_vel(0, europa,   vel(europa,   0,  0,   0,  0)),
         get_vel(0, ganymede, vel(ganymede, 0,  0,   0,  0)),
         get_vel(0, callisto, vel(callisto, 0,  0,   0,  0)).
test1 :- read_positions(Positions, '../../inputs/day12/testinp1'),
         retractall(pos(_,_,_,_,_)),
         maplist(assertz, Positions),
         get_vel(1, io,       vel(io,       1,  3,  -1, -1)),
         get_pos(1, io,       pos(io,       1,  2,  -1,  1)),
         get_vel(1, europa,   vel(europa,   1,  1,   3,  3)),
         get_pos(1, europa,   pos(europa,   1,  3,  -7, -4)),
         get_vel(1, ganymede, vel(ganymede, 1, -3,   1, -3)),
         get_pos(1, ganymede, pos(ganymede, 1,  1,  -7,  5)),
         get_vel(1, callisto, vel(callisto, 1, -1,  -3,  1)),
         get_pos(1, callisto, pos(callisto, 1,  2,   2,  0)).
test3 :- read_positions(Positions, '../../inputs/day12/testinp1'),
         retractall(pos(_,_,_,_,_)),
         maplist(assertz, Positions),
         get_vel(3, io,       vel(io,       3,  0,  -3,  0)),
         get_pos(3, io,       pos(io,       3,  5,  -6, -1)),
         get_vel(3, europa,   vel(europa,   3, -1,   2,  4)),
         get_pos(3, europa,   pos(europa,   3,  0,   0,  6)),
         get_vel(3, ganymede, vel(ganymede, 3,  1,   5, -4)),
         get_pos(3, ganymede, pos(ganymede, 3,  2,   1, -5)),
         get_vel(3, callisto, vel(callisto, 3,  0,  -4,  0)),
         get_pos(3, callisto, pos(callisto, 3,  1,  -8,  2)).
test6 :- read_positions(Positions, '../../inputs/day12/testinp1'),
         retractall(pos(_,_,_,_,_)),
         maplist(assertz, Positions),
         get_vel(6, io,       vel(io,       6,  0,   2,  1)),
         get_pos(6, io,       pos(io,       6, -1,  -7,  3)),
         get_vel(6, europa,   vel(europa,   6, -1,  -1, -5)),
         get_pos(6, europa,   pos(europa,   6,  3,   0,  0)),
         get_vel(6, ganymede, vel(ganymede, 6,  1,  -4,  5)),
         get_pos(6, ganymede, pos(ganymede, 6,  3,  -2,  1)),
         get_vel(6, callisto, vel(callisto, 6,  0,   3, -1)),
         get_pos(6, callisto, pos(callisto, 6,  3,  -4, -2)).
test7 :- read_positions(Positions, '../../inputs/day12/testinp1'),
         retractall(pos(_,_,_,_,_)),
         maplist(assertz, Positions),
         get_vel(7, io,       vel(io,       7,  3,   5, -2)),
         get_pos(7, io,       pos(io,       7,  2,  -2,  1)),
         get_vel(7, europa,   vel(europa,   7, -2,  -4, -4)),
         get_pos(7, europa,   pos(europa,   7,  1,  -4, -4)),
         get_vel(7, ganymede, vel(ganymede, 7,  0,  -5,  4)),
         get_pos(7, ganymede, pos(ganymede, 7,  3,  -7,  5)),
         get_vel(7, callisto, vel(callisto, 7, -1,   4,  2)),
         get_pos(7, callisto, pos(callisto, 7,  2,   0,  0)).

test10 :- read_positions(Positions, '../../inputs/day12/testinp1'),
          retractall(pos(_,_,_,_,_)),
          maplist(assertz, Positions),
          get_vel(10, io,       vel(io,       10, -3,  -2,  1)),
          get_pos(10, io,       pos(io,       10,  2,   1, -3)),
          get_vel(10, europa,   vel(europa,   10, -1,   1,  3)),
          get_pos(10, europa,   pos(europa,   10,  1,  -8,  0)),
          get_vel(10, ganymede, vel(ganymede, 10,  3,   2, -3)),
          get_pos(10, ganymede, pos(ganymede, 10,  3,  -6,  1)),
          get_vel(10, callisto, vel(callisto, 10,  1,  -1, -1)),
          get_pos(10, callisto, pos(callisto, 10,  2,   0,  4)).

test10e :- read_positions(Positions, '../../inputs/day12/testinp1'),
           retractall(pos(_,_,_,_,_)),
           maplist(assertz, Positions),
           total_energy(10, 179).
          
test100 :- energy_at('../../inputs/day12/testinp2', 100, 1940).

test :- test0, test1, test3, test6, test7, test10, test10e, test100.

% ----------------------------------------------------------------------

energy_at(FName, Step, Energy) :-
    read_positions(Positions, FName),
    retractall(pos(_,_,_,_,_)),
    maplist(assertz, Positions),
    total_energy(Step, Energy).
