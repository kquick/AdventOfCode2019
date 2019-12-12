read_map(Map, FName) :-
    open(FName, read, FStrm, []),
    read_line_to_codes(FStrm, Line),
    read_maplines(FStrm, 0, Line, Map).

read_maplines(_, _, end_of_file, []).
read_maplines(FStrm, Y, Line, Map) :-
    decode_line(0, Y, Line, MapLine),
    read_line_to_codes(FStrm, NextLine),
    succ(Y, NextY),
    read_maplines(FStrm, NextY, NextLine, MapLines),
    append(MapLine, MapLines, Map).

% 46 is .
% 35 is #
decode_line(X, Y, [46|CS], MapLine) :-
    succ(X, NextX),
    decode_line(NextX, Y, CS, MapLine).
decode_line(X, Y, [35|CS], [ast(X, Y)|MapLine]) :-
    succ(X, NextX),
    decode_line(NextX, Y, CS, MapLine).
decode_line(_X, _Y, [], []).

:- discontiguous ast/2.

% ----------------------------------------------------------------------

x_loc(ast(X, _Y), X).
y_loc(ast(_X, Y), Y).
at_loc(X, Y, ast(X, Y)).

raw_angle(X, Y, A) :-
    RawA is atan2(Y, X),
    (RawA < 0 -> A is 2 * pi + RawA ; A is RawA).

angle(ast(BaseX, BaseY), ast(TargetX, TargetY), A) :-
    X is TargetX - BaseX,  % x grows left to right
    Y is BaseY - TargetY,  % y grows top to bottom, opposite of normal cartesian coords.
    raw_angle(X, Y, A).

taxi_distance(ast(X1, Y1), ast(X2, Y2), D) :-
    X is X2 - X1,
    Y is Y2 - Y1,
    abs(X, XV),
    abs(Y, YV),
    plus(XV, YV, D).

map_size(Map, MaxX, MaxY) :-
    maplist(x_loc, Map, XS),
    maplist(y_loc, Map, YS),
    max_member(MaxX, XS),
    max_member(MaxY, YS).

visible_from(A, Map, VisibleMap, Count) :-
    select(A, Map, TgtMap),
    map_size(TgtMap, MaxX, MaxY),
    predsort(ast_distance(A), TgtMap, ADistMap),
    remove_hidden(A, MaxX, MaxY, ADistMap, DoneMap, RemMap),
    append(DoneMap, RemMap, VisibleMap),
    length(VisibleMap, Count).

ast_distance(Base, Delta, A1, A2) :-
    angle(Base, A1, Angle1),
    angle(Base, A2, Angle2),
    taxi_distance(Base, A1, D1),
    taxi_distance(Base, A2, D2),
    compare(Cmp1, Angle1, Angle2),
    compare(Cmp2, D1, D2),
    compare_join(Delta, Cmp1, Cmp2).

compare_join((<), (<), _O).
compare_join((>), (>), _O).
compare_join(O, (=), O).

    
stepsize(BX, BY, X, Y, StepX, StepY) :-
    XDistRaw is X - BX,
    YDistRaw is Y - BY,
    DistStep is gcd(XDistRaw, YDistRaw),
    StepX is div(XDistRaw, DistStep),
    StepY is div(YDistRaw, DistStep).
    
remove_hidden(ast(BX,BY), MaxX, MaxY, [ast(X,Y)|Map], [ast(X,Y)|DoneMap], VisibleMap) :-
    stepsize(BX, BY, X, Y, XDist, YDist),
    remove_multiples(BX, BY, XDist, YDist, MaxX, MaxY, 1, Map, RemMap),
    remove_hidden(ast(BX,BY), MaxX, MaxY, RemMap, RHDone, VisibleMap),
    remove_multiples(BX, BY, XDist, YDist, MaxX, MaxY, 1, RHDone, DoneMap).
remove_hidden(_AST, _MaxX, _MaxY, [], [], []).

remove_multiples(BX, BY, XDist, YDist, MaxX, MaxY, Mult, Map, RemMap) :-
    TX is BX + (XDist * Mult),
    TY is BY + (YDist * Mult),
    (between(0, MaxX, TX) ->
         (between(0, MaxY, TY) ->
              (
                  exclude(at_loc(TX, TY), Map, RMap),
                  succ(Mult, NextMult),
                  remove_multiples(BX, BY, XDist, YDist, MaxX, MaxY, NextMult, RMap, RemMap)
              )
         ; RemMap = Map
         )
    ; RemMap = Map
    ).

% ----------------------------------------------------------------------

test1 :- FName = '../../inputs/day10/testinp1',
         read_map(Map, FName),
         visible_from(ast(1,0), Map, _V1, 7),
         visible_from(ast(4,4), Map, _V2, 7),
         visible_from(ast(3,4), Map, VMap, 8),
         member(ast(3,2), VMap),
         ((member(ast(1,0), VMap), !, fail) ; true),
         best_base(FName, ast(3,4), 8).

test2 :- best_base('../../inputs/day10/testinp2', ast(5,8), 33).
test3 :- best_base('../../inputs/day10/testinp3', ast(1,2), 35).
test4 :- best_base('../../inputs/day10/testinp4', ast(6,3), 41).
test5 :- best_base('../../inputs/day10/testinp5', ast(11,13), 210).
test6 :- best_base('../../inputs/day10/testinp6', ast(8,3), 30).

test7 :- vaporized('../../inputs/day10/testinp6', ast(8,1), 1, 801).
test8 :- vaporized('../../inputs/day10/testinp6', ast(9,0), 2, 900).
test9 :- vaporized('../../inputs/day10/testinp6', ast(9,1), 3, 901).
test10 :- vaporized('../../inputs/day10/testinp6', ast(10,0), 4, 1000).
test11 :- vaporized('../../inputs/day10/testinp6', ast(9,2), 5, 902).

test12 :- vaporized('../../inputs/day10/testinp6', ast(11,1), 6, 1101).
test13 :- vaporized('../../inputs/day10/testinp6', ast(12,1), 7, 1201).
test14 :- vaporized('../../inputs/day10/testinp6', ast(11,2), 8, 1102).
test15 :- vaporized('../../inputs/day10/testinp6', ast(15,1), 9, 1501).

test18 :- vaporized('../../inputs/day10/testinp5', ast(11,12), 1, 1112).
    
test :- test1, test2, test3, test4, test5, test6, test7, test8, test9,
        test10, test11, test12, test13, test14, test15, test18.

% ----------------------------------------------------------------------

best_base(FName, Asteroid, MaxCount) :-
    read_map(Map, FName),
    best_base_in(Map, Asteroid, MaxCount).

best_base_in(Map, Asteroid, MaxCount) :-
    findall(R, (member(A, Map), visible_from(A, Map, _VMap, Count), R = result(Count, A)), RS),
    max_member(result(MaxCount, Asteroid), RS).

vaporized(FName, Asteroid, Count, ACode) :-
    read_map(Map, FName),
    best_base_in(Map, Base, _MaxCount), !,
    vaporize_sweep(Map, Base, Asteroid, Count),
    Asteroid = ast(X, Y),
    ACode is X * 100 + Y.

vaporize_sweep(Map, Base, Asteroid, NumVaporized) :-
    visible_from(Base, Map, VisMap, Count),
    (Count < NumVaporized ->
         (
             list_to_set(Map, MapS),
             list_to_set(VisMap, VMapS),
             subtract(MapS, VMapS, RemS),
             list_to_set(Rem, RemS),
             RemVaporized is NumVaporized - Count,
             vaporize_sweep(Rem, Base, Asteroid, RemVaporized)
         )
    ; vaporize_in_this_sweep(VisMap, Base, NumVaporized, Asteroid)
    ).

sweepOrderAngle(Angle, SweepA) :-
    % input Angle is standard radians (0 = pointing ->, increasing
    % values sweep counterclockwise), output SweepA is (0 = pointing
    % up, increasing values sweep clockwise).
    UpdA is (pi/2) - Angle,
    (UpdA < 0 -> SweepA is UpdA + (2 * pi); SweepA is UpdA).

sweepOrder(Base, Delta, A1, A2) :-
    angle(Base, A1, Angle1),
    angle(Base, A2, Angle2),
    sweepOrderAngle(Angle1, S1),
    sweepOrderAngle(Angle2, S2),
    compare(Delta, S1, S2).

vaporize_in_this_sweep(Map, Base, NVap, Asteroid) :-
    predsort(sweepOrder(Base), Map, SMap),
    nth1(NVap, SMap, Asteroid, _).
