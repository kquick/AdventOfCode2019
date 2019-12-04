read_lines(Line1, Line2, FName) :-
    open(FName, read, FStrm, []),
    read_line_to_string(FStrm, Inp1),
    read_line_to_string(FStrm, Inp2),
    inp_to_line(Inp1, Line1),
    inp_to_line(Inp2, Line2).

inp_to_line(Inp, Line) :-
    split_string(Inp, ",", "", Codes),
    moves(Codes, Line).

moves([], []).
moves([C|CS], [move(Dir,Len)|LS]) :-
    sub_string(C, 0, 1, RLen, Dir),
    sub_string(C, 1, RLen, 0, LenStr),
    number_string(Len, LenStr),
    moves(CS,LS).

% ----------------------------------------------------------------------

%% main entry point: given two lines (each is a list of move(D,L),
%% starting at pos(0,0)), return the intersections of those lines,
%% where each intersection is intersect(pos(X,Y), len1, len2).
intersect_line(Line1, Line2, Intersections) :-
    intersect_segment(Line1, pos(0, 0), 0, Line2, pos(0, 0), 0, AllIntersections),
    nth0(0, AllIntersections, _, Intersections).

intersect_segment([], _, _, _, _, _, []).
intersect_segment([S|Segs], SPos, SLen, Line2, LPos, LLen, Intersections) :-
    intersect_segment_to(S, SPos, SLen, Line2, LPos, LLen, IntSeg),
    move_to(SPos, S, NewPos),
    add_len(SLen, S, NewLen),
    intersect_segment(Segs, NewPos, NewLen, Line2, LPos, LLen, IntRem),
    append(IntSeg, IntRem, Intersections).

intersect_segment_to(_, _, _, [], _, _, []).
intersect_segment_to(S, SPos, SLen, [T|TS], TPos, TLen, Intersections) :-
    crossings(S, SPos, SLen, T, TPos, TLen, IntT),
    move_to(TPos, T, NewTPos),
    add_len(TLen, T, NewTLen),
    intersect_segment_to(S, SPos, SLen, TS, NewTPos, NewTLen, IntTS),
    append(IntT, IntTS, Intersections).

move_to(pos(X, Y), move("U", Len), pos(X, NewY)) :- plus(Y, Len, NewY).
move_to(pos(X, Y), move("D", Len), pos(X, NewY)) :- plus(NewY, Len, Y).
move_to(pos(X, Y), move("R", Len), pos(NewX, Y)) :- plus(X, Len, NewX).
move_to(pos(X, Y), move("L", Len), pos(NewX, Y)) :- plus(NewX, Len, X).

add_len(OldLen, move(_, MoveLen), NewLen) :- plus(OldLen, MoveLen, NewLen).

crossings(Move1, Start1, Len1, Move2, Start2, Len2, IS) :-
    move_to(Start1, Move1, End1),
    move_to(Start2, Move2, End2),
    crossings_of(Start1, End1, Len1, Start2, End2, Len2, IS).

crossings_of(Start1, End1, Len1, Start2, End2, Len2, IS) :-
    (line_cross(Start1, End1, Len1, Start2, End2, Len2, IS), !) ;
    (line_cross(Start2, End2, Len2, Start1, End1, Len1, IS), !) ;
    (line_coincide(Start1, End1, Len1, Start2, End2, Len2, IS), !) ;
    IS = [].

line_cross(pos(A,B), pos(C,D), ALen, pos(U,V), pos(W,X), ULen, [intersect(pos(U, B), NewALen, NewULen)]) :-
    B =:= D, U =:= W,
    to_intersect(A, C, U, ALen, NewALen),
    to_intersect(V, X, B, ULen, NewULen).

to_intersect(A, C, U, ALen, NewALen) :-
    ((between(A, C, U), plus(A, AMove, U), !) ;
     (between(C, A, U), plus(U, AMove, A) )),
    plus(ALen, AMove, NewALen).

line_coincide(pos(A,B), pos(C,D), ALen, pos(Q,R), pos(S,T), QLen, Points) :-
    B =:= D, R =:= T, B =:= R, !,
    findall(P, (P = intersect(pos(X, B), NewALen, NewQLen),
                to_intersect(A, C, X, ALen, NewALen),
                to_intersect(Q, S, X, QLen, NewQLen)),
            Points).

line_coincide(pos(A,B), pos(C,D), ALen, pos(Q,R), pos(S,T), QLen, Points) :-
    A =:= C, Q =:= S, A =:= Q,
    findall(P, (P = intersect(pos(A, Y), NewALen, NewQLen),
                to_intersect(B, D, Y, ALen, NewALen),
                to_intersect(R, T, Y, QLen, NewQLen)),
            Points).

% ----------------------------------------------------------------------

fmap(_, [], []).
fmap(F, [P|PS], [D|DS]) :- call(F, P, D), fmap(F, PS, DS).

combined_length(intersect(_, Len1, Len2), L) :- plus(Len1, Len2, L).
    
min_op_from_file(FName, Op, Result) :-
    read_lines(Line1, Line2, FName),
    intersect_line(Line1, Line2, Intersections),
    fmap(Op, Intersections, Distances),
    min_member(Result, Distances).
    
shortest_from_file(FName, Closest) :-
    min_op_from_file(FName, combined_length, Closest).
    
% ----------------------------------------------------------------------

taxi_distance(intersect(pos(X, Y), _, _), D) :- abs(X, XV), abs(Y, YV), plus(XV, YV, D).

closest_from_file(FName, Closest) :-
    min_op_from_file(FName, taxi_distance, Closest).

% ----------------------------------------------------------------------

read_expected(Result, FName) :-
    open(FName, read, FStrm, []),
    read_line_to_string(FStrm, Line),
    number_string(Result, Line).

testf(InpEnd, OutEnd, TestFunc) :-
    string_concat("../../inputs/day3/test", InpEnd, InpFName),
    string_concat("../../inputs/day3/test", OutEnd, OutFName),
    read_expected(Result, OutFName),
    call(TestFunc, InpFName, Result).

test1d() :- testf("inp1", "out1", closest_from_file).
test2d() :- testf("inp2", "out2", closest_from_file).
test3d() :- testf("inp3", "out3", closest_from_file).

test1l() :- testf("inp1", "outshort1", shortest_from_file).
test2l() :- testf("inp2", "outshort2", shortest_from_file).
test3l() :- testf("inp3", "outshort3", shortest_from_file).

testd() :- test1d, test2d, test3d.
testl() :- test1l, test2l, test3l.
