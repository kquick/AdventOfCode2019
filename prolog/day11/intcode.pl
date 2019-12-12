read_intcode(Memory, FName) :-
    open(FName, read, FStrm, []),
    read_line_to_string(FStrm, Line),
    string_to_intcode(Line, Memory).

string_to_intcode(Line, Program) :-
    split_string(Line, ",", "", Codes),
    intcodes(Codes, Program).

intcodes([], []).
intcodes([C|CS], [I|IS]) :-
    number_string(I, C),
    intcodes(CS, IS).

% ----------------------------------------------------------------------

run_program(Memory, Inp, FinalState, Out) :-
    run_opcode(state(Memory, 0, 0, Inp, [], run), FinalState), !,
    output(FinalState, Out).

continue_program(state(Memory, RelBase, IP, _, _Out0, pause), Inp, FinalState, Out) :-
    run_opcode(state(Memory, RelBase, IP, Inp, [], run), FinalState), !,
    output(FinalState, Out).

run_program_to_end(Memory, Inp, FinalState, Out) :-
    run_opcode(state(Memory, 0, 0, Inp, [], run), UpdState), !,
    run_program_to_end(UpdState, FinalState, Out).
run_program_to_end(state(Memory, RelBase, IP, Inp, Out0, pause), FinalState, Out) :-
    run_opcode(state(Memory, RelBase, IP, Inp, Out0, run), UpdState), !,
    run_program_to_end(UpdState, FinalState, Out).
run_program_to_end(state(Memory, RelBase, IP, Inp, Out, halt), FinalState, Out) :-
    FinalState = state(Memory, RelBase, IP, Inp, Out, halt).


run_opcode(state(Memory, RelBase, IP, Inp, Out, run), FinalState) :-
    nth0(IP, Memory, Instruction),
    opcode(Instruction, Opcode, Modes),
    succ(IP, Param0Ptr),
    run_opcode(Opcode, Modes, state(Memory, RelBase, Param0Ptr, Inp, Out, run), UpdState), !,
    run_opcode(UpdState, FinalState).
run_opcode(state(Memory, RelBase, IP, Inp, Out, halt), state(Memory, RelBase, IP, Inp, Out, halt)).
run_opcode(state(Memory, RelBase, IP, Inp, Out, pause), state(Memory, RelBase, IP, Inp, Out, pause)).

opcode(Instruction, Opcode, Modes) :- divmod(Instruction, 100, Modes, Opcode).
mode(Modes, Mode, RemModes) :- divmod(Modes, 10, RemModes, Mode).

memory(state(Memory, _, _, _, _, _), Memory).
output(state(_, _, _, _, Output, _), Output).

can_continue_state(state(_, _, _, _, _, pause)).

% ----------------------------------------------------------------------

%% pi(P).  % input parameter
%% po(P).  % output parameter

% param(Mode, ParamAddr, Memory, RelBase, ParamValue)
%   Mode = 0  -- position (use Memory[Memory[PAddr]])
%   Mode = 1  -- immediate (use Memory[PAddr] directly)
%   Mode = 2  -- relative (use Memory[RelativeBase + Memory[PAddr]])
param(0, PAddr, Memory, _RelBase, pi(P)) :- readmem(PAddr, Memory, Ptr), readmem(Ptr, Memory, P).
param(1, PAddr, Memory, _RelBase, pi(P)) :- readmem(PAddr, Memory, P).
param(2, PAddr, Memory, RelBase,  pi(P)) :- readmem(PAddr, Memory, Ptr), plus(Ptr, RelBase, TgtAddr), readmem(TgtAddr, Memory, P).

param(0, PAddr, Memory, _RelBase, po(P)) :- readmem(PAddr, Memory, P).
% param(0, PAddr, Memory, po(P)) % not supported
param(2, PAddr, Memory, RelBase,  po(P)) :- readmem(PAddr, Memory, POff), plus(POff, RelBase, P).


params(Modes, PAddr, Memory, RelBase, P1, RModes, NextAddr) :-
    mode(Modes, Mode, RModes),
    param(Mode, PAddr, Memory, RelBase, P1),
    succ(PAddr, NextAddr).
params(Modes, PAddr, Memory, RelBase, P1, P2, RModes, NextAddr) :-
    params(Modes, PAddr, Memory, RelBase, P1, Mode1, Next),
    params(Mode1, Next, Memory, RelBase, P2, RModes, NextAddr).
params(Modes, PAddr, Memory, RelBase, P1, P2, P3, RModes, NextAddr) :-
    params(Modes, PAddr, Memory, RelBase, P1, P2, Mode1, Next),
    params(Mode1, Next, Memory, RelBase, P3, RModes, NextAddr).

writemem(Addr, Val, Memory, UpdMemory) :-
    length(Memory, Memsize),
    (Addr < Memsize ->
         (nth0(Addr, Memory, _, M1),
          nth0(Addr, UpdMemory, Val, M1))
    ; (FillSize is Addr - Memsize,
       zeromem([], FillSize, Fill),
       append(Memory, Fill, M1),
       append(M1, [Val], UpdMemory))).

zeromem(I, N, I) :- length(I, N).
zeromem(I, N, O) :- length(I, M), M < N, zeromem([0|I], N, O).

readmem(Addr, Memory, Val) :-
    length(Memory, Memsize),
    (Addr < Memsize ->
         nth0(Addr, Memory, Val, _)
    ; Val = 0).

% ----------------------------------------------------------------------

run_opcode(99, _,
           state(Memory, _, _, _, Out, _),
           state(Memory, _, _, _, Out, halt)).  % HALT

run_opcode(1,  Modes,
           state(Memory,   RelBase, PAddr,  Inp, Out, run),
           state(FinalMem, RelBase, NextIP, Inp, Out, run)) :-  % ADD
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(P2), po(Ptr3), _, NextIP),
    plus(P1, P2, Res),
    writemem(Ptr3, Res, Memory, FinalMem).

run_opcode(2, Modes,
           state(Memory,   RelBase, PAddr,  Inp, Out, run),
           state(FinalMem, RelBase, NextIP, Inp, Out, run)) :-  % MUL
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(P2), po(Ptr3), _, NextIP),
    Res is P1 * P2,
    writemem(Ptr3, Res, Memory, FinalMem).

run_opcode(3, Modes,
           state(Memory, RelBase, PAddr,  Inp,    Out, run),
           state(UpdMem, RelBase, NextIP, RemInp, Out, run)) :- % INP
    params(Modes, PAddr, Memory, RelBase, po(Ptr), _, NextIP),
    nth0(0, Inp, InpVal, RemInp),
    writemem(Ptr, InpVal, Memory, UpdMem).

run_opcode(4, Modes,
           state(Memory, RelBase, PAddr,  Inp,    Out, run),
           state(Memory, RelBase, NextIP, Inp, NewOut, pause)) :- % OUT
    params(Modes, PAddr, Memory, RelBase, pi(P1), _, NextIP),
    append(Out, [P1], NewOut).

run_opcode(5, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(Memory, RelBase, NextIP, Inp, Out, run)) :- % JNZ
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(DestIP), _, StepIP),
    (P1 =:= 0 -> (NextIP is StepIP) ; (NextIP is DestIP)).

run_opcode(6, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(Memory, RelBase, NextIP, Inp, Out, run)) :- % JEQ
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(DestIP), _, StepIP),
    (P1 =:= 0 -> NextIP is DestIP ; NextIP is StepIP).

run_opcode(7, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(UpdMem, RelBase, NextIP, Inp, Out, run)) :- % LT
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(P2), po(Ptr), _, NextIP),
    ((P1 < P2 ->
          writemem(Ptr, 1, Memory, UpdMem)) ;
     writemem(Ptr, 0, Memory, UpdMem)).

run_opcode(8, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(UpdMem, RelBase, NextIP, Inp, Out, run)) :- % LT
    params(Modes, PAddr, Memory, RelBase, pi(P1), pi(P2), po(Ptr), _, NextIP),
    (P1 =:= P2 ->
         writemem(Ptr, 1, Memory, UpdMem) ;
     writemem(Ptr, 0, Memory, UpdMem)).

run_opcode(9, Modes,
           state(Memory, RelBase, PAddr,  Inp, Out, run),
           state(Memory, NewRelB, NextIP, Inp, Out, run)) :- % SetRelBase
    params(Modes, PAddr, Memory, RelBase, pi(P1), _, NextIP),
    NewRelB is RelBase + P1.


% ----------------------------------------------------------------------

test().

% ----------------------------------------------------------------------

paint_panel_count(FName, Result) :-
    read_intcode(Program, FName),
    run_program(Program, [0], TurnState, [Color|_]),
    bootstrap_program(TurnState, Color, Panels),
    assoc_to_list(Panels, PanelList),
    length(PanelList, Result).

paint_panels(FName) :-
    read_intcode(Program, FName),
    run_program(Program, [1], TurnState, [Color|_]),
    bootstrap_program(TurnState, Color, Panels),
    display_panels(Panels).

bootstrap_program(TurnState, Color, Panels) :-
    continue_program(TurnState, [], NextState, [Turn|_]),
    empty_assoc(EmptyPanels),
    update_panel(NextState, pos(0,0), up, Color, Turn, EmptyPanels, Panels).

update_panel(PState, Pos, Dir, Color, Turn, InpPanels, OutPanels) :-
    set_panel_color(InpPanels, Pos, Color, UpdPanels),
    move_robot(Pos, Dir, Turn, NewPos, NewDir),
    get_panel_color(UpdPanels, NewPos, PanelColor),
    (continue_program(PState, [PanelColor], TurnState, [NextColor|_]) ->
         (continue_program(TurnState, [], NextState, [NextTurn|_]) ->
              update_panel(NextState, NewPos, NewDir, NextColor, NextTurn, UpdPanels, OutPanels)
         ; OutPanels = UpdPanels)
    ; OutPanels = UpdPanels).

get_panel_color(Panels, Pos, PanelColor) :-
    (get_assoc(Pos, Panels, PanelColor), !) ; PanelColor = 0.

set_panel_color(Panels, Pos, PanelColor, UpdPanels) :-
    put_assoc(Pos, Panels, PanelColor, UpdPanels).

move_robot(pos(X, Y), Dir, 0, NewPos, NewDir) :-
    turn_left(Dir, NewDir),
    move_one(X, Y, NewDir, NewX, NewY),
    NewPos = pos(NewX, NewY).

move_robot(pos(X, Y), Dir, 1, NewPos, NewDir) :-
    turn_right(Dir, NewDir),
    move_one(X, Y, NewDir, NewX, NewY),
    NewPos = pos(NewX, NewY).

turn_left(up, left).
turn_left(left, down).
turn_left(down, right).
turn_left(right, up).

turn_right(up, right).
turn_right(right, down).
turn_right(down, left).
turn_right(left, up).

move_one(X, Y, up, X, NewY) :- plus(1, NewY, Y).
move_one(X, Y, down, X, NewY) :- plus(1, Y, NewY).
move_one(X, Y, left, NewX, Y) :- plus(1, NewX, X).
move_one(X, Y, right, NewX, Y) :- plus(1, X, NewX).

pos_x(pos(X, _Y), X).
pos_y(pos(_X, Y), Y).

pos_ranges(Positions, MinX, MinY, MaxX, MaxY) :-
    maplist(pos_x, Positions, XPositions),
    maplist(pos_y, Positions, YPositions),
    min_member(MinX, XPositions),
    min_member(MinY, YPositions),
    max_member(MaxX, XPositions),
    max_member(MaxY, YPositions).

display_panels(Panels) :-
    assoc_to_keys(Panels, Positions),
    pos_ranges(Positions, MinX, MinY, MaxX, MaxY),
    succ(MaxX, StopX),
    succ(MaxY, StopY),
    display_rows(Panels, MinX, StopX, MinY, StopY).

display_rows(_Panels, _MinX, _MaxX, MaxY, MaxY).
display_rows(Panels, MinX, MaxX, Y, MaxY) :-
    display_row(Panels, MinX, MaxX, Y),
    plus(1, Y, NextY),
    display_rows(Panels, MinX, MaxX, NextY, MaxY).

display_row(_Panels, MaxX, MaxX, _Y) :- format('~n').
display_row(Panels, X, MaxX, Y) :-
    get_panel_color(Panels, pos(X,Y), PanelColor),
    (PanelColor =:= 0 -> format(' '); format('#')),
    plus(1, X, NextX),
    display_row(Panels, NextX, MaxX, Y).
