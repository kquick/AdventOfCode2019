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
    run_opcode(state(Memory, 0, Inp, [], run), FinalState), !,
    output(FinalState, Out).

continue_program(state(Memory, IP, _, _Out0, pause), Inp, FinalState, Out) :-
    run_opcode(state(Memory, IP, Inp, [], run), FinalState), !,
    output(FinalState, Out).

run_opcode(state(Memory, IP, Inp, Out, run), FinalState) :-
    nth0(IP, Memory, Instruction),
    opcode(Instruction, Opcode, Modes),
    succ(IP, Param0Ptr),
    run_opcode(Opcode, Modes, state(Memory, Param0Ptr, Inp, Out, run), UpdState), !,
    run_opcode(UpdState, FinalState).
run_opcode(state(Memory, IP, Inp, Out, halt), state(Memory, IP, Inp, Out, halt)).
run_opcode(state(Memory, IP, Inp, Out, pause), state(Memory, IP, Inp, Out, pause)).

opcode(Instruction, Opcode, Modes) :- divmod(Instruction, 100, Modes, Opcode).
mode(Modes, Mode, RemModes) :- divmod(Modes, 10, RemModes, Mode).

memory(state(Memory, _, _, _, _), Memory).
output(state(_, _, _, Output, _), Output).

can_continue_state(state(_, _, _, _, pause)).

% ----------------------------------------------------------------------

%% pi(P).  % input parameter
%% po(P).  % output parameter

param(0, PAddr, Memory, pi(P)) :- readmem(PAddr, Memory, Ptr), readmem(Ptr, Memory, P).
param(1, PAddr, Memory, pi(P)) :- readmem(PAddr, Memory, P).

param(0, PAddr, Memory, po(P)) :- readmem(PAddr, Memory, P).
% param(0, PAddr, Memory, po(P)) % not supported


params(Modes, PAddr, Memory, P1, RModes, NextAddr) :-
    mode(Modes, Mode, RModes),
    param(Mode, PAddr, Memory, P1),
    succ(PAddr, NextAddr).
params(Modes, PAddr, Memory, P1, P2, RModes, NextAddr) :-
    params(Modes, PAddr, Memory, P1, Mode1, Next),
    params(Mode1, Next, Memory, P2, RModes, NextAddr).
params(Modes, PAddr, Memory, P1, P2, P3, RModes, NextAddr) :-
    params(Modes, PAddr, Memory, P1, P2, Mode1, Next),
    params(Mode1, Next, Memory, P3, RModes, NextAddr).

writemem(Addr, Val, Memory, UpdMemory) :-
    nth0(Addr, Memory, _, M1),
    nth0(Addr, UpdMemory, Val, M1).

readmem(Addr, Memory, Val) :-
    nth0(Addr, Memory, Val, _).

% ----------------------------------------------------------------------

run_opcode(99, _,
           state(Memory, _, _, Out, _),
           state(Memory, _, _, Out, halt)).  % HALT

run_opcode(1,  Modes,
           state(Memory,   PAddr,  Inp, Out, run),
           state(FinalMem, NextIP, Inp, Out, run)) :-  % ADD
    params(Modes, PAddr, Memory, pi(P1), pi(P2), po(Ptr3), _, NextIP),
    plus(P1, P2, Res),
    writemem(Ptr3, Res, Memory, FinalMem).

run_opcode(2, Modes,
           state(Memory,   PAddr,  Inp, Out, run),
           state(FinalMem, NextIP, Inp, Out, run)) :-  % MUL
    params(Modes, PAddr, Memory, pi(P1), pi(P2), po(Ptr3), _, NextIP),
    Res is P1 * P2,
    writemem(Ptr3, Res, Memory, FinalMem).

run_opcode(3, Modes,
           state(Memory, PAddr,  Inp,    Out, run),
           state(UpdMem, NextIP, RemInp, Out, run)) :- % INP
    params(Modes, PAddr, Memory, po(Ptr), _, NextIP),
    nth0(0, Inp, InpVal, RemInp),
    writemem(Ptr, InpVal, Memory, UpdMem).

run_opcode(4, Modes,
           state(Memory, PAddr,  Inp,    Out, run),
           state(Memory, NextIP, Inp, NewOut, pause)) :- % OUT
    params(Modes, PAddr, Memory, pi(P1), _, NextIP),
    append(Out, [P1], NewOut).

run_opcode(5, Modes,
           state(Memory, PAddr,  Inp, Out, run),
           state(Memory, NextIP, Inp, Out, run)) :- % JNZ
    params(Modes, PAddr, Memory, pi(P1), pi(DestIP), _, StepIP),
    (P1 =:= 0 -> (NextIP is StepIP) ; (NextIP is DestIP)).

run_opcode(6, Modes,
           state(Memory, PAddr,  Inp, Out, run),
           state(Memory, NextIP, Inp, Out, run)) :- % JEQ
    params(Modes, PAddr, Memory, pi(P1), pi(DestIP), _, StepIP),
    (P1 =:= 0 -> NextIP is DestIP ; NextIP is StepIP).

run_opcode(7, Modes,
           state(Memory, PAddr,  Inp, Out, run),
           state(UpdMem, NextIP, Inp, Out, run)) :- % LT
    params(Modes, PAddr, Memory, pi(P1), pi(P2), po(Ptr), _, NextIP),
    ((P1 < P2 ->
          writemem(Ptr, 1, Memory, UpdMem)) ;
     writemem(Ptr, 0, Memory, UpdMem)).

run_opcode(8, Modes,
           state(Memory, PAddr,  Inp, Out, run),
           state(UpdMem, NextIP, Inp, Out, run)) :- % LT
    params(Modes, PAddr, Memory, pi(P1), pi(P2), po(Ptr), _, NextIP),
    (P1 =:= P2 ->
         writemem(Ptr, 1, Memory, UpdMem) ;
     writemem(Ptr, 0, Memory, UpdMem)).


% ----------------------------------------------------------------------

test13() :- thrusters('../../inputs/day7/testinp1', _, 43210).
test14() :- thrusters('../../inputs/day7/testinp2', _, 54321).
test15() :- thrusters('../../inputs/day7/testinp3', _, 65210).
test16() :- feed_thrusters('../../inputs/day7/testinp4', [5,6,7,8,9], 139629729).
test17 :- feed_thrusters('../../inputs/day7/testinp5', [5,6,7,8,9], 18216).
    
test() :- test13, test14, test15, test16, test17.

% ----------------------------------------------------------------------

thrusters(FName, _ValidPhases, Result) :-
    read_intcode(Program, FName),
    findall(Output, amplifiers(Program, Output), Outputs),
    max_member(Result, Outputs).

amplifiers(Program, Output) :-
    permutation([0,1,2,3,4], Phases),
    apply(phased_amps(Program, Output), Phases).

phased_amps(Program, Output, PhaseA, PhaseB, PhaseC, PhaseD, PhaseE) :-
    run_program(Program, [PhaseA, 0],    _, [AmpA|_]),
    run_program(Program, [PhaseB, AmpA], _, [AmpB|_]),
    run_program(Program, [PhaseC, AmpB], _, [AmpC|_]),
    run_program(Program, [PhaseD, AmpC], _, [AmpD|_]),
    run_program(Program, [PhaseE, AmpD], _, [Output|_]).

feed_thrusters(FName, _ValidPhases, Result) :-
    read_intcode(Program, FName),
    findall(Output, feedback_amplifiers(Program, Output), Outputs),
    max_member(Result, Outputs).

feedback_amplifiers(Program, Output) :-
    permutation([5,6,7,8,9], Phases),
    apply(repeated_phased_amps(Program, Output), Phases).

repeated_phased_amps(Program, Output, PhaseA, PhaseB, PhaseC, PhaseD, PhaseE) :-
    % All Amps must run enough to generate at least one output for the thrusters
    run_program(Program, [PhaseA, 0],    StateA, [AmpA|_]),
    run_program(Program, [PhaseB, AmpA], StateB, [AmpB|_]),
    run_program(Program, [PhaseC, AmpB], StateC, [AmpC|_]),
    run_program(Program, [PhaseD, AmpC], StateD, [AmpD|_]),
    run_program(Program, [PhaseE, AmpD], StateE, [AmpE|_]),
    repeat_amps(StateA, StateB, StateC, StateD, StateE, AmpE, Output).

repeat_amps(StateA, StateB, StateC, StateD, StateE, AmpE, Output) :-
    (continue_program(StateA, [AmpE], NewA, [OutA|_]) ->
         (continue_program(StateB, [OutA], NewB, [OutB|_]) ->
              (continue_program(StateC, [OutB], NewC, [OutC|_]) ->
                   (continue_program(StateD, [OutC], NewD, [OutD|_]) ->
                        (continue_program(StateE, [OutD], NewE, [OutE|_]) ->
                             repeat_amps(NewA, NewB, NewC, NewD, NewE, OutE, Output)
                        ; Output = AmpE)
                   ; Output = AmpE)
              ; Output = AmpE)
         ; Output = AmpE)
    ; Output = AmpE).
